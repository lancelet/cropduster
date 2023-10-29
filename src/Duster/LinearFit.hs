{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Duster.LinearFit
  ( -- * Functions
    linearFittingAnimation,
    lossLandscapeAnimation,
    relFileNum,
  )
where

import Control.Concurrent.PooledIO.Independent (run)
import Control.Exception (Exception (displayException))
import Data.Bifunctor (second)
import Data.Colour (opaque, transparent)
import Data.Colour.Names (black, blue, grey, orange, red, white)
import Data.Functor ((<&>))
import Data.List.Split (chunksOf)
import Data.Maybe (fromMaybe)
import Data.VectorSpace
  ( AdditiveGroup (zeroV),
    Scalar,
    VectorSpace,
    (^+^),
    (^/),
  )
import Duster.Learning (Grad (Grad), sgdUpdate)
import Duster.Log (Logger, logString)
import Duster.PRNG (normals, uniforms)
import GHC.Generics (Generic)
import qualified Graphics.Rendering.Chart as C
import Graphics.Rendering.Chart.Backend.Cairo
  ( FileFormat (PNG),
    FileOptions (FileOptions),
    renderableToFile,
  )
import Graphics.Rendering.Chart.Easy (def)
import Lens.Micro ((%~), (.~), (?~))
import Path (Abs, Dir, File, Path, Rel, (</>))
import qualified Path
import System.Random (RandomGen, mkStdGen, split)
import Text.Printf (printf)

-- | 2D Point.
data Pt = Pt
  { pt_x :: Float,
    pt_y :: Float
  }

-- | Convert a `Pt` to an `Example`.
ptToExample :: Pt -> Example Float Float
ptToExample (Pt x y) = Example x y

-- | Parameters of the equation for a straight line.
--
-- 'y = theta_1 * x + theta_0'.
data Line = Line
  { -- | y-intercept of the line (ie. coefficient for input raised to zero).
    theta_0 :: Float,
    -- | slope of the line (ie. coefficient for input raised to first power).
    theta_1 :: Float
  }
  deriving (Show, Generic, AdditiveGroup, VectorSpace)

-- | Given parameters of a linear relationship, apply the equation for the
--   line to an input.
linear :: Line -> Float -> Float
linear (Line c m) x = m * x + c

---- Training -----------------------------------------------------------------

-- | Example for supervised training.
--
-- @i@ is the input type for the example, @o@ is the output type.
data Example i o = Example i o deriving (Eq)

-- | Batch of paired inputs and outputs for supervised training.
type Batch i o = [Example i o]

-- | Loss function with derivative.
--
-- A loss function takes a batch of examples along with the current learnable
-- parameters and returns a tuple containing the value of the loss and the
-- gradient of loss with respect to all the parameters.
type LossFnD i o t = Batch i o -> t -> (Float, Grad t)

-- | Perform a learning step on a batch of examples, updating parameters to
--   their new values and returning the loss and new parameters.
--
-- A learning step combines calculating the loss with an SGD update of the
-- parameters.
learningStep ::
  forall i o t.
  (VectorSpace t) =>
  -- | Learning rate.
  Scalar t ->
  -- | Loss function.
  LossFnD i o t ->
  -- | Current parameter(s).
  t ->
  -- | Batch of examples.
  Batch i o ->
  -- | Loss and updated parameter(s).
  (Float, t)
learningStep r lf t batch = second (sgdUpdate r t) (lf batch t)

-- | Fit a sequence of batches.
--
-- Fitting runs multiple learning steps across all batches.
fit ::
  forall i o t.
  (VectorSpace t) =>
  -- | Learning rate.
  Scalar t ->
  -- | Loss function.
  LossFnD i o t ->
  -- | List of batches to process.
  [Batch i o] ->
  -- | Initial parameter.
  t ->
  -- | List of loss and updated parameters after each batch.
  [(Float, t)]
fit r lf batches t = scanl step (initLoss, t) batches
  where
    initLoss :: Float
    initLoss = fst $ lf (head batches) t

    step :: (Float, t) -> Batch i o -> (Float, t)
    step (_, p) = learningStep r lf p

---- Linear Fit ---------------------------------------------------------------

-- | Loss function for a linear fit.
--
-- The loss function takes a batch of examples, and returns the least-squares
-- difference between predicted values and actual values. It also returns the
-- gradient of this value with respect to each parameter of the line.
linfitLossFn :: LossFnD Float Float Line
linfitLossFn egs line = (mean (egLoss <$> egs), mean (egGradLoss <$> egs))
  where
    egLoss :: Example Float Float -> Float
    egLoss (Example x y) = (linear line x - y) ** 2

    egGradLoss :: Example Float Float -> Grad Line
    egGradLoss (Example x y) = Grad (Line dTheta_0 dTheta_1)
      where
        y' = linear line x
        dTheta_0 = 2 * (y' - y)
        dTheta_1 = 2 * (y' - y) * x

-- | Compute the mean of a list.
mean :: forall v s. (VectorSpace v, s ~ Scalar v, Fractional s) => [v] -> v
mean [] = error "Cannot compute mean of an empty list."
mean xs = go 0 zeroV xs
  where
    go :: (Fractional s, s ~ Scalar v) => Int -> v -> [v] -> v
    go n run_sum [] = run_sum ^/ fromIntegral n
    go n run_sum (y : ys) = go (n + 1) (run_sum ^+^ y) ys

---- Animated sequence generation ---------------------------------------------

-- | Perform a linear fit for animation.
--
-- The output from this function produces a list of tuples containing the
-- batch used for a training step along with
fitForAnimation ::
  -- | Number of epochs to train.
  Int ->
  -- | Batch size.
  Int ->
  -- | Learning rate.
  Float ->
  -- | List of tuples of the batch used, and the loss and parameters it
  --   produced.
  [(Maybe (Batch Float Float), Float, Line)]
fitForAnimation n_epochs batch_size lr =
  let init_line = Line 10 -1.2

      dataset :: [Example Float Float]
      dataset = concat $ replicate n_epochs $ ptToExample <$> defaultLinFitPts

      batches :: [Batch Float Float]
      batches = chunksOf batch_size dataset

      fits :: [(Float, Line)]
      fits = fit lr linfitLossFn batches init_line

      convert :: (m, (l, t)) -> (m, l, t)
      convert (m, (l, t)) = (m, l, t)
   in convert <$> zip (Nothing : (Just <$> batches)) fits

-- | Produce an animation of linear fitting for demonstration purposes.
--
-- This calls matplotlib, outputting individual PNG files showing the
-- progress of linear fitting via SGD.
linearFittingAnimation ::
  -- | Thread-safe logger.
  Logger ->
  -- | Parent directory for output of PNG files. This must exist.
  Path Abs Dir ->
  -- | Maximum number of frames to render.
  --
  -- The actual number of rendered frames may be less than this depending on the
  -- dataset size, batch size and number of epochs.
  Int ->
  -- | Batch size to use.
  Int ->
  -- | Learning rate.
  Float ->
  -- | IO action to create the animation.
  IO ()
linearFittingAnimation logger out_dir max_frames batch_size lr = do
  let n_epochs = 4 :: Int

      lin_fit_examples :: [Example Float Float]
      lin_fit_examples = ptToExample <$> defaultLinFitPts

      batch_and_outcome :: [(Maybe (Batch Float Float), Float, Line)]
      batch_and_outcome = fitForAnimation n_epochs batch_size lr

      llsq :: Line
      llsq = lsqFit defaultLinFitPts

      file_names :: [Path Abs File]
      file_names =
        zip [0 .. (max_frames - 1)] batch_and_outcome
          <&> \(index :: Int, _) -> out_dir </> relFileNum 6 index ".png"

      -- Actions to render each frame.
      actions :: [IO ()]
      actions =
        zip file_names batch_and_outcome
          <&> \(outfile, (maybe_batch, _loss, line)) ->
            renderLinearFittingAnimationFrame
              logger
              lin_fit_examples
              outfile
              maybe_batch
              llsq
              line

  -- Run actions in parallel.
  run actions

-- Render a single frame from the linear fitting animation example.
--
-- This is intended to be run as a concurrent task, so do not dump text to the
-- console except via the 'logger'.
renderLinearFittingAnimationFrame ::
  -- | Thread-safe logger to print what we're doing.
  Logger ->
  -- | The linear fitting examples we're training on.
  [Example Float Float] ->
  -- | Path to render the frame.
  Path Abs File ->
  -- | Batch trained on to produce the current frame fit result (if it exists).
  Maybe (Batch Float Float) ->
  -- | Known best-fit line (linear least squares solution).
  Line ->
  -- | Current line parameters.
  Line ->
  -- | IO action to render the frame.
  IO ()
renderLinearFittingAnimationFrame
  logger
  lin_fit_examples
  out_file
  maybe_batch
  llsq
  line = do
    let batch_examples :: [Example Float Float]
        batch_examples = fromMaybe [] maybe_batch

        bg_examples :: [Example Float Float]
        bg_examples = filter (`notElem` batch_examples) lin_fit_examples

        bg_tuples :: [(Float, Float)]
        bg_tuples = (\(Example !x !y) -> (x, y)) <$> bg_examples

        batch_tuples :: [(Float, Float)]
        batch_tuples = (\(Example !x !y) -> (x, y)) <$> batch_examples

        x_min, x_max, y_min, y_max :: Float
        x_min = -7.5
        x_max = 7.5
        y_min = -5
        y_max = 15

        fit_line_tuples :: [(Float, Float)]
        fit_line_tuples =
          [ (x_min, linear line x_min),
            (x_max, linear line x_max)
          ]

        lsq_line_tuples :: [(Float, Float)]
        lsq_line_tuples =
          [ (x_min, linear llsq x_min),
            (x_max, linear llsq x_max)
          ]

        fit_line :: C.PlotLines Float Float
        fit_line =
          C.plot_lines_values .~ [fit_line_tuples] $
            C.plot_lines_style . C.line_color .~ opaque black $
              C.plot_lines_style . C.line_width .~ 1.5 $
                def

        lsq_line :: C.PlotLines Float Float
        lsq_line =
          C.plot_lines_values .~ [lsq_line_tuples] $
            C.plot_lines_style . C.line_color .~ opaque grey $
              C.plot_lines_style . C.line_dashes .~ [4, 4] $
                C.plot_lines_style . C.line_width .~ 1.5 $
                  def

        bg_scatter :: C.PlotPoints Float Float
        bg_scatter =
          C.plot_points_values .~ bg_tuples $
            C.plot_points_style .~ C.filledCircles 3 (opaque blue) $
              def

        batch_scatter :: C.PlotPoints Float Float
        batch_scatter =
          C.plot_points_values .~ batch_tuples $
            C.plot_points_style .~ C.filledCircles 6 (opaque red) $
              def

        layout :: C.Layout Float Float
        layout =
          C.layout_x_axis . C.laxis_generate
            .~ C.scaledAxis
              def
              (x_min, x_max)
            $ C.layout_y_axis . C.laxis_generate
              .~ C.scaledAxis
                def
                (y_min, y_max)
            $ C.layout_x_axis . C.laxis_title .~ "input"
            $ C.layout_y_axis . C.laxis_title .~ "output"
            $ C.layout_all_font_styles . C.font_size %~ (* 1.5)
            $ C.layout_plots
              .~ [ C.toPlot lsq_line,
                   C.toPlot bg_scatter,
                   C.toPlot batch_scatter,
                   C.toPlot fit_line
                 ]
            $ C.layout_plot_background ?~ C.FillStyleSolid (opaque white)
            $ C.layout_background .~ C.FillStyleSolid transparent
            $ def

        chart :: C.Renderable ()
        chart = C.toRenderable layout

    logString logger $ "Rendering: " <> Path.toFilePath out_file
    let opts = FileOptions (640, 480) PNG
    _ <- renderableToFile opts (Path.toFilePath out_file) chart
    pure ()

lossLandscapeAnimation ::
  Logger ->
  Path Abs Dir ->
  Int ->
  Float ->
  IO ()
lossLandscapeAnimation logger out_dir batch_size lr = do
  let n_epochs :: Int
      n_epochs = 12

      batch_and_outcome :: [(Maybe (Batch Float Float), Float, Line)]
      batch_and_outcome = fitForAnimation n_epochs batch_size lr

      fit_history :: [Line]
      fit_history = (\(_, _, line) -> line) <$> batch_and_outcome

      llsq :: Line
      llsq = lsqFit defaultLinFitPts

      file_names :: [Path Abs File]
      file_names =
        zip [0 ..] batch_and_outcome
          <&> \(index, _) -> out_dir </> relFileNum 6 index ".png"

      actions :: [IO ()]
      actions =
        zip [0 ..] file_names
          <&> \(index, out_file) ->
            renderLossLandscapeAnimationFrame
              logger
              out_file
              llsq
              fit_history
              index

  -- Run rendering actions in parallel.
  run actions

renderLossLandscapeAnimationFrame ::
  -- | Thread-safe logger.
  Logger ->
  -- | Path to render the frame.
  Path Abs File ->
  -- | Known best-fit line (linear least-squares solution).
  Line ->
  -- | Phase space points of progressively-better fits.
  [Line] ->
  -- | Index of the current fit.
  Int ->
  -- | IO action to render the frame.
  IO ()
renderLossLandscapeAnimationFrame
  logger
  out_file
  llsq
  fit_history
  index = do
    let trimmed_fit_history :: [Line]
        trimmed_fit_history = take (index + 1) fit_history

        history_tuples :: [(Float, Float)]
        history_tuples = (\(Line !t0 !t1) -> (t0, t1)) <$> trimmed_fit_history

        t0_min, t0_max, t1_min, t1_max :: Float
        t0_min = 4
        t0_max = 11
        t1_min = -1.5
        t1_max = 2

        lsq_point :: [(Float, Float)]
        lsq_point = [(theta_0 llsq, theta_1 llsq)]

        history_scatter :: C.PlotPoints Float Float
        history_scatter =
          C.plot_points_values .~ history_tuples $
            C.plot_points_style .~ C.filledCircles 3 (opaque orange) $
              def

        history_line :: C.PlotLines Float Float
        history_line =
          C.plot_lines_values .~ [history_tuples] $
            C.plot_lines_style . C.line_color .~ opaque grey $
              C.plot_lines_style . C.line_width .~ 1.2 $
                def

        lsq_scatter :: C.PlotPoints Float Float
        lsq_scatter =
          C.plot_points_values .~ lsq_point $
            C.plot_points_style .~ C.exes 8 2.0 (opaque red) $
              def

        lsq_label :: C.PlotAnnotation Float Float
        lsq_label =
          C.plot_annotation_values
            .~ [(theta_0 llsq, theta_1 llsq + 0.2, "Least-Squares Solution")]
            $ C.plot_annotation_style . C.font_color .~ opaque red
            $ C.plot_annotation_style . C.font_size %~ (* 1.8)
            $ C.plot_annotation_vanchor .~ C.VTA_Bottom
            $ C.plot_annotation_hanchor .~ C.HTA_Left
            $ def

        layout :: C.Layout Float Float
        layout =
          C.layout_x_axis . C.laxis_generate
            .~ C.scaledAxis def (t0_min, t0_max)
            $ C.layout_y_axis . C.laxis_generate
              .~ C.scaledAxis def (t1_min, t1_max)
            $ C.layout_x_axis . C.laxis_title .~ "y-intercept"
            $ C.layout_y_axis . C.laxis_title .~ "slope"
            $ C.layout_all_font_styles . C.font_size %~ (* 1.5)
            $ C.layout_plots
              .~ [ C.toPlot history_line,
                   C.toPlot history_scatter,
                   C.toPlot lsq_scatter,
                   C.toPlot lsq_label
                 ]
            $ C.layout_plot_background ?~ C.FillStyleSolid (opaque white)
            $ C.layout_background .~ C.FillStyleSolid transparent
            $ def

        chart :: C.Renderable ()
        chart = C.toRenderable layout

    logString logger $ "Rendering: " <> Path.toFilePath out_file
    let opts = FileOptions (640, 480) PNG
    _ <- renderableToFile opts (Path.toFilePath out_file) chart
    pure ()

-- | Construct a numbered relative file.
--
-- eg:
-- >>> relFileNum 5 42 ".png"
-- "00042.png"
relFileNum ::
  -- Width of numeric part of the file name.
  Int ->
  -- Number of the file name.
  Int ->
  -- Extension of the filename (including the dot).
  String ->
  -- Relative path to a file.
  Path Rel File
relFileNum width n ext = relFile $ printf ("%0*d" <> ext) width n

-- | Construct a relative file, throwing a runtime error if that is not
--   possible.
relFile :: String -> Path Rel File
relFile name =
  case Path.parseRelFile name of
    Right prf -> prf
    Left ex -> error (displayException ex)

---- Closed-form least-squares linear fitting ---------------------------------

-- | Return a least-squares linear fit obtained from the closed-form least
--   squares equations.
lsqFit :: [Pt] -> Line
lsqFit [] = error "cannot least-squares fit an empty list"
lsqFit [_] = error "cannot least-squares fit a single point"
lsqFit pts =
  let n :: Int
      n = length pts

      nf :: Float
      nf = fromIntegral n

      xs, ys :: [Float]
      xs = fmap pt_x pts
      ys = fmap pt_y pts

      square :: Float -> Float
      square x = x * x

      s_x, s_y, s_xx, s_xy :: Float
      s_x = sum xs
      s_y = sum ys
      s_xx = sum $ square <$> xs
      s_xy = sum $ (\(Pt x y) -> x * y) <$> pts

      theta_1' = (nf * s_xy - s_x * s_y) / (nf * s_xx - s_x * s_x)
      theta_0' = (1 / nf) * s_y - theta_1' * (1 / nf) * s_x
   in Line theta_0' theta_1'

---- Generation of example points for linear fitting --------------------------

-- | A default (finite) set of points used for the linear fitting example.
defaultLinFitPts :: [Pt]
defaultLinFitPts =
  let seed = 42
      gen = mkStdGen seed
      line = Line 5.0 1.5
      n_points = 30
      x_range = (-5.0, 5.0)
      y_stdev = 0.75
   in take n_points (linFitPts gen line x_range y_stdev)

-- | Generate points with pseudo-random noise for a linear fit.
--
-- x values are distributed uniformly. y values follow the provided linear
-- relationship, with additional Normally-distributed noise.
linFitPts ::
  forall g.
  (RandomGen g) =>
  -- | Random generator.
  g ->
  -- | Underlying linear relationship for the generated points.
  Line ->
  -- | '(min, max)' range for the uniformly-distributed x values.
  (Float, Float) ->
  -- | Standard Deviation of the noise added to y values.
  Float ->
  -- | Infinite list of generated points.
  [Pt]
linFitPts gen line x_range stdev =
  let -- Configure random number generators.
      gen_x, gen_y :: g
      (gen_x, gen_y) = split gen

      -- Generate values.
      xs = uniforms gen_x x_range
      ys = zipWith (+) (map (linear line) xs) (normals gen_y (0.0, stdev))
   in zipWith Pt xs ys
