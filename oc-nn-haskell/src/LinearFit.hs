module LinearFit (
    -- * Types   
    Pt (Pt, pt_x, pt_y),
    -- * Functions
    linfitPts
) where

import Statistics.Distribution (quantile, ContDistr)
import Statistics.Distribution.Normal (normalDistr)
import System.Random (randoms, mkStdGen, StdGen, split)


-- | 2D Point.
data Pt = Pt {
    pt_x :: Float,
    pt_y :: Float
}


-- | Points for the linear fit demo.
--
-- This returns an infinite list of points, so use `take` to limit it to the
-- required number.
linfitPts :: [Pt]
linfitPts =
    let
        -- | Number of points to generate.
        n_points :: Int
        n_points = 30
        
        -- | Parameters of the line.
        slope, intercept :: Float
        slope = 1.5
        intercept = 5.0

        -- | Standard deviation of the y-values.
        y_stdev :: Double
        y_stdev = 2.5

        -- | Range of x-values in the points.
        x_min, x_max :: Float
        x_min = 0.0
        x_max = 10.0

        -- | Random generators.
        gen_base, gen_x, gen_y :: StdGen
        gen_base = mkStdGen 42
        (gen_x, gen_y) = split gen_base

        -- | Function to transform x-values from [0,1] to x_range.
        xform_x :: Float -> Float
        xform_x x = x * (x_max - x_min) + x_min

        -- | x-values
        xs :: [Float]
        xs = map xform_x (randoms gen_x)
        
        -- | Noise for each y-value. 
        quantile_f :: ContDistr d => d -> Float -> Float
        quantile_f dist inp = realToFrac (quantile dist (realToFrac inp))
        y_noise :: [Float]
        y_noise = map (quantile_f (normalDistr 0.0 y_stdev)) (randoms gen_y)

        -- | y-values
        ys :: [Float]
        ys = zipWith (+) (map (\x -> x * slope + intercept) xs) y_noise
    
    in zipWith Pt xs ys