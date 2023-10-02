from typing import Tuple

import matplotlib.pyplot as plt
import numpy as np
from numpy.typing import NDArray


def generate_data(
    slope: float,
    intercept: float,
    stdev: float,
    min_x: float,
    max_x: float,
    n_points: int,
    seed: int = 42
) -> Tuple[NDArray[np.float64], NDArray[np.float64]]:
    """
    Generate data for the linear fit example.

    This is data from a straight line, with Gaussian noise. x-axis entries
    come from a uniform random distribution. y-axis entries come from the
    x-axis entries substituted into the equation for the line, and with y-axis
    Gaussian noise.

    :param slope: Slope of the line.
    :param intercept: y-axis intercept.
    :param stdev: Standard deviation of Gaussian / Normal noise.
    :param min_x: Minimum x value.
    :param max_x: Maximum x value.
    :param n_points: Number of points to generate.
    :param seed: Random seed.
    :return: `(xs, ys)` - two numpy arrays containing x and y coordinates
    """

    np.random.seed(seed)

    # Generate x values using a uniform distribution.
    xs = np.random.uniform(low=min_x, high=max_x, size=n_points)
    # Generate y values based on the slope plus random noise.
    ys = xs * slope + intercept + stdev * np.random.randn(n_points)

    return (xs, ys)


def tabulate_lsq_loss(
    xs: NDArray[np.float64],
    ys: NDArray[np.float64],
    min_slope: float,
    max_slope: float,
    min_intercept: float,
    max_intercept: float,
    n_slope_pts: int,
    n_intercept_pts: int
) -> Tuple[NDArray[np.float64], NDArray[np.float64], NDArray[np.float64]]:
    """
    Tabulate least-squares loss from the linear fit.

    :param xs: x values
    :param ys: y values
    :param min_slope: minimum value along the slope axis
    :param max_slope: maximum value along the slope axis
    :param min_intercept: minimum value along the intercept axis
    :param max_intercept: maximum value along the intercept axis
    :param n_slope_pts: number of points to use along the slope axis
    :param n_intercept_pts: number of points to use along the intercept axis
    :return: `(slopes, intercepts, losses)` - three arrays definining
      the slopes, intercepts and losses
    """

    assert (len(xs) == len(ys))

    slope_vals = np.linspace(min_slope, max_slope, n_slope_pts)
    intercept_vals = np.linspace(min_intercept, max_intercept, n_intercept_pts)

    def loss_fn(slope, intercept):
        loss = 0.0
        for x, y in zip(xs, ys):
            y_pred = slope * x + intercept
            err = y - y_pred
            loss += err * err
        return loss

    loss_vals = np.empty((len(intercept_vals), len(slope_vals)))

    for i, intercept in enumerate(intercept_vals):
        for j, slope in enumerate(slope_vals):
            loss_vals[i, j] = loss_fn(slope, intercept)

    return (slope_vals, intercept_vals, loss_vals)


if __name__ == '__main__':

    (xs, ys) = generate_data(
        slope=1.5,
        intercept=2.0,
        stdev=0.5,
        min_x=0.0,
        max_x=3.0,
        n_points=30,
        seed=42
    )

    plt.scatter(xs, ys)
    plt.show()

    (slopes, intercepts, losses) = tabulate_lsq_loss(
        xs=xs,
        ys=ys,
        min_slope=0.0,
        max_slope=3.2,
        min_intercept=0,
        max_intercept=4,
        n_slope_pts=100,
        n_intercept_pts=100
    )
    plt.imshow(losses,
               extent=[slopes.min(),
                       slopes.max(),
                       intercepts.min(),
                       intercepts.max()],
               origin='lower',
               aspect='auto',
               cmap='viridis')
    plt.contour(slopes,
                intercepts,
                losses,
                colors='white',
                levels=40,
                linewidths=1.0,
                alpha=0.4)
    plt.xlabel('slope')
    plt.ylabel('intercept')
    plt.show()
