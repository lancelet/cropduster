# Optimal Control with Neural Networks

![CI Build]
(https://github.com/lancelet/cropduster/actions/workflows/haskell.yml/badge.svg)

WIP!

This is a repository containing work for a planned presentation to FP-Syd
(the Sydney, Australia, Functional Programming Group) on near-optimal control
using neural networks.

The presentation will eventually cover:
  - Basics of SGD.
  - Backpropagation.
  - Training supervised networks on batches of examples.
  - Training networks for near-optimal control (ie. control of dynamical systems
    using networks that have been trained by minimising an objective function
    typical of an optimal control problem).

Code examples will (hopefully) include:
  - Least-squares linear fitting by SGD. Compared against the closed-form
    solution for a least-squares fit. This introduces SGD with manual gradient
    calculation to cover the basic approach, in order to motivate automatic
    differentiation.
  - Fitting the parameters of a mass-spring-damper system to an observed
    trajectory. This demonstrates backpropagation through a fixed-step RK4
    ODE solver.
  - Training a network to swing-up a pole in a cart-pole pendulum system.
  - Training a network to do a rocket landing in 2D.

## Plan

Major items
- [x] Show SGD linear fitting.
- [x] Show SGD linear fitting in phase space.
- [x] Implement RK4 for backprop.
- [x] Fit parameters to a mass-spring-damper system using backprop.
- [x] Run compiling and plotting in GitHub CI.
- [ ] Tidy spring-damper example; add noise to data; make mass a
      constant since only spring and damping constants are truly free
      parameters.
- [ ] Network for cart-pole pendulum balancing example.
- [ ] Network for 2D rocket landing example.
- [ ] Presentation for FP-Syd.

## Running

To generate linear fitting example movies:

```
$ ./linfit-examples.sh  # generates movies under the plots directory
```

To generate mass-spring-damper example movies:

```
$ ./msd-examples.sh  # generates movies under the plots directory
```