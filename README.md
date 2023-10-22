# Optimal Control with Neural Networks

## Plan

Major items
- [x] Show SGD linear fitting.
- [x] Show SGD linear fitting in phase space.
- [x] Implement RK4 for backprop.
- [x] Fit parameters to a mass-spring-damper system using backprop.
- [ ] Tidy spring-damper example; add noise to data; make mass a
      constant since only spring and damping constants are truly free
      parameters.
- [ ] Run compiling and plotting in GitHub CI.
- [ ] Network for cart-pole pendulum balancing example.
- [ ] Network for 2D rocket landing example.
- [ ] Presentation for FP-Syd.

## Running

To generate linear fitting example movies:

```
$ pushd oc-nn-haskell
$ ./linfit-examples.sh
$ popd
$ ls oc-nn-haskell/plots
```

To generate mass-spring-damper example movies:

```
# TODO
```