---
title: News
---

# `patter` 0.0.0.9000

## Overview 

This is the development version of `patter`. This package is a complete re-write of the predecessor `flapper` package. `patter` is designed to be simpler, faster, more powerful, better tested and easier to maintain. The main thrust of the package is the reconstruction of animal movement paths and emergent patterns of space in passive acoustic telemetry systems within an integrated particle-filtering framework. This consists of a forward simulation and a backward pass. Supporting functions for _de novo_ simulation and other activities are also provided, but current functionality remains more streamlined than `flapper`. We have worked hard to improve internal routines---with function reparameterisions, faster back-ends (especially `terra`, `data.table`, `collapse` and `arrow`) and improved memory handling---alongside the API and supporting documentation. The package also benefits from fewer dependencies and has been fully upgraded in line with the evolution of `R`'s spatial packages (with `terra` and `sf` superseding `raster`, `sp`, `rgeos` and associates). Please provide feedback on the new package. Much work remains!

## _De-novo_ simulation

The `sim_*()` functions in `flapper` have been replaced with faster and more flexible alternatives, including:

* `sim_array()`, which replaces `flapper::sim_array()`;
* `sim_path_walk()`, which replaces `flapper::sim_path_sa()`;
* `sim_detections()`, which replaces `flapper::sim_detections()`;

## Forward simulation 

The `pf_forward()` function implements the forward simulation. This brings together and enhances the `ac()`, `dc()`, `acdc()` and `pf()` functions in `flapper`. A major benefit of this integration is that the likelihood of simulated locations is evaluated exclusively at particle locations, rather than across the entire grid (as in `ac()`, `dc()` and `acdc()`), which is much faster. `pf_forward()` also reparameterises the forward simulation of individual locations, with stochastic kick and directed sampling methodologies. In `flapper`, we simulated new locations by calculating movement probabilities into surrounding grid cells and then sampling grid cells in line with those probabilities (a directed sampling methodology). This approach has some significant benefits but becomes prohibitively expensive as grid size (area and resolution) increases. In `patter`, a new stochastic-kick methodology simulates new locations by 'kicking' previous locations using a movement model. Stochastic kicks are vectorised and independent of grid resolution, so this approach is much faster. At the same time, `pf_forward()` implements an improved directed sampling methodology, and can chop and change between the stochastic-kick and directed sampling methodologies on the fly. This approach exploits the speed gains of stochastic kicks, where possible, while benefiting from the convergence properties of directed sampling, where required. 

## Backward pass 

The `pf_backward_killer()` is used to prune dead-ends from the forward simulation. This replaces previous functionality in `pf_simplify()` and is much faster. Pruning a 20,000 step time series with 1,000 particles at each time step now takes less than two minutes.

`pf_backward_sampler()` is a major, new routine that implements the backward sampler. This is required for the reconstruction of movement trajectories. 

## Analysis 

For the reconstruction of movement paths, `pf_path()` has been completely re-written and is massively faster, thanks to routines from `collapse`. 

For the reconstruction of movement paths, `map_pou()` replaces `flapper::pf_plot_map()` and resolves an issue with the calculation of particle weights. `map_dens()` is a new function for the reconstruction of maps is based on `spatstat` routines. This function accounts for particle weights and fits smooth utilisation distributions using cross validation, superseding approaches in `flapper` based on `adehabitatHR`. Both approaches can be flexibly implemented using a `data.table` of coordinates and are not restricted to particle samples. The `map_hr_*()` routines for the reconstruction of home ranges are re-implementations of the `get_hr_*()` functions in `flapper`. 

For the comparison of simulated and reconstructed patterns, the new `skill_*()` functions can be used. 
