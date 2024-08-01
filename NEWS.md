# News

# v 1.0.1.9000

This is the current development version.

# v1.0.1

* Minor bug fixes

# v1.0.0

## Overview 

This is the first version of `patter`. The main thrust of the package is the provision of particle algorithms for the reconstruction of animal movement paths and emergent patterns of space, with a focus on passive acoustic telemetry systems. The package evolved from the predecessor `flapper` package, but is simpler, faster, more powerful, better tested and easier to maintain. 
While current functionality remains more streamlined than `flapper`, internal routines---which now used a specialised, fast `Julia` backend (`Patter.jl`)---alongside the API and supporting documentation are substantially improved. The package also benefits from fewer dependencies and has been fully upgraded in line with the evolution of `R`'s spatial packages (with `terra` and `sf` superseding `raster`, `sp`, `rgeos` and associates). Please provide feedback on the new package. Much work remains!

## _De-novo_ simulation

The `sim_*()` functions in `flapper` have been replaced with faster and more flexible alternatives, including:

* `sim_path_walk()`, which replaces `flapper::sim_path_sa()`;
* `sim_array()`, which replaces `flapper::sim_array()`;
* `sim_observations()`, which replaces `flapper::sim_detections()`;

## Particle filter

The `pf_filter()` function implements the particle filter. This brings together and enhances the `ac()`, `dc()`, `acdc()` and `pf()` functions in `flapper`. A major benefit of this integration is that the likelihood of simulated locations is evaluated exclusively at particle locations, rather than across the entire grid (as in `ac()`, `dc()` and `acdc()`), which is much faster. `pf_filter()` also reparameterises the forward simulation of individual locations with a stochastic kick methodology. In `flapper`, we simulated new locations by calculating movement probabilities into surrounding grid cells and then sampling grid cells in line with those probabilities (a directed sampling methodology). This approach has some significant benefits but becomes prohibitively expensive as grid size (area and resolution) increases. In `patter`, the stochastic-kick methodology simulates new locations by 'kicking' previous locations using a movement model. Stochastic kicks are independent of grid resolution, so this approach is much faster.

## Backward smoother 

The `pf_smoother_two_filter()` implements particle smoothing. This is a major new routine that substantially refines maps of space use. 

## Mapping 

For mapping, `map_pou()` replaces `flapper::pf_plot_map()` and resolves an issue with the calculation of weights. `map_dens()` is a new function for the reconstruction of maps is based on `spatstat` routines. This function accounts for particle weights and fits smooth utilisation distributions using cross validation, superseding approaches in `flapper` based on `adehabitatHR`. Both approaches can be flexibly implemented using a `data.table` of coordinates and are not restricted to particle samples. The `map_hr_*()` routines for the reconstruction of home ranges are re-implementations of the `get_hr_*()` functions in `flapper`. 

For the comparison of simulated and reconstructed patterns, the new `skill_*()` functions can be used. 
