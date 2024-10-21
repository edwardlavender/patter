# `patter` v2.0.0

## Major changes

`patter` v 2.0.0 includes some major internal changes required to required to permit use on Linux. There are small changes in the API of some functions as a result. We have also added new data-assembly routines, most notably `assemble_acoustics_containers()`, which support particle filtering, alongside additional improvements to select functions. Where required for existing code, you can continue to use `patter` v1.0.1 with [`renv`](https://rstudio.github.io/renv/articles/renv.html). 

* **Julia setup** 
    * `julia_connect()` has been revised and enhanced. The `.threads` argument has been replaced with `JULIA_NUM_THREADS`. 

* **Map export** 
    * `set_map()` has been revamped to support applications on Linux. The function accepts a file path to a raster and includes `.as_Raster` and `.as_GeoArray` arguments. 

* **Data assembly** 
    * A distinction is made between detections and acoustic observations, which include detections and non-detections:
        * `dat_acoustics` has been renamed to `dat_detections`;
        * `assemble_acoustics(.timeline, .acoustics, ...)` has been reformulated to `assemble_acoustics(.timeline, .detections, ...)`;
    * New data assembly routines have been added:
        * `assemble_acoustics_containers()` assembles acoustic containers, which facilitate convergence of the particle filter with fewer particles;
        * `assemble_custom()` assembles custom datasets;

* **Movement initialisation**
    * Initial particle samples, formerly generated in `R` via `simulate_states_init()` and associated (internal) routines, have been moved to `Patter.jl`. This change affects `sim_path_walk()` and `pf_filter()`. The `.map` argument only accepted for plotting in `sim_path_walk()` and no longer accepted in `pf_filter()`. See the revised examples for automated sampling of initial states from the map. 
    *  A bug in `map_init.ModelObsAcousticLogisTrunc` that overly restricted the region from which initial samples were drawn has been fixed in the new `Julia` routines. 

* **Movement simulation**
    * `sim_path_walk()` now fails with an error for invalid maps/movement models.  

* **Observation simulation**
    * `sim_observations()` now expects `ModelObs` structures and parameters in a single named list (passed to `.model_obs`). The `.model_obs_pars` argument has been dropped. 

* **Particle filter**
    * `pf_filter()` now expects `ModelObs` structures and observations in a single named list (passed to `.yobs`). The `.model_obs` argument has been dropped. 
    *  A new `.n_trial` argument permits multiple runs of the filter. 

* **Particle smoothing**
    * `pf_smoother_two_filter()` uses a more flexible `.vmap` argument, supported by `set_vmap()`, in place of `.box` and `.mobility`. 

## Minor improvements and bug fixes

* Minor documentation improvements, including to `Julia` installation instructions;
* Installation via `JuliaLang` or `juliaup` is now recommended;
* Minor bug fixes;

# `patter` v1.0.1

* This version includes minor documentation, bug fixes and tweaks, most notably:
    * `Patter.logpdf_step()` now includes a `t` argument that can be used from `patter`
    * `map_dens()` includes an `.fterra` argument that permits parallelisation of `terra::resample()` 

# `patter` v1.0.0

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
