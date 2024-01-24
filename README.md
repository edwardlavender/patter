
# [`patter`](https://github.com/edwardlavender/patter) for passive acoustic telemetry

**Particle filters to reconstruct movement paths and patterns of space
use in passive acoustic telemetry**

<!-- badges: start -->

[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/patter)](https://CRAN.R-project.org/package=patter)
[![Codecov test
coverage](https://codecov.io/gh/edwardlavender/patter/branch/main/graph/badge.svg)](https://app.codecov.io/gh/edwardlavender/patter?branch=main)
[![R-CMD-check](https://github.com/edwardlavender/patter/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/edwardlavender/patter/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

[`patter`](https://github.com/edwardlavender/patter) is
[R](https://www.r-project.org) package implementation of a forward
filtering–backward sampling algorithm for passive acoustic telemetry.
This methodology enables the reconstruction of movement paths and
patterns of space use in passive acoustic telemetry systems.
[`patter`](https://github.com/edwardlavender/patter) unifies a suite of
methods formerly known as the
[`flapper`](https://github.com/edwardlavender/flapper) algorithms
(Lavender et al., 2023) and supersedes the experimental
[`flapper`](https://github.com/edwardlavender/flapper) package.

# Features

[`patter`](https://github.com/edwardlavender/patter) is designed to
reconstruct fine-scale movement paths and emergent patterns of space use
in passive acoustic telemetry systems. A powerful, flexible
process-orientated framework known as a forward filtering–backward
sampling algorithm is used for this purpose. This framework unifies the
[`flapper`](https://github.com/edwardlavender/flapper) algorithms and
provides important opportunities for development, which we exploit here.

The essential functions are `pf_forward()` and
`pf_backward_sampler_*()`. `pf_forward()` is the forward filter. This
simulates the possible locations of an individual moving forwards in
time, accounting for all of the data (including acoustic observations,
depth observations and any other observations) *up to* each time point
and the animal’s movement. `pf_backward_sampler_*()` refines outputs
from the forward filter. This function runs a simulation backwards in
time and samples likely locations in line with all of the data *up to
and after* each time point. The outcome is a set of location samples
that represent possible trajectories and embody emergent patterns of
space use.

# Evolution

[`patter`](https://github.com/edwardlavender/patter) evolved from the
experimental [flapper](https://github.com/edwardlavender/flapper)
package, but is:

- **More powerful**, with a substantially revised methodology;
- **Faster**, with overhauled internal routines;
- **Simpler** to use and maintain;
- **Stable**, with fewer dependencies and an upgraded spatial ecosystem;
- **Better tested**, with comprehensive unit tests;

See `NEWS` for a summary of the evolution of
[`flapper`](https://github.com/edwardlavender/flapper) to
[`patter`](https://github.com/edwardlavender/patter).

At the time of writing (December 2023),
[`patter`](https://github.com/edwardlavender/patter) focuses on the
reconstruction movement paths and patterns of space use, which represent
a subset of the capacity provided by
[`flapper`](https://github.com/edwardlavender/flapper). Further
developments will be driven by user requirements. Please get in touch if
you would like to see additional functionality brought into
[`patter`](https://github.com/edwardlavender/patter).

# Installation

This package requires [R](https://www.r-project.org) version ≥ 4.1 (but
the most recent version is strongly recommended). You can check your
version with `R.version.string`. Subsequent installation steps (may)
require the `devtools` and `pkgbuild` packages, which can be installed
with `install.packages(c("devtools", "pkgbuild"))`. On Windows, package
building requires `Rtools`. You can check whether `Rtools` is installed
with `pkgbuild::has_rtools()`. If `Rtools` is not installed, it is
necessary to download and install the appropriate version of `Rtools`
before proceeding by following the instructions
[here](https://cran.r-project.org/bin/windows/Rtools/). Once you are set
up, you can install [`patter`](https://github.com/edwardlavender/patter)
via:

``` r
devtools::install_github("edwardlavender/patter", 
                         dependencies = TRUE, 
                         build_vignettes = TRUE)
```

The `dependencies = TRUE` argument ensures that suggested packages are
also installed, which are required for some functions and to build
vignettes. This process may take several minutes. Set
`build_vignettes = FALSE` for a faster installation.

We strongly recommend using
[`renv`](https://rstudio.github.io/renv/articles/renv.html) (or similar)
and [RStudio Projects](https://r4ds.had.co.nz/workflow-projects.html) to
track the version of
[`patter`](https://github.com/edwardlavender/patter) that you use in
your projects. This will ensure that your code continues to work, even
if we have to make breaking changes to
[`patter`](https://github.com/edwardlavender/patter) as the package
evolves in response to user feedback.

# Vignettes

For an introduction to
[`patter`](https://github.com/edwardlavender/patter), use:

- `vignette("a-methodology", package = "patter")` for a conceptual
  introduction to the methodology;
- `vignette("b-workflow-outline", package = "patter")` for an overview
  of the workflow;
- `vignette("c-workflow-example", package = "patter")` for an example
  workflow;
- `vignette("d-demos", package = "patter")` for more involved
  demonstrations;
- `vignette("e-faqs", package = "patter")` for FAQs;

For a full list of all functions, see `help(package = 'patter')`.

# Datasets

For example datasets from the Movement Ecology of Flapper Skate project
(`datasets-mefs`), which inspired
[`patter`](https://github.com/edwardlavender/patter), see:

- `dat_moorings` for receiver locations and associated information;
- `dat_acoustics` for acoustic time series;
- `dat_archival` for archival (depth) time series;
- `dat_gebco()` for a bathymetry grid;

For example algorithm outputs (`datasets-algorithms`), see:

- `dat_obs()` for an example output dataset from `pf_setup_obs()`;
- `dat_pff()` and `dat_pff_src()` for example outputs from
  `pf_forward()`;
- `dat_pfbk()` and `dat_pfbk_src()` for example outputs from
  `pf_backward_killer()`;
- `dat_pfp()` for an example output from `pf_path()`;
- `dat_coa()` for an example output from `coa()`;

# Simulation

To simulate passive acoustic telemetry data, see:

- `sim_array()` to simulate an acoustic array;
- `sim_path_walk()` to simulate a movement path from a walk model;
- `sim_detections()` to simulate detections at receivers;

These functions are supported by a set of simulation helpers, including:

- `rbern()`, `rdet()`, `dbern()`, `ddetlogistic()` and `ddet()` for the
  simulation of detections;
- `dtruncgamma()`, `rtruncgamma()`, `rlen()` and `clen()` for the
  simulation of step lengths;
- `rwn()`, `rangrw()`, `rangcrw()` and `cang()` for the simulation of
  turning angles;
- `rstep()`, `dstep()` and `cstep()` for the simulation of steps into
  new locations;
- `dkick()` and `rkick()` for `*step()` wrappers;

To evaluate model skill in reconstructing simulated patterns, see
`skill_()` functions, specifically:

- `skill_mb()` to calculate mean bias;
- `skill_me()` to calculate mean error;
- `skill_rmse()` to calculate root mean squared error;
- `skill_R()` to calculate Spearman’s rank correlation coefficient;
- `skill_d()` to calculate the index of agreement;

# Data exploration

For help with data acquisition, processing, checking and preliminary
analyses, see the [`flapper`](https://github.com/edwardlavender/flapper)
package. This facilitates:

- Data preparation;
- Spatial operations;
- Distance calculations;
- Movement analyses;

Please submit a [feature
request](https://github.com/edwardlavender/patter/issues) if you would
like functions from
[`flapper`](https://github.com/edwardlavender/flapper) in
[`patter`](https://github.com/edwardlavender/patter).

# Modelling workflow

The main thrust of [`patter`](https://github.com/edwardlavender/patter)
is the provision of an integrated modelling workflow for reconstructing
animal movement paths and emergent patterns of space use in passive
acoustic telemetry systems.

**To set up data for
[`patter`](https://github.com/edwardlavender/patter)**, use

- `pat_setup_data()` to set up data;

**To implement the centre-of-activity algorithm**, use:

- `coa()` to calculate centres of activity;

**To implement the particle filter (PF)**, use:

- `pf_setup_obs()` to set up a timeline of observations;
- `pf_forward()` to implement the forward simulation;

**PF is supported by**:

- Proposal functions (see `pf_propose`) for the generation of new
  (candidate) locations, including:
  - `pf_rpropose_kick()`, which uses stochastic kicks;
  - `pf_rpropose_reachable()`, which supports directed sampling;
- Likelihood functions (see `pf_lik`) for evaluating the likelihood of
  the data, given proposal locations, including:
  - `acs_filter_land`, which filters proposals on land;
  - `acs_filter_container`, which filters proposals incompatible with
    acoustic container dynamics;
  - `pf_lik_ac`, which calculates the likelihood of acoustic data;
  - `pf_lik_dc`, which calculates the likelihood of depth observations;
- Likelihood helpers, including:
  - `acs_setup_detection_overlaps()`, which pre-calculates detection
    overlaps;
  - `acs_setup_detection_kernel()`, which prepares a detection kernel;
  - `acs_setup_detection_kernels()`, which pre-calculates detection
    kernels;
- (Re)sampling functions (see `pf_sample`) for the (re)sampling of valid
  proposal locations, including:
  - `pf_sample_multinomial()`, which implements multinomial resampling;
  - `pf_sample_systematic()`, which implements systematic resampling;
- Option functions (`pf_opt`) for tuning the forward simulation,
  including:
  - `pf_opt_trial()`, which sets convergence parameters;
  - `pf_opt_rerun_from()`, which sets re-run parameters;
  - `pf_opt_control()`, which sets control parameters;
  - `pf_opt_record()`, which sets output properties;

**To implement the backward pass (`pf_backward_*()`)**, use:

- `pf_backward_killer()` to prune dead-ends;
- `pf_backward_sampler_p()` or \[`pf_backward_sampler_v()`\] to run the
  backward sampler;

**For particle diagnostics**, see:

- `pf_diag_convergence()` to collate convergence diagnostics;
- `pf_diag_summary()` to collate summary diagnostics;

**For convenience plotting functions**, see:

- `pf_plot_history()` to plot particle histories;

**For common utility functions**, see:

- `pf_files()` to list particle-sample files;
- `pf_files_size()` to measure file size;

**For movement-path reconstruction**, use:

- `pf_path()` (and `pf_path_pivot()`) to reconstruct movement paths;

**For mapping utilisation distributions**, use:

- `pf_coord()` to collate particle coordinates for mapping;
- `map_pou()` to map probability-of-use;
- `map_dens()` to create smooth maps using `spatstat`, plus the
  supporting functions:
  - `as.im.SpatRaster()`, to convert `SpatRaster`s to pixel images;
  - `as.owin.SpatRaster()`, to convert `SpatRaster`s to observation
    windows;
  - `as.owin.sf()`, to convert `sf` objects to observation windows;
- `map_hr_*()` to map home ranges, specifically:
  - `map_hr_prop()` for a custom range;
  - `map_hr_core()` for the ‘core’ range;
  - `map_hr_home()` for the ‘home’ range;
  - `map_hr_full()` for the full range;

# Miscellaneous helpers

The following convenience functions are also exported. Use:

- `ssf()` and `ssv()` to `set.seed()`;
- `dist_along_path()` to calculate distances along a movement path;
- `degrees()` to create circular angles;
- `spatTemplate()` to create a template `SpatRaster`;

# Options

For additional options in
[`patter`](https://github.com/edwardlavender/patter), see:

- `patter-progress` to monitor function progress;

# Resources

**For full details on the methods**, see the references below.

**For further information of the
[`patter`](https://github.com/edwardlavender/patter) package**, see:

- `?patter::patter` for an overview of package functions;
- `?patter::pf_forward`for information on specific functions (such as
  `pf_forward()`);

**For further code examples**, see:

- [`patter-eval`](https://github.com/edwardlavender/patter-eval) for an
  extensive simulation-based workflow and analysis;
- [`patter-flapper`](https://github.com/edwardlavender/patter-flapper)
  for a complete real-world analysis;

# Disclaimer and troubleshooting

[`patter`](https://github.com/edwardlavender/patter) is a new
[R](https://www.r-project.org/) package. All routines are experimental.
Researchers interested in using the package are encouraged to get in
touch while the methods and package remain at an early stage of
evolution (<edward.lavender@eawag.ch>).

# Citation

To cite [`patter`](https://github.com/edwardlavender/patter) in
publications, please use:

- Lavender, E. et al. (2023). An integrative modelling framework for
  passive acoustic telemetry. Methods in Ecology and Evolution.
  <https://doi.org/10.1111/2041-210X.14193>
- Lavender, E. et al. (in prep). Particle filters for passive acoustic
  telemetry.
- Lavender, E. et al. (in prep). `patter` for passive acoustic
  telemetry.
- Lavender, E. et al. (in prep). Particle filtering reveals patterns of
  space use in a Critically Endangered elasmobranch.

------------------------------------------------------------------------

Please note that [`patter`](https://github.com/edwardlavender/patter) is
released with a [Contributor Code of
Conduct](https://contributor-covenant.org/version/2/1/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
