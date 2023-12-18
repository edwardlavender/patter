
# [`patter`](https://github.com/edwardlavender/patter) for passive acoustic telemetry

**A re-implementation of the
[`flapper`](https://github.com/edwardlavender/flapper) family of
algorithms for passive acoustic telemetry**

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
[R](https://www.r-project.org) package implementation of the
[`flapper`](https://github.com/edwardlavender/flapper) family of
algorithms for passive acoustic telemetry (Lavender et al., 2023). This
evolved from the experimental
[`flapper`](https://github.com/edwardlavender/flapper) package, but is
simpler, faster and better tested. The aim of the package is to
facilitate the reconstruction of movement paths and patterns of space
use in passive acoustic telemetry systems.

# Key features

Building on its
[predecessor](https://github.com/edwardlavender/flapper),
[`patter`](https://github.com/edwardlavender/patter) has been designed
to be:

- **Simpler** to use and maintain;
- **More streamlined**, with fewer functions and function arguments that
  are better tested;
- **More stable**, with fewer dependencies and an upgraded spatial
  ecosystem;
- **Faster**, with revised internal routines that exploit faster
  packages and substantive, new algorithm parameterisations;
- **Better tested**, with comprehensive unit tests;

At the time of writing (October 2023),
[`patter`](https://github.com/edwardlavender/patter) principally
provides the functions required to implement the
[`flapper`](https://github.com/edwardlavender/flapper) family of
algorithms, which represent a subset of the capacity provided by
[`flapper`](https://github.com/edwardlavender/flapper). Further
developments will be driven by user requirements. Please get in touch if
you would like to see additional functionality brought into
[`patter`](https://github.com/edwardlavender/patter).

# Installation

This package requires `R` version ≥ 4.1. You can check your current
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
vignettes.

# Vignettes

For an introduction to
[`patter`](https://github.com/edwardlavender/patter), use:

- `vignette("workflow", package = "patter")` for an overview of the
  analytical workflow;
- `vignette("faqs", package = "patter")` for FAQs;

For a full list of all functions, see `help(package = 'patter')`.

# Datasets

For example datasets from the Movement Ecology of Flapper Skate project
(`datasets-mefs`), see:

- `dat_moorings` for receiver locations and associated information;
- `dat_acoustics` for acoustic time series;
- `dat_archival` for archival (depth) time series;
- `dat_gebco()` for a bathymetry grid;

For example algorithm outputs `datasets-algorithms`, see:

- `dat_obs()` for an example output dataset from `acs_setup_obs()`;
- `dat_overlaps()` for an example output from
  `acs_setup_detection_overlaps()`;
- `dat_kernels()` for an example output from
  `acs_setup_detection_kernels()`;
- `dat_pff()` for an example output from `pf_forward()`;
- `dat_pfb()` for an example output from `pf_backward_killer()`;
- `dat_pfp()` for an example output from `pf_path()`;

# Simulation

To simulate passive acoustic telemetry data, see:

- `sim_array()` to simulate an acoustic array;
- `sim_path_walk()` to simulate a movement path from a walk model, with
  the help of:
  - `rtruncgamma()` and `rlen()` to simulate step lengths;
  - `rwn()`, `rangrw()` and `rangcrw()` to simulate turning angles;
- `sim_detections()` to simulate detections at receivers, with the help
  of:
  - `calc_detection_pr_logistic()` and `calc_detection_pr()`, which
    represent example detection probability models;

To evaluate model skill in reconstructing simulated patterns, see
`skill_()` functions, specifically:

- `skill_mb()` to calculate mean bias;
- `skill_me()` to calculate mean error;
- `skill_rmse()` to calculate root mean squared error;
- `skill_R()` to calculate Spearman’s rank correlation coefficient;
- `skill_d()` to calculate the index of agreement;

# Data preparation

For help with data acquisition, processing, checking and preliminary
analyses, see the [`flapper`](https://github.com/edwardlavender/flapper)
package. This facilitates:

- Simulation;
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

To implement the centre-of-activity algorithm use:

- `coa()` to calculate centres of activity;

To implement the acoustic-centroid\* (AC\*) algorithm(s) (e.g., AC and
ACDC), use:

- `acs_setup_obs()` to set up observations;
- `acs_setup_detection_overlaps()` to set up detection overlaps;
- `acs_setup_detection_pr()` to define a detection probability model;
- `acs_setup_detection_kernels()` to set up detection kernels;

To implement particle filtering (PF), use:

- `pf_files()` to set up the record;
- `pf_forward()` to implement the forward simulation;
- `pf_backward_*()` to implement the backward pass;
- `pf_coords()` to collate particle coordinates;
- `pf_path()` (and `pf_path_pivot()`) to reconstruct movement paths;
- `map_pou()` to map probability-of-use;
- `map_dens()` to create smooth maps;

For home ranges, use:

- `map_hr_prop()` for a custom range;
- `map_hr_core()` for the ‘core’ range;
- `map_hr_home()` for the ‘home’ range;
- `map_hr_full()` for the full range;

# Miscellaneous helpers

The following convenience functions are also made available to users of
[`patter`](https://github.com/edwardlavender/patter):

- Operational statistics. See:
  - `make_matrix_receivers()` to matricise receiver deployment time
    series;
- Distance calculations. See:
  - `dist_along_path()` to calculate distances along a movement path;
  - `degrees()` to create circular angles;
- `terra` helpers. See:
  - `spatTemplate()` to create a template `SpatRaster`;
  - `spatNormalise()` to normalise a `SpatRaster`;
  - `as.im.SpatRaster` and `as.owin.SpatRaster` for interfaces to
    `spatstat` function(s);
- Parallelisation. See:
  - `cl_lapply()` and associated
    `cl_*() functions for parallelisation routines`;

# Disclaimer and troubleshooting

[`patter`](https://github.com/edwardlavender/patter) is a new
[R](https://www.r-project.org/) package. All routines are experimental.
Researchers interested in using the package are encouraged to get in
touch while the methods and package remain at an early stage of
evolution (<edward.lavender@eawag.ch>).

# Citation

To cite [`patter`](https://edwardlavender.github.io/flapper/) in
publications, please use Lavender, E. et al. (2023). An integrative
modelling framework for passive acoustic telemetry. Methods in Ecology
and Evolution. <https://doi.org/10.1111/2041-210X.14193>

------------------------------------------------------------------------

Please note that [`patter`](https://github.com/edwardlavender/patter) is
released with a [Contributor Code of
Conduct](https://contributor-covenant.org/version/2/1/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
