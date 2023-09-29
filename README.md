
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
simpler, faster and better tested.

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
- **Faster**, with revised internal routines that exploit the
  [`data.table`](https://github.com/Rdatatable/data.table) and
  [`terra`](https://github.com/rspatial/terra) packages and substantive,
  new algorithm parameterisations;
- **Better tested**, with comprehensive unit tests;

At the time of writing (September
2023),[`patter`](https://github.com/edwardlavender/patter) solely
provides the functions required to implement the
[`flapper`](https://github.com/edwardlavender/flapper) family of
algorithms, which represent a subset of the capacity provided by
[`flapper`](https://github.com/edwardlavender/flapper). Further
developments will be driven by user requirements. Please get in touch if
you would like to see additional functionality brought into
[`patter`](https://github.com/edwardlavender/patter).

# Installation

This package requires `R` version ≥ 4.0. You can check your current
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
vignettes. To access the introductory vignette, use
`vignette("patter_introduction", package = "patter")`.

# Datasets

For example datasets, see:

- `dat_moorings` for receiver locations and associated information;
- `dat_acoustics` for acoustic time series;
- `dat_archival` for archival (depth) time series;
- `dat_gebco()` for a bathymetry grid;

# Workflow

To implement the depth-contour algorithm, use:

- `dc_setup_model()` for an example DC model;
- `dc()` to implement the algorithm;

To implement the AC\* algorithms, use:

- `acs_setup_obs()` to set up observations;
- `acs_setup_detection_containers()` to set up detection containers;
- `acs_setup_detection_overlaps()` to set up detection overlaps;
- `acs_setup_detection_kernels()` to set up detection kernels;
- `acs()` to implement the algorithm(s);

To implement particle filtering, use:

- `pf_setup_record()` to set up the record;
- `pf_kick()` for an example movement model;
- `pf_forward()` to implement the forward simulation;
- `pf_backward()` to implement the backward path’
- `pf_pou()` to map probability-of-use;
- `pf_path()` (and `pf_path_pivot()`) to reconstruct movement paths;

For home ranges, use:

- `get_hr_prop()` for a custom range;
- `get_hr_core()` for the ‘core’ range;
- `get_hr_home()` for the ‘home’ range;
- `get_hr_full()` for the full range;

For supporting functions, see `help(package = 'patter')`:

- `make_matrix_receivers()` matricises receiver deployment time series;
- `normalise()` normalises a `SpatRaster`;
- `cl_*()` functions for parallelisation helpers;

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
