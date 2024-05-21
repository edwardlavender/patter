#' @title [`patter`] for passive acoustic telemetry
#' @description [`patter`] is a `R` implementation of particle filtering, smoothing and sampling algorithms for animal movement modelling in passive acoustic telemetry systems. This methodology enables the reconstruction of movement paths and patterns of space use [`patter`] unifies a suite of methods formerly known as the [`flapper`](https://github.com/edwardlavender/flapper) algorithms (Lavender et al., 2023) and supersedes the experimental [`flapper`](https://github.com/edwardlavender/flapper) package.
#'
#' @references Lavender, E. et al. (2023). An integrative modelling framework for passive acoustic telemetry. Methods in Ecology and Evolution. \url{https://doi.org/10.1111/2041-210X.14193}.
#'
#' @name patter
"_PACKAGE"

#' @importFrom collapse join
#' @importFrom collapse fndistinct
#' @importFrom collapse fnrow
#' @importFrom collapse seq_row
#'
#' @importFrom data.table copy
#' @importFrom data.table data.table
#' @importFrom data.table as.data.table
#' @importFrom data.table .N
#' @importFrom data.table rbindlist
#' @importFrom data.table rleid
#' @importFrom data.table :=
#'
#' @importFrom dplyr any_of
#' @importFrom dplyr arrange
#' @importFrom dplyr everything
#' @importFrom dplyr filter
#' @importFrom dplyr group_by
#' @importFrom dplyr if_else
#' @importFrom dplyr mutate
#' @importFrom dplyr n
#' @importFrom dplyr reframe
#' @importFrom dplyr rename
#' @importFrom dplyr row_number
#' @importFrom dplyr select
#' @importFrom dplyr slice
#' @importFrom dplyr summarise
#' @importFrom dplyr ungroup
#'
#' @importFrom dtplyr lazy_dt
#'
#' @importFrom glue glue
#'
#' @importFrom graphics arrows
#' @importFrom graphics par
#' @importFrom graphics points
#'
#' @importFrom grDevices hcl.colors
#'
#' @importFrom JuliaCall julia_assign
#' @importFrom JuliaCall julia_command
#' @importFrom JuliaCall julia_eval
#' @importFrom JuliaCall julia_exists
#' @importFrom JuliaCall julia_installed_package
#' @importFrom JuliaCall julia_install_package
#' @importFrom JuliaCall julia_update_package
#' @importFrom JuliaCall julia_library
#' @importFrom JuliaCall julia_setup
#' @importFrom JuliaCall julia_source
#'
#' @importFrom lubridate interval
#' @importFrom lubridate int_overlaps
#' @importFrom lubridate int_start int_end
#' @importFrom lubridate round_date
#' @importFrom lubridate %within%
#'
#' @importFrom rlang .data
#'
#' @importFrom stats runif
#'
#' @importFrom utils head
#' @importFrom utils tail
#' @importFrom utils str

NULL
