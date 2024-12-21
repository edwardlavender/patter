#' @title `Julia`: set the map(s)
#' @description Use [`set_map()`] at the start of your workflow to export maps of the study area to `Julia`.
#'
#' @param .x A map, supplied as:
#' * A [`SpatRaster`], supported on Windows and MacOS;
#' * A `character` string specifying the file path to a raster, supported on Windows, MacOS and Linux;
#' @param .as_Raster A `logical` input that defines whether or not to read the map as a `Raster` (see Details).
#' @param .as_GeoArray A `logical` input that defines whether or not to read the map as `GeoArray` (see Details).
#'
#' @details
#' [`set_map()`] exports map(s) of the study area to `Julia`.
#'
#' Maps must be georeferenced rasters. A planar (e.g., Universal Transverse Mercator) projection with coordinates in metres is currently required.
#'
#' Maps are used to (a) simulate initial location(s) for an individual and (b) restrict subsequent, simulated movements to habitable areas (see [`ModelMove`]). `NAs` define inhospitable regions (such as land). Initial location(s) and subsequent movements are restricted to non-`NA` regions.
#'
#' For computational reasons, two maps are defined in `Julia`. The map from which initial locations are sampled is exported as a `Raster` (named `env_init`). The map incorporated into the movement model is exported as a `GeoArray` (named `env`). Usually, both maps are identical. For this reason, under the default options, `.x` is exported as both a `Raster` (`.as_Raster = TRUE`) and a `GeoArray` (`.as_GeoArray = TRUE`).
#'
#' To simulate initial locations from a different map from that used to bound individual movements, export the two maps separately, via:
#' \preformatted{
#' set_map(x1, .as_Raster = TRUE, .as_GeoArray = FALSE) # set initial map
#' set_map(x2, .as_Raster = FALSE, .as_GeoArray = TRUE) # set movement map
#' }
#' @return The function returns `invisible(NULL)`.
#' @example man/examples/example-julia-set_map.R
#' @seealso
#' * [`julia_connect()`] to connect to `Julia`;
#' * [`set_seed()`] to set a seed in `R` and `Julia`;
#' * [`set_map()`] to export map(s) to `Julia`;
#' * [`sim_path_walk()`] to simulate movements on the map;
#' * [`pf_filter()`] to reconstruct movements on the map;
#' * [`set_vmap()`] to set a 'validity map` for particle smoothing;
#' @author Edward Lavender
#' @export

set_map <- function(.x, .as_Raster = TRUE, .as_GeoArray = TRUE) {

  #### Define map names
  # * .as_Raster and .as_GeoArray can be logical or characters
  # * The user should supply logical inputs (simpler)
  # * Internally, we can supply characters, which specifies the name
  if (inherits(.as_Raster, "logical")) {
    .Raster <- "env_init"
  } else {
    .Raster    <- .as_Raster
    .as_Raster <- TRUE
  }
  if (inherits(.as_GeoArray, "logical")) {
    .GeoArray <- "env"
  } else {
    .GeoArray    <- .as_GeoArray
    .as_GeoArray <- TRUE
  }
  stopifnot(any(.as_Raster, .as_GeoArray))

  #### Define path to SpatRaster to read file into Julia
  if (inherits(.x, "character")) {
    if (!file.exists(.x)) {
      abort("File {.x} does not exist.", .envir = environment())
    }
    file <- .x
  } else if (inherits(.x, "SpatRaster")) {
    # TO DO
    # * Warn if on linux
    # Define file
    if (terra::nlyr(.x) != 1L) {
      abort("`.x` should contain a single layer.")
    }
    file <- terra::sources(.x)
    if (file == "") {
      file <- tempfile(fileext = ".tif")
      on.exit(unlink(file), add = TRUE)
      terra::writeRaster(.x, file)
    }
  } else {
    abort("`.x` should be a SpatRaster (Windows, MacOS) or a file path to a raster (Windows, MacOS, Linux).")
  }
  # Normalise file path
  # * This is required for correct parsing of \\ on Windows
  file <- normalizePath(file, winslash = "/", mustWork = TRUE)

  #### Read raster into Julia using Rasters or GeoArrays
  if (.as_Raster) {
    # Read Raster
    julia_command(glue('{.Raster} = Patter.rast(x = "{file}");'))
  }
  if (.as_GeoArray) {
    # Read GeoArray directly as `env` or `vmap`
    # (GeoArray(env_init) does not correctly format the raster)
    julia_command(glue('{.GeoArray} = GeoArrays.read("{file}");'))
  }

  nothing()
}
