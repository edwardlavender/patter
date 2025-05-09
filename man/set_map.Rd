% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/julia-set_map.R
\name{set_map}
\alias{set_map}
\title{\code{Julia}: set the map(s)}
\usage{
set_map(.x, .as_Raster = TRUE, .as_GeoArray = TRUE)
}
\arguments{
\item{.x}{A map, supplied as:
\itemize{
\item A \code{\link[terra:SpatRaster-class]{terra::SpatRaster}}, supported on Windows and MacOS;
\item A \code{character} string specifying the file path to a raster, supported on Windows, MacOS and Linux;
}}

\item{.as_Raster}{A \code{logical} input that defines whether or not to read the map as a \code{Raster} (see Details).}

\item{.as_GeoArray}{A \code{logical} input that defines whether or not to read the map as \code{GeoArray} (see Details).}
}
\value{
The function returns \code{invisible(NULL)}.
}
\description{
Use \code{\link[=set_map]{set_map()}} at the start of your workflow to export maps of the study area to \code{Julia}.
}
\details{
\code{\link[=set_map]{set_map()}} exports map(s) of the study area to \code{Julia}.

Maps must be georeferenced rasters. A planar (e.g., Universal Transverse Mercator) projection with coordinates in metres is currently required.

Maps are used to (a) simulate initial location(s) for an individual and (b) restrict subsequent, simulated movements to habitable areas (see \code{\link{ModelMove}}). \code{NAs} define inhospitable regions (such as land). Initial location(s) and subsequent movements are restricted to non-\code{NA} regions.

For computational reasons, two maps are defined in \code{Julia}. The map from which initial locations are sampled is exported as a \code{Raster} (named \code{env_init}). The map incorporated into the movement model is exported as a \code{GeoArray} (named \code{env}). Usually, both maps are identical. For this reason, under the default options, \code{.x} is exported as both a \code{Raster} (\code{.as_Raster = TRUE}) and a \code{GeoArray} (\code{.as_GeoArray = TRUE}).

To simulate initial locations from a different map from that used to bound individual movements, export the two maps separately, via:
\preformatted{
set_map(x1, .as_Raster = TRUE, .as_GeoArray = FALSE) # set initial map
set_map(x2, .as_Raster = FALSE, .as_GeoArray = TRUE) # set movement map
}
}
\examples{
if (patter_run()) {

  julia_connect()
  set_seed()

  mapfile <- system.file("extdata", "dat_gebco.tif",
                         package = "patter", mustWork = TRUE)
  map     <- terra::rast(mapfile)

  #### Example (1): Set maps using a SpatRaster
  set_map(map)

  #### Example (2): Set maps using a file path to a raster
  # Use this option on Linux
  set_map(mapfile)

  #### Example (3): Distinguish between initial and movement maps
  # Set 'initial' map from which initial locations are sampled
  map_init     <- terra::setValues(map, NA)
  cell         <- 25595
  map_init[cell] <- 1
  set_map(map_init, .as_Raster = TRUE, .as_GeoArray = FALSE)
  # Set map for the movement model
  set_map(map, .as_Raster = FALSE, .as_GeoArray = TRUE)
  # Simulate a movement path
  path <- sim_path_walk(.map = map,
                        .timeline = seq(as.POSIXct("2016-01-01", tz = "UTC"),
                                        length.out = 1000L, by = "2 mins"))
  # The simulated path starts from the set location in `map_init`:
  stopifnot(all(
    path$x[1] == terra::xFromCell(map, cell),
    path$y[1] == terra::yFromCell(map, cell),
    path$map_value[1] == terra::extract(map_init, cell)$map_value)
  )

}

}
\seealso{
\itemize{
\item \code{\link[=julia_connect]{julia_connect()}} to connect to \code{Julia};
\item \code{\link[=set_seed]{set_seed()}} to set a seed in \code{R} and \code{Julia};
\item \code{\link[=set_map]{set_map()}} to export map(s) to \code{Julia};
\item \code{\link[=sim_path_walk]{sim_path_walk()}} to simulate movements on the map;
\item \code{\link[=pf_filter]{pf_filter()}} to reconstruct movements on the map;
\item \code{\link[=set_vmap]{set_vmap()}} to set a 'validity map` for particle smoothing;
}
}
\author{
Edward Lavender
}
