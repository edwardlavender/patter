% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/models.R
\name{plot.ModelObs}
\alias{plot.ModelObs}
\alias{plot.ModelObsAcousticLogisTrunc}
\alias{plot.ModelObsDepthUniformSeabed}
\alias{plot.ModelObsDepthNormalTruncSeabed}
\alias{plot.ModelObsContainer}
\title{Observation model plots}
\usage{
\method{plot}{ModelObsAcousticLogisTrunc}(x, .sensor_id, .par = list(), ...)

\method{plot}{ModelObsDepthUniformSeabed}(x, .seabed = 100, .par = list(), ...)

\method{plot}{ModelObsDepthNormalTruncSeabed}(x, .seabed = 100, .par = list(), ...)

\method{plot}{ModelObsContainer}(x, .radius, .par = list(), ...)
}
\arguments{
\item{x}{A named \code{list} of observation model parameters, including a \code{\link{ModelObs}} \code{\linkS4class{S3}} label (from a \verb{model_obs_*()} function).}

\item{.sensor_id, .radius, .seabed}{Model-specific parameters:
\itemize{
\item \code{.sensor_id}: For \code{\link[=plot.ModelObsAcousticLogisTrunc]{plot.ModelObsAcousticLogisTrunc()}}, \code{.sensor_id} controls the sensors (receivers) for which detection probability curves are shown:
\itemize{
\item \code{missing} (default) plots all unique curves;
\item An \code{integer} vector of sensor IDs plots curves for selected sensors;
\item \code{NULL} plots curves for all sensors;
}
\item \code{.radius}: For \code{\link[=plot.ModelObsContainer]{plot.ModelObsContainer()}}, \code{.radius} controls the radii for which distributions are shown:
\itemize{
\item \code{missing} (default) plots distributions for first three unique radii;
\item A vector of radii plots curves for selected radii;
\item \code{NULL} plots distributions for all radii;
}
\item \code{.seabed}: For \code{plot.ModelObsDepth*Seabed()}, \code{.seabed} is the seabed depth at which distributions are plotted.
}}

\item{.par}{Graphical parameters:
\itemize{
\item \code{NULL} uses current graphical parameters;
\item \code{list()} uses default graphical parameters;
\item A named \code{list} of arguments, passed to \code{\link[=par]{par()}}, customises parameters;
}}

\item{...}{Additional arguments, passed to \code{\link[=plot]{plot()}}.}
}
\value{
The functions produce a \code{\link{plot}}. \code{invisible(NULL)} is returned.
}
\description{
\code{\link[=plot]{plot()}} methods for observation models (see \code{\link{ModelObs}}).
}
\details{
Observation model (\code{\link{ModelObs}}) structures are objects that define the parameters of an observation model. The model specifies the probability of an observation (e.g., a particular depth record) given the data (e.g., a depth measurement).
\itemize{
\item \code{\link[=plot.ModelObsAcousticLogisTrunc]{plot.ModelObsAcousticLogisTrunc()}} plots detection probability as a function of distance from a receiver;
\item \code{\link[=plot.ModelObsDepthUniformSeabed]{plot.ModelObsDepthUniformSeabed()}} plots a uniform distribution for the probability of a depth observation around a particular \code{.seabed} depth;
\item \code{\link[=plot.ModelObsDepthNormalTruncSeabed]{plot.ModelObsDepthNormalTruncSeabed()}} plot a truncated normal distribution for the probability of a depth observation around a particularly \code{.seabed} depth;
\item \code{\link[=plot.ModelObsContainer]{plot.ModelObsContainer()}} plots a uniform distribution for the probability of a \emph{future} observation (e.g., detection) given the maximum possible distance (container radius) from the container centroid (e.g., receiver), and a maximum movement speed, at the current time;
}
}
\examples{
if (patter_run(.julia = TRUE, .geospatial = FALSE)) {

  library(data.table)

  julia_connect()

  #### Example (1): ModelObsAcousticLogisTrunc
  # Plot unique detection-probability function(s)
  dat_moorings |>
    model_obs_acoustic_logis_trunc() |>
    plot()
  # Plot functions for selected sensors
  dat_moorings |>
    model_obs_acoustic_logis_trunc() |>
    plot(.sensor_id = unique(dat_moorings$receiver_id)[1:10])

  #### Example (2): ModelObsDepthUniformSeabed
  data.table(sensor_id = 1L, depth_shallow_eps = 10, depth_deep_eps = 10) |>
    model_obs_depth_uniform_seabed() |>
    plot()
  data.table(sensor_id = 1L, depth_shallow_eps = 10, depth_deep_eps = 10) |>
    model_obs_depth_uniform_seabed() |>
    plot(.seabed = 50)

  #### Example (3): ModelObsDepthSeabedNormalTrunc
  data.table(sensor_id = 1L, depth_sigma = 10, depth_deep_eps = 10) |>
    model_obs_depth_normal_trunc_seabed() |>
    plot()
  data.table(sensor_id = 1L, depth_sigma = 100, depth_deep_eps = 50) |>
    model_obs_depth_normal_trunc_seabed() |>
    plot(.seabed = 150)

  #### Example (4): ModelObsContainer
  # Define detections (1)
  detections <- dat_detections[individual_id == individual_id[1], ]
  # Assemble acoustic observations (0, 1) for a given timeline
  timeline   <- assemble_timeline(list(detections), .step = "2 mins")[1:100]
  acoustics  <- assemble_acoustics(.timeline   = timeline,
                                   .detections = detections,
                                   .moorings   = dat_moorings)
  # Assemble acoustic containers
  containers <- assemble_acoustics_containers(.timeline = timeline,
                                              .acoustics = acoustics,
                                              .mobility = 750)
  # Plot a few example distributions
  containers$forward |>
    model_obs_container() |>
    plot()

  #### Example (5): Customise plot layout via `.par`
  dat_moorings |>
    model_obs_acoustic_logis_trunc() |>
    plot(.sensor_id = unique(dat_moorings$receiver_id)[1:4],
         .par = list(mfrow = c(2, 2)))

  #### Example (6): Customise plot properties via `...`
  dat_moorings |>
    model_obs_acoustic_logis_trunc() |>
    plot(.sensor_id = unique(dat_moorings$receiver_id)[1:4],
         .par = list(mfrow = c(2, 2),
                     oma = c(3, 3, 3, 3),
                     mar = c(1.5, 1.5, 1.5, 1.5)),
         xlab = "", ylab = "", col = "red")
  mtext(side = 1, "Distance (m)", line = -1, outer = TRUE)
  mtext(side = 2, "Probability", line = -1, outer = TRUE)

}
}
\author{
Edward Lavender
}
