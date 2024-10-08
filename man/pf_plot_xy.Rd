% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pf_plot.R
\name{pf_plot_xy}
\alias{pf_plot_xy}
\title{PF: plot particle (\code{x}, \code{y}) locations}
\usage{
pf_plot_xy(
  .map,
  .coord,
  .steps = NULL,
  .png = NULL,
  .add_surface = list(),
  .add_points = list(),
  .add_layer = NULL,
  .prompt = FALSE,
  ...
)
}
\arguments{
\item{.map}{A \code{\link{SpatRaster}} that defines the study area (see \code{\link{glossary}}).}

\item{.coord}{A \code{\link{data.table}} of particle coordinates, including \code{x} and \code{y} columns. Point graphical parameters (\code{pch}, \code{col}, \code{bg}, \code{cex}, \code{lwd}, \code{lty}, \code{lwd}) can be included as columns to customise particle appearance. (Graphical parameters provided here silently overwrite any elements of the same name in \code{.add_points}.)}

\item{.steps}{\code{NULL} or an \code{integer} vector of the time steps for which to map particle samples (e.g., \code{.steps = 1:5L}). \code{NULL} specifies all time steps.}

\item{.png}{(optional) A named \code{list}, passed to \code{\link[grDevices:png]{grDevices::png()}}, to save plots to file. \code{filename} should be the directory in which to write files. Files are named \verb{\{.steps[1]\}.png, \{.steps[2]\}.png, ..., \{.steps[N]\}.png}. \code{.png} should be supplied if \code{.cl} is supplied via \code{...}.}

\item{.add_surface, .add_points}{Named \code{list}s for plot customisation.
\itemize{
\item \code{.add_surface} is passed to \code{\link[terra:plot]{terra::plot()}}, excluding \code{x} and \code{main}.
\item \code{.add_points} is passed to \code{\link[graphics:points]{graphics::points()}}, excluding \code{x} and \code{y}.
}}

\item{.add_layer}{(optional) A \code{function} used to add additional layer(s) to the plot. The function must a single (unnamed) \code{integer} value for the time step (even if ignored). An example function is \code{function(...) points(x, y)} where \code{x} and \code{y} are (for example) receiver coordinates.}

\item{.prompt}{A \code{logical} variable that defines whether or not to prompt the user for input between plots. This is only used in interactive mode if \code{.png = NULL} (and there are multiple time steps).}

\item{...}{Additional argument(s) passed to \code{\link[=cl_lapply]{cl_lapply()}}, such as \code{.cl}.}
}
\value{
The function is called for its side effects. It returns \code{invisible(TRUE)}.
}
\description{
This function maps particle (\code{x}, \code{y}) locations for selected time steps or entire time series and can be used to create animations.
}
\details{
For each \code{.step}, \code{\link[terra:plot]{terra::plot()}} is used to plot \code{.map}. Particle samples in \code{.coord} are added onto the grid via \code{\link[graphics:points]{graphics::points()}}. Particle weights are uniform thanks to resampling.

This function replaces \href{https://edwardlavender.github.io/flapper/reference/pf_plot_history.html}{\code{flapper::pf_plot_history()}}.
}
\examples{
#### Set up
# Define map
map <- dat_gebco()
# Define particle samples
pff <- dat_pff()$states
pfb <- dat_pfb()$states
tff <- dat_tff()$states
# Define directories
con    <- file.path(tempdir(), "patter")
frames <- file.path(con, "frame")
mp4s   <- file.path(con, "mp4")
dir.create(frames, recursive = TRUE)
dir.create(mp4s, recursive = TRUE)
# Cluster options
pbo <- pbapply::pboptions(nout = 4L)

#### Example (1): Plot selected samples
# Plot particles from the forward filter
pf_plot_xy(.map = map,
           .coord = pff,
           .steps = 1L)
# Plot particles from the backward filter
pf_plot_xy(.map = map,
           .coord = pfb,
           .steps = 1L)
# Plot smoothed particles
pf_plot_xy(.map = map,
           .coord = tff,
           .steps = 1L)

#### Example (2): Plot multiple time steps
# Specify selected steps
pf_plot_xy(.map = map,
           .coord = tff,
           .steps = 1:5L)
# Plot all steps (default: .step = NULL)
pf_plot_xy(.map = map,
           .coord = pff)
# Use `.prompt = TRUE`
pf_plot_xy(.map = map,
           .coord = tff,
           .steps = 1:5L,
           .prompt = TRUE)

#### Example (3): Customise the background map via `.add_surface`
pf_plot_xy(.map = map,
           .coord = tff,
           .steps = 1L,
           .add_surface = list(col = rev(terrain.colors(256L))))

#### Example (4): Customise the points
# Use `.add_points`
pf_plot_xy(.map = map,
           .coord = tff,
           .steps = 1L,
           .add_points = list(pch = ".", col = "red"))
# Include `points()` arguments as columns in `.coord`
# * Here, we colour branches from the filter pruned by the smoother in red
pff[, cell_id := terra::extract(map, cbind(x, y))]
pfb[, cell_id := terra::extract(map, cbind(x, y))]
tff[, cell_id := terra::extract(map, cbind(x, y))]
pff[, col := ifelse(cell_id \%in\% tff$cell_id, "black", "red")]
pfb[, col := ifelse(cell_id \%in\% tff$cell_id, "black", "red")]
pf_plot_xy(.map = map,
           .coord = rbind(pff, pfb),
           .steps = 1L,
           .add_points = list(pch = "."))

#### Example (5): Add additional map layers
pf_plot_xy(.map = map,
           .coord = tff,
           .steps = 1L,
           .add_layer = function(t) mtext(side = 4, "Depth (m)", line = -4))

#### Example (6): Write images to file
# Write images in serial
pf_plot_xy(.map = map,
           .coord = tff,
           .steps = 1:50L,
           .png = list(filename = frames))
# Use a fork cluster
if (.Platform$OS.type == "unix") {
  pf_plot_xy(.map = map,
             .coord = tff,
             .steps = 1:50L,
             .png = list(filename = frames),
             .cl = 2L,
             .chunk = TRUE)
}

#### Example (7): Make animations
if (rlang::is_installed("av")) {

  # There are lots of tools to create animations:
  # * `av::av_encode_video()`       # uses ffmpeg
  # * `animation::saveVideo()`      # uses ffmpeg
  # * `magick::image_write_video()` # wraps av()
  # * `glatos::make_video()`        # wraps av()

  # Helper function to open (mp4) files
  Sys.open <- function(.file) {
    if (.Platform$OS.type == "Windows") {
      cmd <- paste("start", shQuote(.file))
    } else {
      cmd <- paste("open", shQuote(.file))
    }
    system(cmd)
  }

  # Use av::av_encode_video()
  # * This is one of the faster options
  input   <- file_list(frames)
  output  <- file.path(mp4s, "ani.mp4")
  av::av_encode_video(input, output, framerate = 10)
  # Sys.open(output)

}

file_cleanup(con)
}
\seealso{
Particle filters and smoothers sample states (particles) that represent the possible locations of an individual through time, accounting for all data and the individual's movement.
\itemize{
\item To simulate artificial datasets, see \verb{sim_*()} functions (especially \code{\link[=sim_path_walk]{sim_path_walk()}}, \code{\link[=sim_array]{sim_array()}} and \code{\link[=sim_observations]{sim_observations()}}).
\item To assemble real-world datasets for the filter, see \code{\link{assemble}}\verb{_*()} functions.
\item \code{\link[=pf_filter]{pf_filter()}} runs the filter:
\itemize{
\item For state types, see \code{\link{State}};
\item For observation models, see \code{\link{ModelObs}};
\item For movement models, see \code{\link{ModelMove}};
}
\item To run particle smoothing, use \code{\link[=pf_smoother_two_filter]{pf_smoother_two_filter()}}.
\item To map emergent patterns of space use, use a \verb{map_*()} function (such as \code{\link[=map_pou]{map_pou()}}, \code{\link[=map_dens]{map_dens()}} and \code{\link[=map_hr]{map_hr()}}).
}
}
\author{
Edward Lavender
}
