% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/patter-progress.R
\name{patter-progress}
\alias{patter-progress}
\alias{julia_progress}
\title{\code{\link{patter}} options: progress}
\usage{
julia_progress(
  enabled = getOption("patter.progress"),
  dt = 0.1,
  showspeed = FALSE
)
}
\arguments{
\item{enabled, dt, showspeed}{Arguments for \code{\link[=julia_progress]{julia_progress()}}, passed to \code{ProgressMeter.Progress} in \code{Julia}:
\itemize{
\item \code{enabled} is a \code{logical} variable that defines whether or not to display a progress bar. By default, \code{enabled} is set from the \code{patter.progress} option.
\item \code{dt} is a \code{double} that defines the duration (s) between progress bar updates (updates are generated at intervals at least \code{dt} seconds apart).
\item \code{showspeed} is a \code{logical} variable that defines whether or not to append a per-iteration average duration, (e.g., \verb{12.34 ms/it}) to the progress bar.
}}
}
\description{
\code{\link{patter}} functions enable progress monitoring via function arguments and global options. At the time of writing, there are three main tools to monitor and enhance function progress:
\itemize{
\item User output messages (via the \code{patter.verbose} option and the \code{.verbose} argument);
\item Progress bars (via \code{\link[pbapply:pboptions]{pbapply::pboptions()}} in \code{R}, \code{ProgressMeter.Progress} in \code{Julia}, \code{\link[=julia_progress]{julia_progress()}}, the \code{patter.progress} option and the \code{.progress} argument);
\item Parallelisation (via \code{.cl_} or \code{.fterra} arguments in \code{R} or \code{JULIA_NUM_THREADS} in \code{Julia});
}

Only selected \code{\link{patter}} functions support these options but this may expand in the future, depending on user feedback.
}
\section{User outputs}{
User output messages are controlled via the \code{.verbose} argument. There is a global option \code{patter.verbose} that can be set to suppress user output messages. See the internal \code{\link{cat_}} documentation for permitted inputs to \code{.verbose}.
}

\section{Progress bars}{
For pure \code{R} functions, progress bars are implemented via the \code{pbapply} package and controlled globally via \code{\link[pbapply:pboptions]{pbapply::pboptions()}}. See the internal \code{\link{pb_}} function documentation for examples.

For \code{R} functions with a \code{Julia} backend, progress bars are implemented via \code{Julia}. Set progress options via \code{\link{julia_progress}}. There is a global option this function inherits (\code{patter.progress}) that can be used to suppress progress bars. This is set to \code{TRUE} during package \code{\link{.onLoad}} unless \code{pbapply::pboptions()$type == "none"}. Note that \code{Julia} progress bars are more limited on Windows than MacOS/Linux. There may also be a speed penalty on Windows. Please report your experience.
}

\section{Parallelisation}{
For pure \code{R} functions, parallelisation is implemented via \code{.cl_} arguments passed to \code{\link[=cl_lapply]{cl_lapply()}}, which wraps \code{\link[pbapply:pbapply]{pbapply::pblapply()}}. See the \code{\link[=cl_lapply]{cl_lapply()}} function documentation for full details.

For \code{R} functions that use \code{\link[terra:terra-package]{terra::terra-package}}, the 'fast-terra' \code{.fterra = TRUE} argument instructs \code{\link[terra:terra-package]{terra::terra-package}} to use parallelisation.

For \code{R} functions with a \code{Julia} backend, parallelisation is implemented via the \code{JULIA_NUM_THREADS} argument to \code{\link[=julia_connect]{julia_connect()}}. This can only be set once per \code{R} session.
}

\author{
Edward Lavender
}
