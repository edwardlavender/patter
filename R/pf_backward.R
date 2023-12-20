#' @title PF: backward pass
#'
#' @description
#' The forward filtering--backward sampling algorithm implemented by [`patter`] that reconstructs animal movements in passive acoustic telemetry systems comprises a forward filter and a backward pass. The forward filter runs a simulation forwards in time, sampling locations (particles) that are consistent with the data up to and including each time point and a movement model (see [`pf_forward()`]). The backward pass refines outputs from the forward filter. [`patter`] provides two options for the backward pass:
#'
#' * [`pf_backward_killer()`] prunes 'dead-ends' from the time series. This function is simple but fast.
#' * [`pf_backward_sampler()`] implements a backward sampler. This function samples from the joint distribution of particles, given all of the data---both from the past and the future. This is much more expensive.
#'
#' See [`pf_backward_killer_diagnostics()`] to compare particle diagnostics for the two approaches.
#'
#' @return [`pf_backward_*()`] functions return [`pf_particles-class`] objects.
#'
#' @inherit pf_forward seealso
#' @author Edward Lavender
#' @name pf_backward_*

NULL
