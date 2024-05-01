#' @title [`patter`] options: progress
#' @description [`patter`] functions enable progress monitoring via function arguments and global options. At the time of writing, the main tools to monitor and enhance function progress are as follows:
#' * User output messages (via the `patter.verbose` option and the `.verbose` argument);
#'
#' Only selected [`patter`] functions support these options but this may expand in the future, depending on user feedback.
#'
#' @details
#'
#' # User outputs
#'
#' User output messages are controlled via the `.verbose` argument. There is a global option `patter.verbose` that can be set to suppress user output messages. See the internal [`cat_`] documentation for permitted inputs to `.verbose`.
#'
#'
#' @author Edward Lavender
#' @name patter-progress
NULL
