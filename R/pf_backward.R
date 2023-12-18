#' @title PF: run the backward pass
#'
#' @description
#'
#' [`patter`] provides two options for the backward simulation:
#'
#' * [`pf_backward_killer()`]
#' * [`pf_backward_sampler()`]
#'
#' @seealso
#' * The PF (forward simulation) is implemented by [`pf_forward()`].
#'
#' * PF is supported by:
#'     * Setup helpers, namely [`pf_files()`];
#'
#' * The backward pass is implemented by [`pf_backward_*()`];
#'
#' * Movement paths are built from PF outputs via `pf_path()` functions:
#'     * [`pf_path()`] reconstructs paths;
#'     * [`pf_path_pivot()`] supports path reconstruction;
#'
#' * To reconstruct maps of space use, see:
#'     * [`pf_coord()`] to extract particle coordinates;
#'     * [`map_pou()`] for probability-of-use maps;
#'     * [`map_dens()`] for smooth utilisation distributions;
#'     * [`get_hr()`] for home range estimates;
#'
#' @name pf_backward_*

NULL
