#' @title PF outputs: [`pf-class`] objects
#' @description An S3 class that defines the named `list` returned by a PF-branch algorithm.
#'
#' @author Edward Lavender
#' @docType package
#'
#' @seealso
#' * The PF (forward simulation) is implemented by [`pf_forward_*()`]:
#'     * [`pf_forward_1()`] refines AC-branch algorithm ([`acs()`] and [`dc()`]) outputs using PF;
#'     * [`pf_forward_2()`] is an integrated implementation that couples AC- and PF-branch algorithms internally;
#'
#' * PF is supported by:
#'     * Setup helpers, namely [`pf_setup_files()`];
#'     * Template movement models, namely [`pf_kick()`];
#'
#' * The backward pass is implemented by [`pf_backward()`];
#'
#' * Movement paths are built from PF outputs via `pf_path()` functions:
#'     * [`pf_path()`] reconstructs paths;
#'     * [`pf_path_pivot()`] supports path reconstruction;
#'
#' * To reconstruct maps of space use, see:
#'     * [`pf_coords()`] to extract particle coordinates;
#'     * [`pf_map_pou()`] for probability-of-use maps;
#'     * [`pf_map_dens()`] for smooth utilisation distributions;
#'     * [`get_hr()`] for home range estimates;
#'
#' @name pf-class
NULL
