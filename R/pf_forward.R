#' @title PF: run the forward simulation
#'
#' @description
#'
#' [`patter`] provides two options for the forward simulation of possible locations.
#'
#' * **Two-step implementation.** In the two-step implementation, an AC-branch algorithm (e.g., [`acs()`], [`dc()`]) is used to reconstruct the possible locations of an individual over a grid. The resultant surfaces are passed to [`pf_forward_1()`], which uses a particle filtering algorithm to refine the set of possible locations for the individual through time. This is the original implementation in both [`flapper`](https://github.com/edwardlavender/flapper) and [`patter`]. A benefit of this approach is that the deterministic and stochastic parts of the algorithm are separated, which facilitates diagnosing the causes of any convergence failures (e.g., [`acs()`] failures suggest overly restrictive detection probability or mobility parameters whereas [`pf_forward_1()`] failures suggest incorrect mobility parameters or insufficient sampling). [`pf_forward_1()`] is also fast because pre-calculated weights are used in calculations. However, with high-resolution and/or large grids, this approach may become prohibitively expensive because [`acs()`] and [`dc()`] are implemented over the entire grid, whereas in actuality we only require AC-branch weights at sampled (particle) locations.
#'
#' * **Integrated implementation.** In the integrated implementation, AC-branch and PF-branch algorithms are implemented via [`pf_forward_2()`]. In this approach, AC-branch calculations are only implemented at particle locations, which is much more efficient. **This is generally the recommended approach.**
#'
#'
#' @seealso
#' * [`acs()`] and [`dc()`] implement AC-branch algorithms;
#' * [`pf_forward_1()`] and [`pf_forward_2()`] implement the forward simulation;
#' * [`pf_backward()`] implements the backward pass;
#' * [`pf_path()`] reconstructs movement paths;
#' * [`pf_pou()`] and [`pf_dens()`] generate maps of space use;
#' * [`pf_setup_files()`], [`pf_kick()`] and [`pf_coords()`] are helper functions;
#'
#' @name pf_forward_*

NULL
