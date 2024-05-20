#' @title States
#' @description TO DO
#' @author Edward Lavender
#' @name State
NULL


#' @title Observation models
#' @description [`ModelObs`] is Abstract Type in [`Patter.jl`](https://edwardlavender.github.io/Patter.jl) that groups observation model subtypes.
#'
#' @details In the particle filter, observation model(s) are used to weight particles (sampled locations or states) according to the likelihood of each observation.
#'
#' The following observation models are built in to Patter.jl:
#' * `ModelObsAcousticLogisTrunc`
#' * `ModelObsDepthUniform`
#' * `ModelObsDepthNormalTrunc`
#'
#' From an `R`-user perspective, you can think of a [`ModelObs`] subtype as an `S4`-[`class`]-like object, with slots for the parameters of the observation model. For example, the default acoustic observation model is a logistic function of distance that depends on two parameters (`receiver_alpha` and `receiver_beta`) that control the rate of decline in detection probability with Euclidean distance from a receiver and a third parameter that truncates detection probability at zero beyond the receiver's detection range (that is, `receiver_gamma`). See `JuliaCall::julia_help("ModelObs")` or [`Patter.jl`](https://edwardlavender.github.io/Patter.jl) for the arguments of the built-in subtypes.
#'
#' In [`patter`], observation models are required by [`sim_observations()`] (to simulate new observational datasets) and [`pf_filter()`] (which runs the particle filter). In both cases, the observation model subtypes are specified as a `character` vector alongside a `list` of [`data.table`](s) that contain the parameter values for each model. Internally, observation model subtypes and parameters are instantiated and used to simulate observations (in [`sim_observations()`]) or evaluate the likelihood of observations (in [`pf_filter()`] and via [`Patter.logpdf_obs()`](https://edwardlavender.github.io/Patter.jl).
#'
#' For instructions for custom `ModelObs` subtypes, see Examples.
#'
#' @example
#' @author Edward Lavender
#' @name ModelObs
#' @aliases ModelObsAcousticLogisTrunc
NULL


#' @title Movement models
#' @description These functions formulate movement model constructors for export to `Julia`. Function arguments should be specified as `character` strings of Julia code that specify the distributions for components of the movement model.
#' @param length,angle,angle_delta,z_delta Character strings that define movement model components:
#' * `length`---the distribution of step lengths;
#' * `angle`---the distribution of turning angles;
#' * `angle_delta`---the distribution of changes in turning angles;
#' * `z_depth`---the distribution of changes in depth
#'
#' @details
#' `move_*()` functions are used to formulate the movement model for use in simulations and algorithms:
#' * `move_xy()` specifies a two-dimensional (x, y) movement model parametrised in terms of step lengths and turning angles;
#' * `move_xyzd()` specifies a four-dimensional movement model (x, y, z and d) parametrised in terms of step lengths, changes in depth and changes in turning angle;
#'
#' `move_*()` functions map onto `MoveModel` structure in [`Patter.jl`](https://github.com/edwardlavender/Patter.jl).
#'
#' @author Edward Lavender
#' @name ModelMove

#' @rdname ModelMove
#' @export

move_xy <- function(length = "truncated(Gamma(1, 250.0), upper = 750.0)",
                    angle = "Uniform(-pi, pi)") {
  julia_check_exists("env")
  glue('ModelMoveXY(env, {length}, {angle});')
}

move_xyz <- function() {

}

#' @rdname ModelMove
#' @export

move_xyzd <- function(length = "truncated(Gamma(1.0, 750.0), upper = 750.0)",
                      angle_delta = "Normal(0, 0.5)",
                      z_delta = "Normal(0, 3.5)") {
  julia_check_exists("env")
  glue('ModelMoveXYZD(env, {length}, {angle_delta}, {z_delta});')
}
