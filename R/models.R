#' @title States
#' @description [`State`] is an Abstract Type in [`Patter.jl`](https://edwardlavender.github.io/Patter.jl) that groups state subtypes.
#'
#' @details
#' [`State`] subtypes are `Julia` structures that hold the parameters that describe the state of an individual at a given time. State typically means 'location' (in two or three dimensions), but individual states may include other dimensions for those state dimensions that depend on the state time step. (For example, turning angles may be included in the [`State`] if the turning angle at one time step is dependent upon that in a previous time step.) From an `R`-user perspective, you can think of a [`State`] sub-type as an `S4`-[`class`]-like object, with slots for the state dimensions.
#'
#' In [`patter`], `.state` is a `character` string that defines the animal's state sub-type. This must match a [`State`] sub-type in [`Patter.jl`](https://github.com/edwardlavender/Patter.jl). Currently supported options are:
#' * `"StateXY"`, which maps to `StateXY` in [`Patter.jl`](https://github.com/edwardlavender/Patter.jl).
#' * `"StateXYZD"`, which maps to `StateXYZD` in [`Patter.jl`](https://github.com/edwardlavender/Patter.jl).
#'
#' See [`Patter.jl`](https://edwardlavender.github.io/Patter.jl) or `JuliaCall::julia_help("State")` for the fields of the built-in sub-types.
#'
#' `.state` is used by [`sim_path_walk()`] and [`pf_filter()`], which all effectively simulate states, where it controls the simulation of initial locations and subsequent method dispatch in [`Patter.jl`](https://github.com/edwardlavender/Patter.jl). `[`sim_states_init()`] handles the simulation of initial states in these routines, such that:
#' * `"StateXY"` specifies the simulation of initial `x` and `y` coordinates.
#' * `"StateXYZD"` requires the simulation of `x`, `y` and `z` coordinates and an initial direction.
#' This generates a [`data.table`] of initial state(s) which is coerced to a vector of `State`s in `Julia` for the simulation. In [`Patter.jl`](https://github.com/edwardlavender/Patter.jl), the simulation of subsequent states depends on the input state and the movement model.
#'
#' The state must match the movement model (see `.move`):
#' * For `"StateXY"`, a movement model that simulates step lengths and turning angles and updates state components (that is, `x` and `y` coordinates) is required;
#' * For `"StateXYZD"`, a movement model that simulates step lengths, changes in turning angles and changes in depth and updates `x`, `y`, `z` and direction state components is required.
#'
#' To use custom [`ModelObs`] subtypes, see Examples.
#'
#' @example man/example/example-State.R
#' @author Edward Lavender
#' @name State
#' @aliases StateXY StateXYZD
NULL


#' @title Observation models
#' @description [`ModelObs`] is Abstract Type in [`Patter.jl`](https://edwardlavender.github.io/Patter.jl) that groups observation model subtypes.
#'
#' @details
#' Observation model subtypes are `Julia` structures that hold the parameters of observation models. From an `R`-user perspective, you can think of a [`ModelObs`] sub-type as an `S4`-[`class`]-like object, with slots for the parameters of an observation model. With an observation model structure, we can simulate new observations and evaluate the log-probability of existing observations.
#'
#' The following observation models are built in to [`Patter.jl`](https://edwardlavender.github.io/Patter.jl):
#' * [`ModelObsAcousticLogisTrunc`]
#' * [`ModelObsDepthUniform`]
#' * [`ModelObsDepthNormalTrunc`]
#'
#' See [`Patter.jl`](https://edwardlavender.github.io/Patter.jl) or `JuliaCall::julia_help("ModelObs")` for the fields of the built-in subtypes.
#'
#' In [`patter`], observation models are required:
#' * To simulate new observational datasets, via [`sim_observations()`];
#' * To run the particle filter, via [`pf_filter()`];
#'
#' Observation model subtypes should be specified as a `character` vector alongside a `list` of [`data.table`](s) that contain the parameter values for each model. Internally, observation model subtypes and parameters are instantiated and used to simulate observations or in the particle filter. The simulation of observations is implemented via [`Patter.simulate_obs()`](https://edwardlavender.github.io/Patter.jl). In the particle filter, log-probabilities are evaluated by [`Patter.logpdf_obs()`](https://edwardlavender.github.io/Patter.jl). These are generic functions. Different methods are dispatched according to the input model. For the built-in [`ModelObs`] subtypes, corresponding methods for these routines are also built-in. For custom [`ModelObs`] subtypes, the methods need to be provided.
#'
#' To use custom [`ModelObs`] subtypes, see Examples.
#'
#' @example man/example/example-ModelObs.R
#' @author Edward Lavender
#' @name ModelObs
#' @aliases ModelObsAcousticLogisTrunc ModelObsDepthUniform ModelObsDepthNormalTrunc
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
#'
#' #' `.move` is a `character` that defines a movement model structure implemented in Julia (that is, a `ModelMove` constructor). `.move_*()` functions are convenience wrappers for the construction of movement models. Currently implemented options are:
#' * [`move_xy()`], which specifies a movement model in terms of the distribution of step lengths and turning angles;
#' * [`move_xyzd()`], which specifies a movement model in terms of the distribution of step lengths, changes in turning angles and changes in depth;
#'
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
