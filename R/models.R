#' @title States
#' @description [`State`] is an Abstract Type in [`Patter.jl`](https://edwardlavender.github.io/Patter.jl) that groups state sub-types.
#'
#' @details
#' [`State`] sub-types are `Julia` structures that hold the parameters that describe the state of an individual at a given time step. 'State' typically means 'location' (in two or three dimensions), but individual states may include additional fields for those state dimensions that depend on the state time step. (For example, turning angles may be included in the [`State`] if the turning angle at one time step is dependent upon that at the previous time step.) From an `R`-user perspective, you can think of a [`State`] sub-type as an `S4`-[`class`]-like object, with slots for the state dimensions.
#'
#' In [`patter`] functions, the `.state` argument is a `character` string that defines the animal's state sub-type. This must match a [`State`] sub-type defined in `Julia`. The built-in options are:
#' * `"StateXY"`, which maps to `StateXY` in [`Patter.jl`](https://github.com/edwardlavender/Patter.jl);
#' * `"StateXYZD"`, which maps to `StateXYZD` in [`Patter.jl`](https://github.com/edwardlavender/Patter.jl);
#'
#' See [`State`](https://edwardlavender.github.io/Patter.jl) or `JuliaCall::julia_help("State")` for the fields of the built-in sub-types.
#'
#' `.state` is used by [`sim_path_walk()`] and [`pf_filter()`], both of which effectively simulate time series of states. `.state` controls the simulation of initial locations and subsequent method dispatch in [`Patter.jl`](https://github.com/edwardlavender/Patter.jl). [`sim_states_init()`] handles the simulation of initial states in these routines. When `.state` is`
#' * `"StateXY"`, the initial state comprises `x` and `y` coordinates;.
#' * `"StateXYZD"`, the initial states comprises `x`, `y` and `z` coordinates and an initial direction;
#'
#' All states additionally include a `map_value` field that defines the value on the movement map at (`x`, `y`).
#'
#' The outcome of [`sim_states_init()`] is a [`data.table`] of initial state(s), which is coerced to a `Vector` of `State`s in `Julia` for the simulation. In [`Patter.jl`](https://github.com/edwardlavender/Patter.jl), the simulation of subsequent states depends on the input state and the movement model.
#'
#' The state must match the movement model (see [`ModelMove`]):
#' * For `"StateXY"`, a movement model that simulates step lengths and turning angles and updates state components (that is, `x` and `y` coordinates) is required;
#' * For `"StateXYZD"`, a movement model that simulates step lengths, changes in turning angles and changes in depth and updates `x`, `y`, `z` and direction state components is required.
#'
#' To use custom [`ModelObs`] sub-types, see Examples.
#'
#' @example man/examples/example-State.R
#' @seealso
#' The routines in [`patter`] for the simulation of individual movements, observations and statistical modelling are built upon three Abstract Types defined in `Julia`:
#' * See [`State`] for individual-state (location) structures;
#' * See [`ModelMove`] for movement model structures;
#' * See [`ModelObs`] for observation model structures;
#' @author Edward Lavender
#' @name State
#' @aliases .state StateXY StateXYZD
NULL


#' @title Observation models
#' @description [`ModelObs`] is Abstract Type in [`Patter.jl`](https://edwardlavender.github.io/Patter.jl) that groups observation model sub-types.
#'
#' @details
#' Observation model sub-types are `Julia` structures that hold the parameters of observation models. From an `R`-user perspective, you can think of a [`ModelObs`] sub-type as an `S4`-[`class`]-like object, with slots for the parameters of an observation model. With an observation model structure, we can simulate new observations and evaluate the log-probability of existing observations.
#'
#' The following observation models are built in to [`Patter.jl`](https://edwardlavender.github.io/Patter.jl):
#' * [`ModelObsAcousticLogisTrunc`]
#' * [`ModelObsDepthUniform`]
#' * [`ModelObsDepthNormalTrunc`]
#'
#' See [`Patter.jl`](https://edwardlavender.github.io/Patter.jl) or `JuliaCall::julia_help("ModelObs")` for the fields of the built-in sub-types.
#'
#' In [`patter`], observation models are required:
#' * To simulate new observational datasets, via [`sim_observations()`];
#' * To run the particle filter, via [`pf_filter()`];
#'
#' Observation model sub-types should be specified as a `character` vector alongside a `list` of [`data.table`](s) that contain the parameter values for each model. Internally, observation model sub-types and parameters are instantiated and used to simulate observations or in the particle filter. The simulation of observations is implemented via [`Patter.simulate_obs()`](https://edwardlavender.github.io/Patter.jl). In the particle filter, log-probabilities are evaluated by [`Patter.logpdf_obs()`](https://edwardlavender.github.io/Patter.jl). These are generic functions. Different methods are dispatched according to the input model. For the built-in [`ModelObs`] sub-types, corresponding methods for these routines are also built-in. For custom [`ModelObs`] sub-types, the methods need to be provided.
#'
#' To use custom [`ModelObs`] sub-types, see Examples.
#'
#' @example man/examples/example-ModelObs.R
#' @inherit State seealso
#' @author Edward Lavender
#' @name ModelObs
#' @aliases .model_obs ModelObsAcousticLogisTrunc ModelObsDepthUniform ModelObsDepthNormalTrunc
NULL


#' @title Movement models
#' @description [`ModelMove`] is Abstract Type in [`Patter.jl`](https://edwardlavender.github.io/Patter.jl) that groups movement model sub-types, of which instances can be created via an `R` `move_*()` function.
#'
#' @param dbn_length,dbn_angle,dbn_angle_delta,dbn_z_delta `Character` strings that define movement model components:
#' * `dbn_length`---the distribution of step lengths;
#' * `dbn_angle`---the distribution of turning angles;
#' * `dbn_angle_delta`---the distribution of changes in turning angles;
#' * `dbn_z_delta`---the distribution of changes in depth;
#'
#' @details Movement model sub-types are `Julia` structures that hold the components of movement models. From an `R`-user perspective, you can think of a [`ModelMove`] sub-type as an `S4`-[`class`]-like object, with slots for the components of a movement model. With a movement model instance, we can simulate movements and evaluate the density of movements from one state (location) to another.
#'
#' The following movement models are built in to [`Patter.jl`](https://edwardlavender.github.io/Patter.jl):
#' * [`ModelMoveXY`]
#' * [`ModelMoveXYZD`]
#'
#' See [`Patter.jl`](https://edwardlavender.github.io/Patter.jl) or `JuliaCall::julia_help("ModelMove")` for the fields of the built-in sub-types. Briefly, all sub-types include:
#' * A `map` field, that defines the region(s) within which movements are permitted. In `R`, it is convenient to represent `map` as a [`SpatRaster`], where `NAs` define inhospitable habitats (e.g., land). This should made available to `Julia` [`ModelMove`] constructors as `env` via [`set_map()`];
#' * Additional model-specific components (such as fields for the distribution of step lengths and turning angles in the case of two-dimensional random walks);
#'
#' In [`patter`], movement models are required:
#' * To simulate movement paths, via [`sim_path_walk()`];
#' * To run the particle filter, via [`pf_filter()`];
#' * To run the particle smoother, via [`pf_smoother_two_filter()`];
#'
#' In `R` functions, the movement-model instance is specified via the `.model_move` argument. This argument expects a `character` string defining a [`ModelMove`] instance that can be evaluated in `Julia` (that is, a [`ModelMove`] constructor). `move_*()` functions are convenience functions for the specification of these constructors for the built-in sub-types. All [`ModelMove`] instances contain a `map` field that defines the region(s) within which movements are permitted. To use a `move_*()` function, the map should be available in `Julia` as `env` (see [`set_map()`]). The additional components of the movement model are specified via `move_*()` function arguments as `character` strings of `Julia` code. Currently implemented `move_*()` functions are:
#' * [`move_xy()`], which specifies a movement model of sub-type [`ModelMoveXY`] in terms of the distributions of step lengths and turning angles;
#' * [`move_xyzd()`], which specifies a movement model of sub-type [`ModelMoveXYZD`] in terms of the distributions of step lengths, changes in turning angles and changes in depth;
#'
#' See [here](https://discourse.julialang.org/t/a-comparison-of-common-distributions-in-julia-python-and-r/61604) for the translations of distributions in `R` (e.g., `*norm()`) into `Julia` (e.g., `Normal()`).
#'
#' In `Julia`, [`ModelMove`] instances are used to simulate states via [`Patter.simulate_step()`](https://edwardlavender.github.io/Patter.jl). In the particle smoother, the density of movement from one state to another is evaluated by [`Patter.logpdf_step()`](https://edwardlavender.github.io/Patter.jl). These are generic functions. Different methods are dispatched according to the input model. For the built-in [`ModelMove`] sub-types, corresponding methods for these routines are also built-in. For custom [`ModelMove`] sub-types, the methods need to be provided.
#'
#' To use custom [`ModelMove`] sub-types, see Examples.
#'
#' @returns `move_*()` functions return a `character` string that defines a [`ModelMove`] instance for evaluation in `Julia`. If the map (`env`) does not exist in `Julia`, an error is thrown.
#'
#' @example man/examples/example-ModelMove.R
#' @inherit State seealso
#' @author Edward Lavender
#' @name ModelMove
#' @aliases .model_move ModelMoveXY ModelMoveXYZD move_xy move_xyzd

#' @rdname ModelMove
#' @export

move_xy <- function(dbn_length = "truncated(Gamma(1, 250.0), upper = 750.0)",
                    dbn_angle = "Uniform(-pi, pi)") {
  julia_check_exists("env")
  glue('ModelMoveXY(env, {dbn_length}, {dbn_angle});')
}

#' @rdname ModelMove
#' @export

move_xyzd <- function(dbn_length = "truncated(Gamma(1.0, 750.0), upper = 750.0)",
                      dbn_angle_delta = "Normal(0, 0.5)",
                      dbn_z_delta = "Normal(0, 3.5)") {
  julia_check_exists("env")
  glue('ModelMoveXYZD(env, {dbn_length}, {dbn_angle_delta}, {dbn_z_delta});')
}
