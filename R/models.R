#' @title States
#' @description [`State`] is an Abstract Type in [`Patter.jl`](https://edwardlavender.github.io/Patter.jl) that groups state sub-types.
#'
#' @details
#' [`State`] sub-types are `Julia` structures that hold the parameters that describe the state of an individual at a given time step. 'State' typically means 'location' (in two or three dimensions), but individual states may include additional fields for those state dimensions that depend on the state time step. (For example, in correlated random walks, the [`State`] should contain the heading, given that the heading at one time step depends on the previous heading and a turning angle.) From an `R`-user perspective, you can think of a [`State`] sub-type as an `S4`-[`class`]-like object, with slots for the state dimensions.
#'
#' In [`patter`] functions, the `.state` argument is a `character` string that defines the animal's state sub-type. This must match a [`State`] sub-type defined in `Julia`. The built-in options are:
#' * `"StateXY"`, which maps to `StateXY`;
#' * `"StateXYZ"`, which maps to `StateXYZ`;
#' * `"StateCXY"`, which maps to `StateCXY` ;
#' * `"StateCXYZ"`, which maps to `StateCXYZ`;
#'
#' See [`State`](https://edwardlavender.github.io/Patter.jl) or `JuliaCall::julia_help("State")` for the fields of the built-in sub-types.
#'
#' `.state` is used by [`sim_path_walk()`] and [`pf_filter()`], both of which effectively simulate time series of states. `.state` controls the simulation of initial locations and subsequent method dispatch in [`Patter.jl`](https://github.com/edwardlavender/Patter.jl). [`Patter.sim_states_init()`](https://github.com/edwardlavender/Patter.jl) handles the simulation of initial states in these routines. When `.state` is
#' * `"StateXY"`, the initial state comprises `x` and `y` coordinates;
#' * `"StateXYZ"`, the initial state comprises `x`, `y` and `z` coordinates;
#' * `"StateCXY"`, the initial state comprises `x` and `y` coordinates and an initial heading;
#' * `"StateCXYZ"`, the initial states comprises `x`, `y` and `z` coordinates and an initial heading;
#'
#' All states additionally include a `map_value` field that defines the value on the movement map at (`x`, `y`).
#'
#' The outcome of [`Patter.sim_states_init()`](https://github.com/edwardlavender/Patter.jl) is a `DataFrame` of initial state(s), which is coerced to a `Vector` of `State`s in `Julia` for the simulation. In [`Patter.jl`](https://github.com/edwardlavender/Patter.jl), the simulation of subsequent states depends on the input state and the movement model.
#'
#' The state must match the movement model (see [`ModelMove`]):
#' * For `"StateXY"`, a movement model that simulates step lengths and headings, and updates state components (that is, `x` and `y` coordinates), is required;
#' * For `"StateXYZ"`, a movement model that simulates step lengths, headings and depths, and updates state components (that is, `x`, `y` and `z` coordinates), is required;
#' * For `"StateCXY"`, a movement model that simulates step lengths and turning angles, and updates `x`, `y`, `heading` state components, is required.
#' * For `"StateCXYZ"`, a movement model that simulates step lengths, turning angles and changes in depth, and updates `x`, `y`, `z` and `heading` state components, is required.
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
#' @aliases .state StateXY StateXYZ StateCXY StateCXYZ
NULL


#' @title Observation models
#' @description [`ModelObs`] is Abstract Type in [`Patter.jl`](https://edwardlavender.github.io/Patter.jl) that groups observation model sub-types.
#'
#' @details
#' Observation model sub-types are `Julia` structures that hold the parameters of observation models. From an `R`-user perspective, you can think of a [`ModelObs`] sub-type as an `S4`-[`class`]-like object, with slots for the parameters of an observation model. With an observation model structure, we can simulate new observations and evaluate the log-probability of existing observations.
#'
#' The following observation models are built in to [`Patter.jl`](https://edwardlavender.github.io/Patter.jl):
#' * [`ModelObsAcousticLogisTrunc`]
#' * [`ModelObsAcousticContainer`]
#' * [`ModelObsDepthUniform`]
#' * [`ModelObsDepthNormalTrunc`]
#'
#' See [`Patter.jl`](https://edwardlavender.github.io/Patter.jl) or `JuliaCall::julia_help("ModelObs")` for the fields of the built-in sub-types.
#'
#' In [`patter`], observation models are required:
#' * To simulate new observational datasets, via [`sim_observations()`];
#' * To run the particle filter, via [`pf_filter()`];
#'
#' Observation model sub-types and parameters should be specified as a named `list` of [`data.table`](s). Internally, observation model sub-types and parameters are instantiated and used to simulate observations or in the particle filter. The simulation of observations is implemented via [`Patter.simulate_obs()`](https://edwardlavender.github.io/Patter.jl). In the particle filter, log-probabilities are evaluated by [`Patter.logpdf_obs()`](https://edwardlavender.github.io/Patter.jl). These are generic functions. Different methods are dispatched according to the input model. For the built-in [`ModelObs`] sub-types, corresponding methods for these routines are also built-in. For custom [`ModelObs`] sub-types, the methods need to be provided.
#'
#' To use custom [`ModelObs`] sub-types, see Examples.
#'
#' @example man/examples/example-ModelObs.R
#' @inherit State seealso
#' @author Edward Lavender
#' @name ModelObs
#' @aliases .model_obs ModelObsAcousticLogisTrunc ModelObsAcousticContainer ModelObsDepthUniform ModelObsDepthNormalTrunc
NULL


#' @title Movement models
#' @description [`ModelMove`] is Abstract Type in [`Patter.jl`](https://edwardlavender.github.io/Patter.jl) that groups movement model sub-types, of which instances can be created via an `R` `move_*()` function.
#'
#' @param .mobility,.dbn_length,.dbn_heading,.dbn_heading_delta,.dbn_z,.dbn_z_delta `Character` strings that define movement model components:
#' * `.mobility`---the maximum movement distance between two time steps (m);
#' * `.dbn_length`---the distribution of step lengths (m);
#' * `.dbn_heading`---the distribution of headings (rad);
#' * `.dbn_heading_delta`---the distribution of changes in heading, i.e., turning angle (rad);
#' * `.dbn_z`---the distribution of depths;
#' * `.dbn_z_delta`---the distribution of changes in depth;
#'
#' @details Movement model sub-types are `Julia` structures that hold the components of movement models. From an `R`-user perspective, you can think of a [`ModelMove`] sub-type as an `S4`-[`class`]-like object, with slots for the components of a movement model. With a movement model instance, we can simulate movements and evaluate the density of movements from one state (location) to another.
#'
#' The following movement models are built in to [`Patter.jl`](https://edwardlavender.github.io/Patter.jl):
#' * Random walks (RWs):
#'    * [`ModelMoveXY`]
#'    * [`ModelMoveXYZ`]
#' * Correlated random walks (CRWs):
#'    * [`ModelMoveCXY`]
#'    * [`ModelMoveCXYZ`]
#'
#' See [`Patter.jl`](https://edwardlavender.github.io/Patter.jl) or `JuliaCall::julia_help("ModelMove")` for the fields of the built-in sub-types. Briefly, all sub-types include:
#' * A `map` field, that defines the region(s) within which movements are permitted. In `R`, it is convenient to represent `map` as a [`SpatRaster`], where `NAs` define inhospitable habitats (e.g., land). This should made available to `Julia` [`ModelMove`] constructors as `env` via [`set_map()`];
#' * The `mobility` parameter;
#' * Additional model-specific components (such as fields for the distribution of step lengths and headings in the case of two-dimensional random walks);
#'
#' In [`patter`], movement models are required:
#' * To simulate movement paths, via [`sim_path_walk()`];
#' * To run the particle filter, via [`pf_filter()`];
#' * To run the particle smoother, via [`pf_smoother_two_filter()`];
#'
#' In `R` functions, the movement-model instance is specified via the `.model_move` argument. This argument expects a `character` string defining a [`ModelMove`] instance that can be evaluated in `Julia` (that is, a [`ModelMove`] constructor). `move_*()` functions are convenience functions for the specification of these constructors for the built-in sub-types. All [`ModelMove`] instances contain a `map` field that defines the region(s) within which movements are permitted. To use a `move_*()` function, the map should be available in `Julia` as `env` (see [`set_map()`]). The additional components of the movement model are specified via `move_*()` function arguments as `character` strings of `Julia` code. Currently implemented `move_*()` functions are:
#' * [`move_xy()`], which specifies a RW in X and Y of sub-type [`ModelMoveXY`] in terms of the distributions of step lengths and headings;
#' * [`move_xyz()`], which specifies a RW in X, Y and Z of sub-type [`ModelMoveXY`] in terms of the distributions of step lengths, headings and depths;
#' * [`move_cxy()`], which specifies a CRW in X and Y of sub-type [`ModelMoveXY`] in terms of the distributions of step lengths and turning angles;
#' * [`move_cxyz()`], which specifies a CRW in X, Y and Z of sub-type [`ModelMoveCXYZ`] in terms of the distributions of step lengths, turning angles and changes in depth;
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
#' @aliases .model_move ModelMoveXY ModelMoveXYZ ModelMoveCXY ModelMoveCXYZ move_xy move_xyz move_cxy move_cxyz

#' @rdname ModelMove
#' @export

move_xy <- function(.mobility = "750.0",
                    .dbn_length = "truncated(Gamma(1, 250.0), upper = 750.0)",
                    .dbn_heading = "Uniform(-pi, pi)") {
  julia_check_exists("env")
  glue('ModelMoveXY(env, {.mobility}, {.dbn_length}, {.dbn_heading});')
}

#' @rdname ModelMove
#' @export

move_xyz <- function(.mobility = "750.0",
                     .dbn_length = "truncated(Gamma(1, 250.0), upper = 750.0)",
                     .dbn_heading = "Uniform(-pi, pi)",
                     .dbn_z = "truncated(Normal(100.0, 250.0), lower = 0.0, upper = 350.0)") {
  julia_check_exists("env")
  glue('ModelMoveXYZ(env, {.mobility}, {.dbn_length}, {.dbn_heading}, {.dbn_z});')
}

#' @rdname ModelMove
#' @export

move_cxy <- function(.mobility = "750.0",
                     .dbn_length = "truncated(Gamma(1.0, 750.0), upper = 750.0)",
                     .dbn_heading_delta = "Normal(0, 0.5)") {
  julia_check_exists("env")
  glue('ModelMoveCXY(env, {.mobility}, {.dbn_length}, {.dbn_heading_delta});')
}

#' @rdname ModelMove
#' @export

move_cxyz <- function(.mobility = "750.0",
                      .dbn_length = "truncated(Gamma(1.0, 750.0), upper = 750.0)",
                      .dbn_heading_delta = "Normal(0, 0.5)",
                      .dbn_z_delta = "Normal(0, 3.5)") {
  julia_check_exists("env")
  glue('ModelMoveCXYZ(env, {.mobility}, {.dbn_length}, {.dbn_heading_delta}, {.dbn_z_delta});')
}
