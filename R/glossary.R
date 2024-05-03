#' @title Glossary
#'
#' @description This is a glossary of key arguments in [`patter`].
#'
#' # `.map`.
#'
#' `.map` is [`SpatRaster`] that defines the area of interest. `NAs` define inhospitable regions (such as land).
#'
#' # `.move`.
#'
#' `.move` is a `character` that defines a movement model structure implemented in Julia (that is, a `ModelMove` constructor). `.move_*()` functions are convenience wrappers for the construction of movement models. Currently implemented options are:
#' * [`move_xy()`], which specifies a movement model in terms of the distribution of step lengths and turning angles;
#' * [`move_xyzd()`], which specifies a movement model in terms of the distribution of step lengths, changes in turning angles and changes in depth;
#'
#' The movement model must match the state (see below).
#'
#' # `.state`.
#'
#' `.state` is `character` that defines the animal's state subtype. In the simplest case, an animal's state is a two-dimensional vector of `x` and `y` coordinates that define its location. In more complex scenarios, the state may include additional dimensions, such as a depth (`z`) component.
#'
#' In [`patter`], `.state` is a `character` string that defines the animal's state subtype. This must match a `State` subtype in [`Patter.jl`](https://github.com/edwardlavender/Patter.jl). Currently supported options are:
#' * `"StateXY"`, which maps to `StateXY` in [`Patter.jl`](https://github.com/edwardlavender/Patter.jl).
#' * `"StateXYZD"`, which maps to `StateXYZD` in [`Patter.jl`](https://github.com/edwardlavender/Patter.jl).
#'
#' In [`sim_path_walk()`] and the particle filter, .`state` controls the simulation of initial locations and subsequent method dispatch in [`Patter.jl`](https://github.com/edwardlavender/Patter.jl):
#' * `"StateXY"` requires simulation of initial `x` and `y` coordinates.
#' * `"StateXYZD"` requires the simulation of `x`, `y` and `z` coordinates and an initial direction.
#'
#' The [`data.frame`] of initial state(s) is then coerced to a vector of `State`s in `Julia` for the simulation. In [`Patter.jl`](https://github.com/edwardlavender/Patter.jl), the simulation of subsequent states depends on the input state and the movement model.
#'
#' The state must match the movement model (see `.move`):
#' * For `"StateXY"`, a movement model that simulates step lengths and turning angles and updates state components (that is, `x` and `y` coordinates) is required;
#' * For `"StateXYZD"`, a movement model that simulates step lengths, changes in turning angles and changes in depth and updates `x`, `y`, `z` and direction state components is required.
#'
#' @name glossary
NULL
