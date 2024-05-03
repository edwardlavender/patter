#' @title Movement models
#' @description These functions formulate movement model constructors for export to Julia. Function arguments should be specified as `character` strings of Julia code that specify the distributions for components of the movement model.
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
#' @name move

#' @rdname move
#' @export

move_xy <- function(length = "truncated(Gamma(1, 250.0), upper = 750.0)",
                    angle = "Uniform(-pi, pi)") {
  julia_check_exists("env")
  glue('ModelMoveXY(env, {length}, {angle});')
}

move_xyz <- function() {

}

#' @rdname move
#' @export

move_xyzd <- function(length = "truncated(Gamma(1.0, 750.0), upper = 750.0)",
                      angle_delta = "Normal(0, 0.5)",
                      z_delta = "Normal(0, 3.5)") {
  julia_check_exists("env")
  glue('ModelMoveXYZD(env, {length}, {angle_delta}, {z_delta});')
}
