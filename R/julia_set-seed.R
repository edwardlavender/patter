#' @title `Julia`: set the seed
#' @description Use [`set_seed()`] to set the seed in `R` and `Julia` at the start of your workflow.
#' @param .seed An `integer` that defined the seed (see [`set.seed()`]).
#' @seealso [`set.seed()`]
#' @example man/examples/example-julia-set_seed.R
#' @return The function returns `invisible(NULL)`.
#' @author Edward Lavender
#' @export

set_seed <- function(.seed = 123L) {
  stopifnot(!is.null(.seed))
  set.seed(.seed)
  if (julia_works(.action = warn)) {
    julia_command(glue('Random.seed!({.seed});'))
  }
  nothing()
}
