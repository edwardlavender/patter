#' @title Progress bar wrappers
#'
#' @examples
#' \dontrun{
#' # Define loop
#' loop <- function(.progress) {
#'   n <- 10L
#'   pb <- pb_init(n, .progress)
#'   for (i in seq_len(n)){
#'     Sys.sleep(0.1)
#'     pb_tick(pb, .progress)
#'   }
#'   pb_close(pb, .progress)
#' }
#'
#' # Show progress bar
#' loop(.progress = TRUE)
#'
#' # Hide no progress bar
#' loop(.progress = FALSE)
#' }
#' @name pb

#' @rdname pb
#' @keywords internal

pb_init <- function(.n, .init = 0L, .progress) {
  if (.progress) {
    pbapply::timerProgressBar(min = 0L, max = .n, initial = .init)
  } else {
    NULL
  }
}

#' @rdname pb
#' @keywords internal

pb_tick <- function(.pb, .t, .progress) {
  if (.progress) {
    pbapply::setTimerProgressBar(.pb, .t)
  }
}

#' @rdname pb
#' @keywords internal

pb_close <- function(.pb, .progress) {
  if (.progress) {
    pbapply::closepb(.pb)
  }
}
