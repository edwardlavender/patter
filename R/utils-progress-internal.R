#' @title Progress bar wrappers
#'
#' @description Progress bars in [`patter`] are implemented via the `pbapply` package. These can be customised or suppressed before the implementation of `patter` functions that use progress bars via [`pbapply::pboptions()`]. These simple wrappers facilitate the internal implementation of progress bars via `pbapply` functions.
#'
#' @examples
#' \dontrun{
#' # Define loop
#' loop <- function() {
#'   n <- 10L
#'   pb <- pb_init(.min = 0L, .max = n)
#'   for (i in seq_len(n)) {
#'     Sys.sleep(0.1)
#'     pb_tick(pb, .t = i)
#'   }
#'   pb_close(pb)
#' }
#'
#' # Show default progress bar
#' loop()
#'
#' # Customise default progress bar
#' pbo <- pbapply::pboptions(type = "txt")
#' loop()
#'
#' # Suppress progress bar
#' pbo <- pbapply::pboptions(type = "none")
#' loop()
#'
#' # Reset options
#' pbapply::pboptions(pbo)
#' }
#' @seealso These functions wrap [`pbapply::setpb`] and associated routines.
#'
#' @name pb_

#' @rdname pb_
#' @keywords internal

pb_init <- function(.min, .max) {
  pbapply::startpb(min = .min, max = .max)
}

#' @rdname pb_
#' @keywords internal

pb_tick <- function(.pb, .t) {
  pbapply::setpb(pb = .pb, value = .t)
}

#' @rdname pb_
#' @keywords internal

pb_close <- function(.pb) {
  pbapply::closepb(pb = .pb)
}
