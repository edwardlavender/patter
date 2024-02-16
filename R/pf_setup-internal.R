#' @title PF: set up internals
#' @description [`.pf_setup_obs_receiver_id_next()`] defines, for each time step, the receiver(s) at which the next detection was recorded.
#' @param .receiver_id A `list` column.
#' @return The function returns a `list` column that defines, for each time step, the receiver(s) that recorded the next detection.
#' @author Edward Lavender
#' @seealso The `receiver_id_next` column is required by [`acs_filter_container()`].
#' @keywords internal

.pf_setup_obs_receiver_id_next <- function(.receiver_id) {
  rlang::check_installed("zoo")
  dt <- data.table(receiver_id = .receiver_id)
  dt$receiver_id[sapply(dt$receiver_id, is.null)] <- list(NA_integer_)
  out <-
    dt |>
    mutate(receiver_id_next = lead(.data$receiver_id),
           receiver_id_next = zoo::na.locf(.data$receiver_id_next,
                                           fromLast = TRUE,
                                           na.rm = FALSE))
  out$receiver_id_next[nrow(out)][[1]] <- NA_integer_
  out$receiver_id_next
}
