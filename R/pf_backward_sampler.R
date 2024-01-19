#' @title PF: count particles
#' @description This function counts particle (cell) samples. Its primary use is to process outputs from forward filter for [`pf_backward_sampler()`].
#' @param .history Particle samples, provided in any format accepted by [`.pf_history_list()`]. Particle samples must contain the `cell_now` column.
#' @param .cl,.cl_varlist,.cl_chunk Parallelisation options, passed to [`cl_lapply()`].
#' @param .record A named `list` of output options, from [`pf_opt_record()`].
#' @param .verbose User output control (see [`patter-progress`] for supported options).
#'
#' @details
#' [`pf_count()`] iterates over time steps, counts the number of copies of each cell and saves a [`data.table`] of counts in memory and/or writes them to file for [`pf_backward_sampler()`]. This is designed to improve speed in [`pf_backward_sampler()`].
#'
#' @return The function returns a [`pf_particles-class`] object.
#'
#' @author Edward Lavender
#' @export

pf_count <- function(.history,
                     .cl = NULL, .cl_varlist = NULL, .cl_chunk = TRUE,
                     .record = pf_opt_record(),
                     .verbose = getOption("patter.verbose")) {

  #### Check user inputs
  # TO DO (enhance checks)

  #### Set up messages
  t_onset <- Sys.time()
  cat_log <- cat_init(.verbose = .verbose)
  cat_log(call_start(.fun = "pf_count", .start = t_onset))
  on.exit(cat_log(call_end(.fun = "pf_count", .start = t_onset, .end = Sys.time())), add = TRUE)

  #### Set up loop
  .history <- .pf_history_list(.history)
  read     <- .pf_history_read(.history)
  write    <- !is.null(.record$sink)
  check_names(.pf_history_elm(.history, .elm = 1L, .read = read, .cols = .record$cols),
              c("cell_now"))

  #### Process history
  .history <-
    cl_lapply(
      .x = seq_len(length(.history)),
      .cl = .cl, .varlist = .cl_varlist, .chunk = .cl_chunk,
      .fun = function(t) {

        # Count distinct cells
        d <-
          .pf_history_elm(.history,
                          .elm = t,
                          .read = .pf_history_read(.history),
                          .cols = .record$cols) |>
          lazy_dt() |>
          group_by(.data$cell_now) |>
          mutate(n = n()) |>
          slice(1L) |>
          ungroup() |>
          as.data.table()

        # Record outputs
        if (write) {
          arrow::write_parquet(d, file.path(.record$sink, paste0(t, ".parquet")))
        }
        if (.record$save) {
          return(d)
        } else {
          return(NULL)
        }
      })

  # Return outputs
  .pf_backward_killer_outputs(.start = t_onset,
                              .history = .history,
                              .record = .record)

}
