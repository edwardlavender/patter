#### Example (1): Use particle samples from pf_forward() or pf_backward_*()
pf_diag_summary(.history = dat_pff())
pf_diag_summary(.history = dat_pfbk())
pf_diag_summary(.history = dat_pfbs())

#### Example (2): Use particle samples in memory or on file
# Particles can be provided in any format accepted by `?.pf_history_dt()`
d1 <- pf_diag_summary(.history = dat_pfbk())
d2 <- pf_diag_summary(.history = dat_pfbk()$history)
d3 <- pf_diag_summary(.history = dat_pfbk_src())
d4 <- pf_diag_summary(.history = pf_files(dat_pfbk_src()))
stopifnot(isTRUE(all.equal(d1, d2)))
stopifnot(isTRUE(all.equal(d1, d3)))
stopifnot(isTRUE(all.equal(d1, d4)))

## (C) Examine trends in particle diagnostics

# Collate particle diagnostics
diag_f <- pf_diag_summary(dat_pff())
diag_k <- pf_diag_summary(dat_pfbk())
diag_s <- pf_diag_summary(dat_pfbs())

# Plot diagnostic time series
pf_plot_diag_ts <- function(.f, .k, .s, .metric = c("n", "nu", "ess"), ...){
  # Define base plot
  .metric <- match.arg(.metric)
  vals    <- c(.f[[.metric]], .k[[.metric]], .s[[.metric]])
  if (all(is.na(vals))) {
    return(NULL)
  }
  ylim <- range(vals, na.rm = TRUE)
  plot(.f$timestep, .f[[.metric]],
       xlab = "time step", ylab = .metric,
       ylim = ylim, type = "n", ...)
  # Add diagnostics for each algorithm
  add_diag <- function(x, y, col) {
    if (!all(is.na(y))) {
      # Add semi-transparent smoother (colour: ct)
      ct <- col2rgb(col) / 255
      ct <- rgb(ct[1], ct[2], ct[3], alpha = 0.25)
      lines(stats::loess.smooth(x, y), lwd = 2, col = ct)
      # Add points
      points(x, y, cex = 0.5, col = col)
    }
    invisible(NULL)
  }
  add_diag(.f$timestep, .f[[.metric]], col = "red")
  add_diag(.k$timestep, .k[[.metric]], col = "orange")
  add_diag(.s$timestep, .s[[.metric]], col = "darkgreen")
}

# Plot time series
# * Red:    pf_forward()
# * Orange: pf_backward_killer()
# * Green:  pf_backward_sampler_*()
pp <- par(mfrow = c(1, 3))
cl_lapply(c("n", "nu", "ess"), function(.metric) {
  print(.metric)
  pf_plot_diag_ts(.f = diag_f,
                  .k = diag_k,
                  .s = diag_s,
                  .metric = .metric)
})
par(pp)
