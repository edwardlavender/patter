#### Example (1): Calculate diagnostics from objects in memory
diag <- dat_pfbk() |> pf_backward_killer_diagnostics()
head(diag)

#### Example (2): Calculate diagnostics from parquet files
pfb_folder <- dat_pfbk_src()
diag_2 <- pf_backward_killer_diagnostics(pfb_folder)
stopifnot(isTRUE(all.equal(diag, diag_2)))

#### Example (3): Analyse diagnostics
# Define helper function
plot_loess <- function(x, y,
                       xlab = deparse(substitute(x)),
                       ylab = deparse(substitute(y)), ...) {
  plot(x, y,
       xlab = xlab, ylab = ylab,
       type = "l")
  lines(loess.smooth(x, y), col = "royalblue", lwd = 2)
}
# Plot time series
pp <- par(mfrow = c(1, 3))
plot_loess(diag$timestep, diag$n)
plot_loess(diag$timestep, diag$n_u)
plot_loess(diag$timestep, diag$ess)
par(pp)
