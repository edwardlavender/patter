#### Example (1): Use particle samples from pf_forward() or pf_backward_*()
pf_diag_summary(.history = dat_pff())
# (TO REVISE)

#### Example (2): Use particle samples in memory or on file
# Particles can be provided in any format accepted by `?.pf_history_dt()`
d1 <- pf_diag_summary(.history = dat_pff())
d2 <- pf_diag_summary(.history = dat_pff()$history)
d3 <- pf_diag_summary(.history = dat_pff_src())
d4 <- pf_diag_summary(.history = pf_files(dat_pff_src()))
stopifnot(isTRUE(all.equal(d1, d2)))
stopifnot(isTRUE(all.equal(d1, d3)))
stopifnot(isTRUE(all.equal(d1, d4)))

## (C) Examine trends in particle diagnostics
# (TO REVISE)
