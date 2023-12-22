# For outputs stored in memory, extract the diagnostics element
dat_pff()$diagnostics

# pf_forward_diagnostics() also works but is unnecessary
pf_forward_diagnostics(dat_pff())

# For outputs stored on file, use pf_forward_diagnostics()
pff_folder <- dat_pff_src(.folder = NULL)
pf_forward_diagnostics(pff_folder)
pf_forward_diagnostics(file.path(pff_folder, "diagnostics"))

# Outputs are identical
stopifnot(all.equal(
  dat_pff()$diagnostics,
  pf_forward_diagnostics(pff_folder)
))
