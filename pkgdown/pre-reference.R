# pre-reference.R

# We use pkgdown::build_site() to build a website
# pkgdown/pre-reference.R is run within pkgdown::build_site()
# ... before pkgdown::build_reference() within that function
# ... (see ?pkgdown::build_reference)
# We preload library(sf) here:
# * This solves a crytic R/Julia callr error in pkgdown::build_site() (see below)
# * This enables us to build the site with pkgdown::build_site()

# Error:
#   ! in callr subprocess.
# Caused by error in `build_reference()`:
#   ! Failed to parse Rd in map_dens.Rd
# Caused by error:
#   ! .onLoad failed in loadNamespace() for 'sf', details:
#   call: CPL_crs_from_input(x)
# error: c++ exception (unknown reason)
# ℹ See `$stdout` and `$stderr` for standard output and error.
# Type .Last.error to see the more details.

library(sf)
