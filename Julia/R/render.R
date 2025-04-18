# Render.R

# This script renders the README for the Patter.jl package
# It is convenient to do this within {patter} & copy the README to Patter.jl

library(testthat)
library(tictoc)

# (optional) Set Patter.jl logo
if (FALSE) {

  # (A) Use use_logo() to make Patter.jl logo & copy to Julia directory
  usethis::use_logo("/Users/lavended/Documents/work/projects/move-smc/patter/supporting/doc/fig/hex-sticker/v2/logo-jl.png")
  success <- file.copy("man/figures/logo.png", "Julia/docs/figures/logo.png")
  expect_true(success)

  # (B) Manually update Julia/README.Rmd if required
  # patter <a href="https://edwardlavender.github.io/patter/"><img src="man/figures/logo.png" align="right" height="130" alt="patter website" /></a>
  # # `Patter.jl` <a href="https://edwardlavender.github.io/Patter.jl/"><img src="docs/figures/logo.png" align="right" height="130" alt="Patter.jl website" /></a>

  # (C) Restore patter logo
  usethis::use_logo("/Users/lavended/Documents/work/projects/move-smc/patter/supporting/doc/fig/hex-sticker/v2/logo.png")
}

# Define output connection
outcon <- "/Users/lavended/Documents/work/projects/move-smc/patter/packages/Patter.jl"
expect_true(dir.exists(outcon))

# Render README.Rmd (~40 s)
tic()
rmarkdown::render(file.path("julia", "README.Rmd"),
                  output_file = "README.md",
                  output_format = "github_document")
toc()

# Copy MD file
success <- file.copy(file.path("julia", "README.md"),
                     file.path(outcon, "README.md"),
                     overwrite = TRUE)
expect_true(success)

# Copy figures
figs_from <- list.files(file.path("julia", "docs", "figures"), full.names = TRUE)
figs_to   <- file.path(outcon, "docs", "figures", basename(figs_from))
success <- file.copy(figs_from, figs_to, overwrite = TRUE)
expect_true(all(success))
