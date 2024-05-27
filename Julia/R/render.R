# Render.R

# This script renders the README for the Patter.jl package
# It is convenient to do this within {patter} & copy the README to Patter.jl

library(tictoc)

# Render README.Rmd (~40 s)
tic()
rmarkdown::render(file.path("julia", "README.Rmd"))
toc()

# Define output connection
outcon <- "/Users/lavended/Documents/work/projects/particle-filters/patter/packages/Patter.jl"

# Copy MD file
file.copy(file.path("julia", "README.md"),
          file.path(outcon, "README.md"),
          overwrite = TRUE)

# Copy figures
figs_from <- list.files(file.path("julia", "docs", "figures"), full.names = TRUE)
figs_to   <- file.path(outcon, "docs", "figures", basename(figs_from))
file.copy(figs_from, figs_to, overwrite = TRUE)
