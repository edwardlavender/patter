#### Set up
# Define directories
con    <- file.path(tempdir(), "patter")
frames <- file.path(con, "frame")
mp4s   <- file.path(con, "mp4")
dir.create(frames, recursive = TRUE)
dir.create(mp4s, recursive = TRUE)
# Define data list
dlist <- dat_dlist()

#### Example (1): Plot selected samples (from the forward run)
# Use particles in memory
pf_plot_history(.dlist = dlist,
                .forward = dat_pff(),
                .steps = 1L)
# Use the `history` list directly
pf_plot_history(.dlist = dlist,
                .forward = dat_pff()$history,
                .steps = 1L)
# Use a directory
pf_plot_history(.dlist = dlist,
                .forward = dat_pff_src(),
                .steps = 1L)
# Use a list of files
pf_plot_history(.dlist = dlist,
                .forward = pf_files(dat_pff_src()),
                .steps = 1L)

#### Example (2): Plot selected samples (from the backward pass)
pf_plot_history(.dlist = dlist,
                .forward = dat_pfbk(),
                .steps = 1L)

#### Example (3) Plot particles from both forward and backward runs
# Dead ends are shown in red
pf_plot_history(.dlist = dlist,
                .forward = dat_pff(),
                .backward = dat_pfbk(),
                .steps = 1L)

#### Example (4): Plot multiple time steps
# Specify selected steps
pf_plot_history(.dlist = dlist,
                .forward = dat_pfbk(),
                .steps = 1:5L)
# Plot all steps (default: .step = NULL)
pf_plot_history(.dlist = dlist,
                .forward = dat_pfbk())
# Use `.prompt = TRUE`
pf_plot_history(.dlist = dlist,
                .forward = dat_pfbk(),
                .prompt = TRUE)

#### Example (5): Customise the plot
# Customise the SpatRaster surface
pf_plot_history(.dlist = dlist,
                .forward = dat_pfbk(),
                .steps = 1L,
                .add_surface = list(col = grDevices::cm.colors(256)))
# Customise the particle samples
pf_plot_history(.dlist = dlist,
                .forward = dat_pfbk(),
                .steps = 1L,
                .add_forward = list(pch = ".", col = "blue"))

#### Example (6): Write images to file
pf_plot_history(.dlist = dlist,
                .forward = dat_pff(),
                .backward = dat_pfbk(),
                .add_forward = list(pch = 21, cex = 0.5),
                .png = list(filename = frames))

#### Example (7): make animations
if (rlang::is_installed("av")) {

  # There are lots of tools to create animations:
  # * `av::av_encode_video()`       # uses ffmpeg
  # * `animation::saveVideo()`      # uses ffmpeg
  # * `magick::image_write_video()` # wraps av()
  # * `glatos::make_video()`        # wraps av()

  # Helper function to open (mp4) files
  Sys.open <- function(.file) {
    if (.Platform$OS.type == "Windows") {
      cmd <- paste("start", shQuote(.file))
    } else {
      cmd <- paste("open", shQuote(.file))
    }
    system(cmd)
  }

  # Use av::av_encode_video()
  # * This is one of the faster options
  input   <- unlist(pf_files(frames))
  output  <- file.path(mp4s, "ani.mp4")
  av::av_encode_video(input, output, framerate = 10)
  # Sys.open(output)

}

# Clean up
unlink(con, recursive = TRUE)
