#### Set up example
# Set up the particle filter with an example dataset
# (See `?pf_filter()` for the full workflow)
args <- example_setup("pf_smoother_two_filter")
# Run the particle filter forwards
args$.direction <- "forward"
fwd <- do.call(pf_filter, args)
# Run the particle filter backwards
args$.direction <- "backward"
bwd <- do.call(pf_filter, args)

#### Example (1): Implement the smoother with default options
# Run the smoother
# * This uses objects defined by `pf_filter()` in `Julia`
smo <- pf_smoother_two_filter()
# The filter returns a `pf_particles`-class object
# (See `?pf_filter()` for examples)
class(smo)
summary(smo)

#### Example (2): Implement the smoother using 'mobility box' arguments
# We can take advantage of the 'mobility box' arguments b/c:
# * `.state` = "StateXY"
# * `.map` does not contain NAs
args$.state
args$.map
# To implement the mobility `box`, we define `.map` and `.mobility`,
# ... which we can see here is 750 m:
args$.model_move
# Run the smoother
# * In all of these examples, we should implement the smoother like this,
# * but for illustration purposes we only do so here.
smo <- pf_smoother_two_filter(.map = args$.map, .mobility = 750.0)

#### Example (3): Implement the smoother with a sub-sample of particles
# This is useful for quick tests
smo <- pf_smoother_two_filter(.n_particle = 50L)

#### Example (4): Adjust the number of MC simulations
smo <- pf_smoother_two_filter(.n_sim = 1000L)

#### Example (5): Analyse smoothed particles
# * See `map_*()` functions (e.g., `?map_dens()`) to map utilisation distributions
