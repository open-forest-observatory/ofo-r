# Purpose: Demonstrate function for identifying the optimal x, y, z shift to align two (simulated)
# tree maps

# Setup

library(spatstat)
library(tidyverse)
set.seed(1)

# Functions

# Visualize a tree map
vis1 = function(trees) {
    
    p = ggplot(trees, aes(x, y, size = z , color = z)) +
    geom_point() +
    scale_size_continuous(range = c(0.5, 3)) +
    scale_color_viridis_c(end = 0.85) +
    theme_bw() +
    coord_cartesian(xlim = c(-10, 10), ylim = c(-10, 10))

    print(p)
}

# Visualize two tree maps (predicted and observed) overlaid
vis2 = function(pred, obs) {

    pred = pred |> mutate(layer = "predicted")
    obs = obs |> mutate(layer = "observed")
    
    trees = bind_rows(pred, obs)
    
    p = ggplot(trees, aes(x, y, size = z , color = layer)) +
    geom_point() +
    scale_size_continuous(range = c(0.5, 3)) +
    theme_bw() +
    coord_cartesian(xlim = c(-10, 10), ylim = c(-10, 10))

    print(p)
}

# ---- Simulate a "predicted" and "observed" tree map

# Generate random, slightly clustered points in x-y space, over an area wider than would reasonably
# have a field stem map (to represent the drone-based tree predictions)
pred = rMatClust(kappa = 0.4, scale = 2, mu = 5, win = as.owin(c(-10, 10, -10, 10)))
pred = as.data.frame(pred)

# Add random heights, and sort so smallest plot on top
pred$z = runif(nrow(pred), 0, 10)
pred = pred[order(-pred$z), ]

# Visualize the map
vis1(pred)

# Take a spatial subset of these points, to represent the "observed" tree map (e.g. a field plot)
obs = pred |>
  filter(between(x, -3, 3) & between(y, -3, 3))

# Visualize the map
vis1(obs)

# Vis the two overlaid
vis2(pred, obs)

# Remove 20% of the trees from each map, to simulate false positives and false negatives
pred = pred |>
  sample_frac(0.8)
obs = obs |>
  sample_frac(0.8)
  
# Add some noise to the predicted tree heights
pred$z = pred$z + runif(nrow(pred), 0, 2)

vis2(pred, obs)

# Shift one slightly to help visualize the slight differences and the alignment challenge
obs_shift = obs |>
  mutate(x = x + 0.2, y = y + 0.2)
  
vis2(pred, obs_shift)

# Shift obs a known amount that we will attempt to recover
obs = obs |>
  mutate(x = x + 5, y = y + 3)

vis2(pred, obs)


# ---- Define objective function

# For each observed point, get the closest predicted point in x, y, z space
# (note: this is not a true distance, but is fine for our purposes)
get_closest_obs = function(obs, pred) {
  
  # Get the closest predicted point to each observed point, in x, y, z space
  closest = pred |>
    group_by(x, y) |>
    mutate(dist = sqrt((x - first(obs$x))^2 + (y - first(obs$y))^2 + (z - first(obs$z))^2)) |>
    filter(dist == min(dist)) |>
    ungroup()
  
  # Join back to the observed points
  obs = obs |>
    left_join(closest, by = c("x", "y"))
  
  return(obs)
}

vis2(pred_crop, obs)


objective = function(pred, obs) {
        
    # For each predicted point, get the closest observed point in x, y, z space

    # Crop the predicted points to the x-y extent of the observed points + 50%, so we don't have to
    # compute distances to points that are definitely not the closest

    xmin = min(obs$x)
    ymin = min(obs$y)
    xmax = max(obs$x)
    ymax = max(obs$y)
    xrange = xmax - xmin
    yrange = ymax - ymin

    pred_crop = pred |>
    filter(between(x, xmin - xrange/2, xmax + xrange/2) & between(y, ymin - yrange/2, ymax + yrange/2))
    
    # For each observed point, get the distance to every predicted point
    # TODO: Is it OK that multiple observed points may be matched to the same predicted point? Maybe
    # keep it simple for the sake of speed, if it can recover the correct shift
    dists = sqrt(outer(obs$x, pred_crop$x, "-")^2 +
                outer(obs$y, pred_crop$y, "-")^2 +
                outer(obs$z, pred_crop$z, "-")^2
            )

    # For each observed point, get the distance to the nearest predicted point
    min_dists = apply(dists, 1, min)

    # Summarize the alignment as the mean distance to the nearest predicted point
    objective = mean(min_dists)
    
    return(objective)
}

objective(pred_crop, obs)


# ---- Define the shifts to test

x_shifts = seq(-10, 10, 0.5)
y_shifts = seq(-10, 10, 0.5)

shifts = expand.grid(x_shift = x_shifts, y_shift = y_shifts)


# Try each shift and compute the objective function
for(i in 1:nrow(shifts)) {
    
    shift = shifts[i, ]
    
    obs_shifted = obs |>
      mutate(x = x + shift$x_shift, y = y + shift$y_shift)
      
    shifts[i, "objective"] = objective(pred, obs_shifted)
}

# Get the shift that minimized the objective function
shifts |> 
  filter(objective == min(objective))
