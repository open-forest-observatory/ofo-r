# Setup
devtools::load_all()

library(tidyverse)
library(lidR)


# ---- Simulate a "predicted" and "observed" tree map
sim = simulate_tree_maps(trees_per_ha = 300, trees_per_clust = 5, cluster_radius = 5,
                         obs_extent = 60,
                         pred_extent = 300,
                         horiz_jitter = 3,
                         vert_jitter = 5, # max of 5
                         false_pos = 0.25,
                         false_neg = 0.50,
                         drop_observed_understory = TRUE,
                         shift_x = -9.25,
                         shift_y = 15.5)



X = runif(1000, 684766, 684993)
Y = runif(1000, 5017773, 5018007)
Z = runif(1000, 0, 30)
data = data.frame(X,Y,Z)

pred = sim$pred
coords = pred |> select (X = x, Y = y, Z = z)
pred_las = LAS(coords)

obs = sim$obs
coords = obs |> select (X = x, Y = y, Z = z)
obs_las = LAS(coords)

# Write to temp dir
pred_file = tempfile(fileext = ".las")
obs_file = tempfile(fileext = ".las")
out_file = tempfile(fileext = ".las")
writeLAS(pred_las, pred_file)
writeLAS(obs_las, obs_file)


# Create PDAL pipeline for filter.CPD algorithm
pipeline = c(
  "[",
    paste0('"', pred_file, '"', ","),
    paste0('"', obs_file, '"', ","),
    "{",
      '"type":"filters.cpd",',
      '"method":"rigid"',
      "},",
    paste0('"', out_file, '"'),
  "]"
)

pipeline_file = tempfile(fileext = ".json")
writeLines(pipeline, pipeline_file)

