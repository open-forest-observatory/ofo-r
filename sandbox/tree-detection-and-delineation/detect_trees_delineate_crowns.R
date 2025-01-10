# Purpose: Perform geometric treetop detection and delineation for a single CHM

# SETUP

library(terra)
library(lidR)
library(sf)
library(nngeo)
library(tictoc)

CHM_FILEPATH = "/ofo-share/cv-itd-eval_data/photogrammetry-outputs/emerald-point_10a-20230103T2008/chm.tif"


# PROCESSING

# start timer
tic()


chm = rast(CHM_FILEPATH)

chm[chm < 0] = -0.1

chm_res = res(chm) %>% mean

#resample coarser
chm_coarse = project(chm, y = "epsg:3310", res = 0.25)

# apply sliding window mean smooth
smooth_size = 7
weights = matrix(1,nrow = smooth_size, ncol = smooth_size)
chm_smooth = focal(chm_coarse, weights, fun = mean)

# define variable window function
lin = function(x){
  win = x*0.11 + 0
  win[win < 0.5] = 0.5
  win[win > 100] = 100
  return(win)
}



# # Need to get the full raster loaded into memory before the locate_trees step
# writeRaster(chm_smooth, file.path(data_dir, "temp/chm.tif"), overwrite = TRUE)
# chm_smooth = raster::raster(file.path(data_dir, "temp/chm.tif"))
# chm_smooth = chm_smooth * 1

treetops = locate_trees(chm_smooth, lmf(ws = lin, shape = "circular", hmin = 5))
gc()

treetops = as(treetops, "sf")
treetops = st_transform(treetops, st_crs(chm))


crowns = silva2016(chm, treetops, max_cr_factor = 0.24, exclusion = 0.1)()

crowns = as.polygons(crowns)
crowns = st_as_sf(crowns)
crowns = st_cast(crowns, "MULTIPOLYGON")
crowns = st_cast(crowns, "POLYGON")
crowns = st_remove_holes(crowns)
crowns = st_make_valid(crowns)

# stop timer
toc()

# # Extra steps to make nice crowns, beyond the scope of benchmarking against TDF
# crowns <- smooth(crowns, method = "ksmooth", smoothness = 3)
# crowns <- st_simplify(crowns, preserveTopology = TRUE, dTolerance = 0.1)
