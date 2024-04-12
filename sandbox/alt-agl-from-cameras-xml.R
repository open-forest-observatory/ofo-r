# Purpose: Extract the mission flight altitude based on photogrammetry outputs (camera positions and
# estimated DSM)

library(tidyverse)
library(elevatr)
library(xml2)
library(sf)
library(terra)

devtools::load_all()

datadir = readLines("sandbox/data-dirs/js2-itd-crossmapping.txt")



photogrammetry_outputs_path = file.path(datadir, "photogrammetry", "outputs")

# Get a list of the projects
projects = list.files(photogrammetry_outputs_path,
                      pattern = "_cameras.xml$") |>
  basename() |>
  str_replace("_cameras.xml", "")

project = projects[1]

missions_agl = map(projects,
                   get_mission_agl,
                   photogrammetry_outputs_path = photogrammetry_outputs_path)

missions_agl = bind_rows(missions_agl)
missions_agl
