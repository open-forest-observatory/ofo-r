# Purpose: Map the locations of field reference data plots, and make a table summarizing the data

devtools::load_all()
library(tidyverse)
library(sf)
library(readxl)

datadir = readLines("sandbox/data-dirs/derek-fieldref-laptop.txt")


field_projects = read_excel(file.path(datadir, "field-reference-data.xlsx"), sheet = "field-projects")
field_plots = read_excel(file.path(datadir, "field-reference-data.xlsx"), sheet = "field-plots")
field_trees = read_excel(file.path(datadir, "field-reference-data.xlsx"), sheet = "field-trees")

# TODO: check warnings on file read