# Purpose: Create the Hugo markdown pages for all field reference sites, as well as a dataset index
# page

library(dplyr)
library(googlesheets4)
library(leaflet)
library(stringr)
library(sf)
library(htmlwidgets)
library(DT)
library(jinjar)

devtools::load_all()


PLOT_BOUNDARIES_PATH = "~/Documents/repo-data-local/ofo-field/field-plot-boundaries"
GOOGLE_SHEET_ID = "1GjDseDCR1BX_EIkJAni7rk2zvK6nHmZz1nOFBd1d6k4"

BASE_OFO_URL = "https://openforestobservatory.org/"
#BASE_OFO_URL = "http://localhost:1313/"
WEBSITE_REPO_PATH = "~/repos/ofo-website-3/"

# Path to the plot details template page within theis repo
PLOT_DETAILS_TEMPLATE_FILEPATH = fs::path(file.path("sandbox", "ground-ref-data", "templates", "ground-ref-plot-details.md"))

# Path to plot details dir relative to the 'content' dir in the website repo. No leading slash but
# trailing slash
PLOT_DETAILS_PAGE_DIR = "data/ground-ref/plot-details/"

# Path to static website files dirs within website repo relative to 'static'. Leading slash but no
# trailing slash
DATATABLE_HEADER_FILES_DIR = "/datatable-header-files"
LEAFLET_HEADER_FILES_DIR = "/leaflet-header-files"
PLOT_CATALOG_DATATABLE_DIR = "/ground-plot-catalog-datatable/"
PLOT_CATALOG_DATATABLE_FILENAME = "ground-plot-catalog-datatable.html"
PLOT_CATALOG_MAP_DIR = "/ground-plot-catalog-map/"
PLOT_CATALOG_MAP_FILENAME = "ground-plot-catalog-map.html"
PLOT_DETAILS_DATATABLE_DIR = "/ground-plot-details-datatables"
PLOT_DETAILS_MAP_DIR = "/ground-plot-details-maps"

WEBSITE_STATIC_PATH = file.path(WEBSITE_REPO_PATH, "static", "")
WEBSITE_CONTENT_PATH = file.path(WEBSITE_REPO_PATH, "content", "")


# --- Notes on data issues:
# - tree 4310 (VP dataset) has `crown_position` of 7, which is nonsensical and translates to ohvis
#   of 7
# - ******* Johnston plot cleaning

# --- TODO:
# - Options to color markers by project? Plot size? Etc.:
#   https://stackoverflow.com/questions/47966375/how-to-create-leaflet-markers-colored-by-a-numeric-variable
# - Make sure plots with zero trees are not being removed



# ---- Processing

# Load and prep field ref data

tabular_data = read_and_standardize_tabular_field_ref_data(GOOGLE_SHEET_ID)
bounds = read_and_merge_plot_boundaries(PLOT_BOUNDARIES_PATH, base_ofo_url = BASE_OFO_URL, plot_details_dir = PLOT_DETAILS_PAGE_DIR) 

check_field_ref_data(tabular_data, bounds)

trees = prep_trees(tabular_data$trees, tabular_data$species_codes)
plots = prep_plots(tabular_data$plots)

trees_for_plot_summary = prep_trees_for_plot_summary(trees)

plot_level_tree_summary = summarize_trees_by_plot(trees_for_plot_summary)


# Save header library files required by embedded HTML datatables and leaflet maps
save_dt_header_files(WEBSITE_STATIC_PATH, DATATABLE_HEADER_FILES_DIR)
save_leaflet_header_files(WEBSITE_STATIC_PATH, LEAFLET_HEADER_FILES_DIR)


plot_summary = compile_plot_summary_table(plots = plots,
                                          projects = tabular_data$projects,
                                          plot_level_tree_summary = plot_level_tree_summary,
                                          bounds = bounds,
                                          base_ofo_url = BASE_OFO_URL,
                                          plot_details_dir = PLOT_DETAILS_PAGE_DIR)


# Imprecise test for whether the data is still in the process of entry and should be skipped. TODO:
# Consider turning into a function
plot_summary = plot_summary |>
  dplyr::filter(!is.na(plot_id) & !is.na(survey_date))

# Write the plot-level data to a csv (for use otside the website purposes)
# readr::write_csv(plot_summary, FILE_PATH_HERE)

# Make a HTML data table of plot catalog
dt = make_plot_catalog_datatable(plot_summary = plot_summary,
                            website_static_path = WEBSITE_STATIC_PATH,
                            datatable_header_files_dir = DATATABLE_HEADER_FILES_DIR,
                            plot_catalog_datatable_dir = PLOT_CATALOG_DATATABLE_DIR,
                            plot_catalog_datatable_filename = PLOT_CATALOG_DATATABLE_FILENAME)

# Make leaflet map of field data catalog
plot_centroids = sf::st_centroid(bounds)
m = make_plot_catalog_map(plot_summary = plot_summary,
                      plot_centroids = plot_centroids,
                      plot_bounds = bounds,
                      website_static_path = WEBSITE_STATIC_PATH,
                      leaflet_header_files_dir = LEAFLET_HEADER_FILES_DIR,
                      plot_catalog_map_dir = PLOT_CATALOG_MAP_DIR,
                      plot_catalog_map_filename = PLOT_CATALOG_MAP_FILENAME)

# For website directories that house plot-level page components, delete existing directories and
# create new empty directory
reset_detail_dirs(WEBSITE_STATIC_PATH,
                       WEBSITE_CONTENT_PATH,
                       PLOT_DETAILS_PAGE_DIR,
                       PLOT_DETAILS_MAP_DIR,
                       PLOT_DETAILS_DATATABLE_DIR)

trees_vis = prep_trees_for_stem_map(trees_for_plot_summary, plot_summary)


make_plot_details_pages(plot_summary = plot_summary,
                   bounds = bounds,
                   trees_vis = trees_vis,
                   website_static_path = WEBSITE_STATIC_PATH,
                   leaflet_header_files_dir = LEAFLET_HEADER_FILES_DIR,
                   datatable_header_files_dir = DATATABLE_HEADER_FILES_DIR,
                   plot_details_datatable_dir = PLOT_DETAILS_DATATABLE_DIR,
                   plot_details_map_dir = PLOT_DETAILS_MAP_DIR,
                   plot_details_template_dir = PLOT_DETAILS_TEMPLATE_FILEPATH,
                   plot_details_page_dir = PLOT_DETAILS_PAGE_DIR)
