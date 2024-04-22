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


datadir = readLines("sandbox/data-dirs/derek-fieldref-laptop.txt", n = 1)

plot_boundaries_data_dir = file.path(datadir, "field-plot-boundaries")
google_sheet_id = "1GjDseDCR1BX_EIkJAni7rk2zvK6nHmZz1nOFBd1d6k4"

BASE_OFO_URL = "https://openforestobservatory.netlify.app/"
BASE_OFO_URL = "localhost:1313/"
WEBSITE_REPO_PATH = "~/repos/ofo-website-3/"

# Path to the plot details template page within theis repo
PLOT_DETAILS_TEMPLATE_FILEPATH = fs::path(file.path("sandbox", "field-ref-data", "templates", "field-ref-plot-details.md"))

# Path to plot details dir relative to the 'content' dir in the website repo. No leading slach but
# trailing slash
PLOT_DETAILS_PAGE_DIR = "data-field-ref-plot-details/"

# Path to static website files dirs within website repo relative to 'static'. Leading slash but no
# trailing slash
DATATABLE_HEADER_FILES_DIR = "/datatable-header-files"
LEAFLET_HEADER_FILES_DIR = "/leaflet-header-files"
PLOT_CATALOG_DATATABLE_DIR = "/field-plot-catalog-datatable/"
PLOT_CATALOG_DATATABLE_FILENAME = "field-plot-catalog-datatable.html"
PLOT_CATALOG_MAP_DIR = "/field-plot-catalog-map/"
PLOT_CATALOG_MAP_FILENAME = "field-plot-catalog-map.html"
PLOT_DETAILS_DATATABLE_DIR = "/field-plot-details-datatables"
PLOT_DETAILS_MAP_DIR = "/field-plot-details-maps"

WEBSITE_STATIC_PATH = file.path(WEBSITE_REPO_PATH, "static", "")
WEBSITE_CONTENT_PATH = file.path(WEBSITE_REPO_PATH, "content", "")


# --- Notes on data issues:
# - tree 4310 (VP dataset) has `crown_position` of 7, which is nonsensical and translates to ohvis
#   of 7
# - ******* Johnston plot cleaning

# --- TODO:
# - Options to color markers by project? Plot size? Etc. https://stackoverflow.com/questions/47966375/how-to-create-leaflet-markers-colored-by-a-numeric-variable

# ---- Processing





# Load and prep field ref data

tabular_data = read_and_standardize_tabular_field_ref_data(google_sheet_id)
bounds = read_and_merge_plot_boundaries(plot_boundaries_data_dir)

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
                      website_static_path = WEBSITE_STATIC_PATH,
                      leaflet_header_files_dir = LEAFLET_HEADER_FILES_DIR,
                      plot_catalog_map_dir = PLOT_CATALOG_MAP_DIR,
                      plot_catalog_map_filename = PLOT_CATALOG_MAP_FILENAME)

reset_plot_detail_dirs(WEBSITE_STATIC_PATH,
                       WEBSITE_CONTENT_PATH,
                       PLOT_DETAILS_PAGE_DIR,
                       PLOT_DETAILS_MAP_DIR,
                       PLOT_DETAILS_DATATABLE_DIR)

trees_vis = prep_trees_for_stem_map(trees_for_plot_summary, plot_summary)



## Loop through each plot and make a details page, including its media (map and datatable)

plot_ids = plot_summary$plot_id
nplots = length(plot_ids)

for(plot_id_foc in plot_ids) {

  cat("\rGenerating details page for plot ", plot_id_foc, "of", nplots, "    ")

  plot_summary_foc = plot_summary |>
    filter(plot_id == plot_id_foc)

  bound_foc = bounds |>
    filter(plot_id == plot_id_foc)

  trees_foc = trees_vis |>
    filter(plot_id == plot_id_foc)

  plot_details_map_path = make_plot_details_map(plot_summary_foc = plot_summary_foc,
                            bound_foc = bound_foc,
                            trees_foc = trees_foc,
                            website_static_path = WEBSITE_STATIC_PATH,
                            leaflet_header_files_dir = LEAFLET_HEADER_FILES_DIR,
                            plot_details_map_dir = PLOT_DETAILS_MAP_DIR)

  plot_details_datatable_path = make_plot_details_datatable(plot_summary_foc = plot_summary_foc,
                                  website_static_path = WEBSITE_STATIC_PATH,
                                  datatable_header_files_dir = DATATABLE_HEADER_FILES_DIR,
                                  plot_details_datatable_dir = PLOT_DETAILS_DATATABLE_DIR)


  # Render plot details page from template
  render_plot_page(template_filepath = PLOT_DETAILS_TEMPLATE_FILEPATH,
                  plot_summary_foc = plot_summary_foc,
                  plot_details_map_path = plot_details_map_path,
                  plot_details_datatable_path = plot_details_datatable_path,
                  website_repo_content_path = WEBSITE_CONTENT_PATH,
                  plot_details_page_dir = PLOT_DETAILS_PAGE_DIR)
}



















## OLD: extract datatable headers

# # List table supporting files and copy to website repo static folder

# table_files_rel = list.files(file.path(tabledir, "table_files"), include.dirs = TRUE)
# table_files_abs = file.path(tabledir, "table_files", table_files_rel)

# file.copy(table_files_abs,
#           FIELDREF_PLOT_DATA_TABLE_SUPPORTING_FILES_DIR, recursive = TRUE)

# # Extract the table HTML header that links in these files, and sub in the new paths

# table_html = readLines(tablepath)
# first_line = grep("<style>", table_html)[1]
# last_line = grep("</style>", table_html)[1]




# # Extract the datatable HTML header that links in these files, and sub in the new paths
# THIS IS NOW OBSOLETE because embedding the entire table HTML as an iframe

# dt_html = readLines(tablepath)
# dt_html_header = dt_html[grep("<head>", dt_html)[1]:grep("</head>", dt_html)[1]]
# header_file_line_idxs = grep("(<link)|(<script)", dt_html_header)
# header_file_lines = dt_html_header[header_file_line_idxs]

# header_file_lines_updated = header_file_lines |>
#   str_replace_all("table_files", "/datatable-header-files") |>
#   str_replace_all("table_files", "/datatable-header-files") |>
#   paste(collapse = "\n")
# # ^ these lines are now ready to write into the markdown templates

