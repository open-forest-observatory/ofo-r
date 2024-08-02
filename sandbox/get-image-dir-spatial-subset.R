library(sf)
# devtools::document("/ofo-share/repos-derek/ofo-r")
devtools::load_all("/ofo-share/repos-derek/ofo-r")

FULL_DATASET_DIR = "/ofo-share/drone-imagery-organization/2_sorted-notcleaned/2021-tnc-yuba/000450-01/00"
SUBSET_DATASET_DIR = "/ofo-share/scratch-derek/conceptual-fig-sfm/raw-images/unit1/"

SUBSET_BOUNDS = "/ofo-share/scratch-derek/conceptual-fig-sfm/bounds/unit1_foc.gpkg"

# Set the number of levels to preserve in the file path (1 for filename only) when copying images
N_LEVELS_TREE_PRESERVE = 1


# for prototyping
source_dir = FULL_DATASET_DIR
dest_dir = SUBSET_DATASET_DIR
subset_bounds_filepath = SUBSET_BOUNDS
n_levels_tree_preserve = N_LEVELS_TREE_PRESERVE

spatial_subset_image_dir = function(source_dir,
                                    dest_dir,
                                    subset_bounds_filepath,
                                    n_levels_tree_preserve = 1) {

  image_filepaths = list.files(source_dir, pattern = ".JPG$", full.names = TRUE, recursive = TRUE)

  exif = ofo::read_exif_drop_thumbnails(image_filepaths)

  exif_sp = ofo::prep_exif(exif)

  subset_bounds = sf::st_read(subset_bounds_filepath)

  subset_bounds = sf::st_transform(subset_bounds, st_crs(exif_sp))

  exif_subset = sf::st_intersection(exif_sp, subset_bounds)



  # Copy subsetted images (as hardlinks)

  from_filepath_parts = strsplit(exif_subset$SourceFile, "/")
  path_preserve = sapply(from_filepath_parts,
                         function(x) paste0(x[(length(x) - n_levels_tree_preserve + 1):length(x)],
                         collapse = "/"))
  
  dest_filepath = paste0(dest_dir, path_preserve)

  # Create dirs if they don't exist
  dirs = unique(dirname(dest_filepath))

  for(dir in dirs) {
    if(!dir.exists(dir)) {
      dir.create(dir, recursive = TRUE)
    }
  }

  file.link(from = exif_subset$SourceFile, to = dest_filepath)
}


spatial_subset_image_dir(FULL_DATASET_DIR, SUBSET_DATASET_DIR, SUBSET_BOUNDS, N_LEVELS_TREE_PRESERVE)

