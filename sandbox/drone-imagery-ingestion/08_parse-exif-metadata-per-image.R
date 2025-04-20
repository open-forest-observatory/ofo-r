# Purpose: Read a CSV file containing the EXIF data for all images and extract standardized
# metadata. The sensor/platform is used to correctly interpret the metadata conventions. This comes
# from the contributed metadata (from Baserow), which is the only reason the contributed metadata is
# being read.

# sf needs to be included or else the bind_rows call to create polygon_perdataset fails. I don't
# understand the concept of dispatching and why this is required.
library(sf)
library(tidyverse)
library(ofo)
library(furrr)

## Constants

# In
RAW_EXIF_PATH = "/ofo-share/drone-imagery-organization/metadata/2_intermediate/3_raw-exif"
CONTRIBUTED_METADATA_PER_SUB_MISSION_PATH = "/ofo-share/drone-imagery-organization/metadata/2_intermediate/2_contributed-metadata-per-sub-mission/"
MISSIONS_TO_PROCESS_LIST_PATH = file.path("sandbox", "drone-imagery-ingestion", "missions-to-process.csv")

# Out
PARSED_EXIF_METADATA_PATH = "/ofo-share/drone-imagery-organization/metadata/2_intermediate/4_parsed-exif"


## Functions

# Note that `exif` will be defined in the global environment
extract_sub_mission_exif = function(unique_sub_mission_id) {
  # Extract the exif rows matching that sub-mission ID
  sub_mission_exif <- exif |>
    filter(sub_mission_id == unique_sub_mission_id)
  return(sub_mission_exif)
}

# Note that `baserow_sub_mission_metadata` will be defined in the global environment
extract_aircraft_model_name = function(unique_sub_mission_id) {
  # Extract the baserow entries for this sub-mission ID
  baserow_for_sub_mission <- baserow_sub_mission_metadata[
    baserow_sub_mission_metadata$sub_mission_id == unique_sub_mission_id,
  ]

  # There should only be one matching row
  if (nrow(baserow_for_sub_mission) == 1) {
    # Extract the aircraft model name
    aircraft_model_name = baserow_for_sub_mission$aircraft_model_name
  } else {
    warning(paste("There were ", nrow(baserow_for_sub_mission), " rows in baserow matching ", unique_sub_mission_id))
    # Default value indicating a single corresponding entry could not be found
    aircraft_model_name = NaN
  }

  return(aircraft_model_name)
}


## Workflow

# Determine which missions to process
missions_to_process = read_csv(MISSIONS_TO_PROCESS_LIST_PATH) |>
  pull(mission_id)

# Run for each mission_id

parse_mission_exif_at_image_level = function(mission_id_foc) {

  # For tracking down the mission ID(s) that produces warnings when this function is called inside a
  # map() function, you can include this line and see which mission ID warning was printed just
  # before the real warning. warning(paste("Parsing EXIF for mission ID:", mission_id_foc))

  # Input data
  exif_filepath = file.path(RAW_EXIF_PATH, paste0(mission_id_foc, ".csv"))
  exif = read_csv(exif_filepath)

  # All sub-mission contributed metadata files (at the sub-mission level) for the focal mission, to
  # obtain the aircraft model name
  pattern = paste0(mission_id_foc, "-[0-9]{2}.csv")
  files = list.files(
    path = CONTRIBUTED_METADATA_PER_SUB_MISSION_PATH,
    pattern = pattern,
    full.names = TRUE
  )
  contrib_metadata_list = purrr::map(files, read_csv)
  contrib_sub_mission_metadata = bind_rows(contrib_metadata_list)

  # Output filepath
  metadata_perimage_filepath = file.path(PARSED_EXIF_METADATA_PATH, paste0(mission_id_foc, ".csv"))


  # Compute the unique sub mission IDs. The metadata is extracted per-image, but the sub-mission
  # gives us information about the platform/sensor that is neccessary for interpreting the raw
  # metadata
  unique_sub_mission_ids = unique(exif$sub_mission_id)

  # Merge the aircraft model names from the contributed metadata into the exif data
  contrib_sub_mission_metadata_simp = contrib_sub_mission_metadata |>
    select(sub_mission_id, aircraft_model_name)
  exif = left_join(
    exif,
    contrib_sub_mission_metadata_simp,
    by = "sub_mission_id"
  )

  # Only retain sub-missions for which there was valid baserow data identifying the aircraft model,
  # but give a warning if there are any missing aircraft model names, where the sub-mission ID without
  # the name is printed. This may be unnecessary because the extract_imagery_perimage_metadata
  # function errors if an unsupported aircraft model name is passed to it (need to confirm this is
  # also the case for NA values).

  exif_with_no_aircraft_model_name = exif |>
    filter(is.na(aircraft_model_name))
  sub_mission_ids_with_no_aircraft_model_name = unique(exif_with_no_aircraft_model_name$sub_mission_id)
  if (length(sub_mission_ids_with_no_aircraft_model_name) > 0) {
    warning(paste("The following sub-mission IDs had no aircraft model name in the contributed metadata: ",
      paste(sub_mission_ids_with_no_aircraft_model_name, collapse = ", "),
      ". Skipping."
    ))
    # Remove the rows with no aircraft model name
    exif = exif |>
      filter(!is.na(aircraft_model_name))
  }


  # Previous code where we split the exif data by sub-mission (potentially different platforms) and
  # then extracted the metadata per image. This would be useful if we go back to the approach where
  # the metadata extraction is not vectorized acros a vector aircraft platforms (one platform per
  # image), but instead is passed a single aircraft platform per exif dataframe.
  #
  # # For parallelizing, make a list of subsets of the exif dataframe, one for each sub-mission
  # exif_per_sub_mission <- lapply(
  #   unique_sub_mission_ids,
  #   extract_sub_mission_exif
  # )

  # # Get aircraft model name to use for determining the appropriate image-level metadata extractor
  # # functions to use
  # aircraft_model_names <- lapply(
  #   unique_sub_mission_ids,
  #   extract_aircraft_model_name
  # )
  # # Extract the metadata in a standardized manner no matter the platform
  # # This is run on a per-image basis, but requires the per-sub-mission platform name to understand how
  # # to interpret the information.
  # future::plan("future::multisession")
  # print("Started extracting metadata per image to a standardized format")
  # metadata_per_sub_mission = furrr::future_map2(
  #   .x = exif_per_sub_mission,
  #   .y = aircraft_model_names,
  #   .f = extract_imagery_perimage_metadata,
  #   .progress = TRUE,
  #   .options = furrr::furrr_options(seed = TRUE)
  # )
  # # Bind the rows together across all sub-missions
  # metadata_perimage = bind_rows(metadata_per_sub_mission)
  # # Ensure that the exif data is ordered in the same way and any dropped sub-missions are reflected
  # exif = bind_rows(exif_per_sub_mission)

  # Extract the metadata in a standardized manner no matter the platform
  metadata_perimage = extract_imagery_perimage_metadata(
    exif,
    platform_name = exif$aircraft_model_name
  )

  # Copy the sub-mission and mission ID to the output metadata
  metadata_perimage$sub_mission_id = exif$sub_mission_id
  metadata_perimage$mission_id = exif$mission_id
  # Pad the mission ID since it may start as an integer (not the case for the sub-mission ID since it
  # contains a dash and thus never gets misinterpreted as an int)
  metadata_perimage = metadata_perimage |> mutate(mission_id = str_pad(mission_id, 6, pad = "0", side = "left"))

  # Create the folder to save the image metadata in
  create_dir(dirname(metadata_perimage_filepath))
  # Write out the standardized metadata per image
  write_csv(metadata_perimage, metadata_perimage_filepath)
  return(TRUE)
}

# Run the function for each mission
future::plan("multisession")
furrr::future_walk(missions_to_process, parse_mission_exif_at_image_level)
