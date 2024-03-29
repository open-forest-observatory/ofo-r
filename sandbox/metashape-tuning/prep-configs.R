devtools::load_all()

datadir = readLines("sandbox/data-dirs/js2-metashape-tuning.txt")



scenarios = expand.grid("filterPointsUSGS__enabled" = c(FALSE),
                        "alignPhotos__downscale" = c(2, 4),
                        "buildDepthMaps__downscale" = c(2, 4),
                        "buildDepthMaps__filter_mode" = c("Metashape.MildFiltering", "Metashape.AggressiveFiltering"),
                        "buildDepthMaps__max_neighbors" = c(60),
                        "buildModel__face_count" = c("Metashape.LowFaceCount", "Metashape.HighFaceCount"))
name = paste("cfg", stringr::str_pad(1:nrow(scenarios), 3, pad = "0"), sep = "")
scenarios$photo_path = "/ofo-share/metashape-tuning/raw-images-subset/delta2/"
scenarios$config_filename = paste0("tuning02d2_", name)
scenarios01 = scenarios

base_yaml_filepath = file.path(getwd(), "sandbox", "metashape-tuning", "configs", "02d2", "base.yml")

make_derived_configs(base_yaml_filepath, scenarios01,
                     metashape_path = "/ofo-share/utils/automate-metashape/python/metashape_workflow.py")
