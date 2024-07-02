# Create a directory if it doesn't exist
create_dir <- function(dir) {
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE)
  }
}