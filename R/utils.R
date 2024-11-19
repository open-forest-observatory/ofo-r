# Create a directory if it doesn't exist
#' @export
create_dir <- function(dir) {
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE)
  }
}

drop_units_if_present = function(x) {
  if (inherits(x, "units")) {
    return(x |> units::drop_units())
  } else {
    return(x)
  }
}