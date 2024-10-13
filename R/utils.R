# Utility function for creating directory if it doesn't exist. Used in all 10 .R files
create_directory <- function(folder) {
  if (!dir.exists(folder)) {
    dir.create(folder, recursive = TRUE)
  }
}
