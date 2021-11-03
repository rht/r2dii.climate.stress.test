outputs_path <- function() {
  fs::path(Sys.getenv("ST_PROJECT_FOLDER"), "outputs")
}

dir_empty <- function(dir) {
  if (fs::dir_exists(dir)) fs::dir_delete(dir)
  fs::dir_create(dir)
}

is_empty_dir <- function(dir) {
  identical(fs::dir_ls(dir), character(0))
}

ls_results <- function(dir) {
  fs::dir_ls(dir, regexp = "stress_test_results.*[.]csv$")
}

is_mauro <- function() {
  identical(as.character(fs::path_home()), "/home/mauro")
}
