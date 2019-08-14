
.check_system <- function(package) {
  out <- suppressWarnings(system(glue::glue("which {package}"), intern = TRUE))

  if (length(out) == 0) {
    stop("Pacakge ", package," not installed.")
  }
}


.check_folder <- function(dir) {

}
