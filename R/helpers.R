
.check_system <- function(package) {
  out <- suppressWarnings(system(glue::glue("which {package}"), intern = TRUE))

  if (length(out) == 0) {
    stop("Pacakge ", package," not installed.")
  }
}


<<<<<<< HEAD
.pattern_python_to_r <- function(pattern) {
=======
.pattern_python_to_r <- function(x) {
>>>>>>> 40df7796b850ef3f41539b2e8a1ad95fbb200c73
  x <- gsub("%(", "{", x, fixed = TRUE)
  x <- gsub(")s", "}", x, fixed = TRUE)
  x
}
