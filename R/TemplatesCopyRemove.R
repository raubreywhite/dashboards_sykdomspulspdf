#' hi
#' @param output_dir a
#' @param SYNDROM a
#' @export sykdompulspdf_template_copy
sykdompulspdf_template_copy <- function(output_dir, SYNDROM) {
  dir <- system.file("extdata", package = "sykdomspulspdf")
  files <- list.files(dir, pattern = "Rmd*")
  for (f in files) {
    file.copy(
      from = file.path(dir, f),
      to = file.path(output_dir, f),
      overwrite = T
    )
  }
}


#' hi
#' @param output_dir a
#' @param SYNDROM a
#' @export sykdompulspdf_template_remove
sykdompulspdf_template_remove <- function(output_dir, SYNDROM) {
  dir <- system.file("extdata", package = "sykdomspulspdf")
  files <- list.files(dir, pattern = "Rmd*")
  for (f in files) {
    file.remove(file.path(output_dir, f))
  }
}


