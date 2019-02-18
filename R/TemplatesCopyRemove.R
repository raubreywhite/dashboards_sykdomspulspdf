#' hi
#' @param output_dir a
#' @param SYNDROM a
#' @export sykdompulspdf_template_copy
sykdompulspdf_template_copy <- function(output_dir, SYNDROM) {
  dir <- system.file("extdata", package = "sykdomspulspdf")
  files <- list.files(dir, pattern = paste("monthly_report_", SYNDROM, ".Rmd", sep = ""))
  for (f in files) {
    file.copy(
      from = file.path(dir, f),
      to = file.path(output_dir, f)
    )
  }
}


#' hi
#' @param output_dir a
#' @param SYNDROM a
#' @export sykdompulspdf_template_copy_ALL
sykdompulspdf_template_copy_ALL <- function(output_dir, SYNDROM) {
  dir <- system.file("extdata", package = "sykdomspulspdf")
  files1 <- list.files(dir, pattern = paste("monthly_report_", SYNDROM, "ALL.Rmd", sep = ""))
  files2 <- list.files(dir, pattern = paste("child", SYNDROM, ".Rmd", sep = ""))

  for (f in files1) {
    file.copy(
      from = file.path(dir, f),
      to = file.path(output_dir, f)
    )
  }

  for (f in files2) {
    file.copy(
      from = file.path(dir, f),
      to = file.path(output_dir, f)
    )
  }

}

#' hi
#' @param output_dir a
#' @param SYNDROM a
#' @export sykdompulspdf_template_remove
sykdompulspdf_template_remove <- function(output_dir, SYNDROM) {
  dir <- system.file("extdata", package = "sykdomspulspdf")
  files <- list.files(dir, pattern = paste("monthly_report_", SYNDROM, ".Rmd", sep = ""))
  for (f in files) {
    file.remove(file.path(output_dir, f))
  }
}


#' hi
#' @param output_dir a
#' @param SYNDROM a
#' @export sykdompulspdf_template_remove_ALL
sykdompulspdf_template_remove_ALL <- function(output_dir, SYNDROM) {
  dir <- system.file("extdata", package = "sykdomspulspdf")
  files1 <- list.files(dir, pattern = paste("monthly_report_", SYNDROM, "ALL.Rmd", sep = ""))
  files2 <- list.files(dir, pattern = paste("child", SYNDROM, ".Rmd", sep = ""))

  for (f in files1) {
    file.remove(file.path(output_dir, f))
  }

  for (f in files2) {
    file.remove(file.path(output_dir, f))
  }

}
