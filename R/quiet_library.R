#' Quietly load package
#'
#' Utility function for suppressing as much console output as
#' possible when calling `library`, used mainly in scripts that
#' are meant to be run in batch with Rscript.
#'
#' @param pkgs Character vector of packages.
#'
#' @family utility functions
#' @export
quiet_library <- function(pkgs) {
  for (pkg in pkgs) {
    suppressPackageStartupMessages(
      library(pkg, quietly = TRUE,
              character.only = TRUE,
              warn.conflicts = FALSE)
    )
  }
}
