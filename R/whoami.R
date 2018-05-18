#' Get name the executing script file
#'
#' Utility function that, when used with Rscript, detects the file
#' running and returns a vector the basename without extension.
#'
#' @return String of basename.
#'
#' @export
whoami <- function() {

  # Return nothing if running interactively
  if (interactive()) return(NULL)

  # Get command line arguments and find the filename
  cmd_args <- commandArgs(trailingOnly = FALSE)
  file_arg <- "--file="
  path <- sub(file_arg, "", cmd_args[grep(file_arg, cmd_args)])
  name <- basename(path)
  name <- gsub("\\..+", "", name)
  return(name)
}
