#' Get name the executing script file
#'
#' Utility function that tries to detecs the file being run,
#' either with Rscript or with `source`, and returns the
#' basename without file extension.
#' If the function is not called as a result of a file being
#' run, it throws a warning and returns an empty string.
#'
#' @return String of basename.
#'
#' @export
whoami <- function() {

  # Return nothing if running interactively
  # if (interactive()) return(NULL)

  # Get command line arguments and find the filename
  # or get it from the env if file is sourced
  cmd_args <- commandArgs(trailingOnly = FALSE)
  file_arg <- "--file="
  if (length(m <- grep(file_arg, cmd_args)) > 0L) {
    path <- sub(file_arg, "", cmd_args[m])
  } else {
      path <- try(get("filename", envir = sys.frame(1L)),
                  silent = TRUE)
  }

  # Return nothing if a file name could not be found
  if (inherits(path, "try-error")) {
    warning("Filename not detected. Did you run this function interactively?")
    return(character(0L))
  }

  # Strip path and file extension
  name <- basename(path)
  name <- gsub("\\..+", "", name)
  return(name)
}
