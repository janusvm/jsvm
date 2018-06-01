#' Translate R expression into tikz-qtree and save figure
#'
#' Plots the abstract syntax tree of an expression in a pdf, png, or svg file.
#' The qtree string assumes the node styles \code{call}, \code{sym}, and
#' \code{lit} to exist (call, symbol, and literal nodes, respectively).
#' See below for system requirements.
#'
#' This function is heavily inspired by the abstract syntax tree figures in
#' Hadley Wickham's \href{https://adv-r.hadley.nz}{Advanced R},
#' Section 20 (Expressions), and the tikz styles are made to closely mimic the
#' style of those figures.
#' The code itself is adapted from the function \link[lobstr]{ast}.
#'
#' @section System requirements:
#' In order to render the tree, the following components are required to be installed
#' on the system:
#'
#' For pdf figures, \code{pdflatex} must be installed (e.g. with \href{https://www.tug.org/texlive/}{TeXLive}
#' or \href{https://miktex.org/}{MiKTeX}) and on the PATH, and the following packages must be installed:
#' \itemize{
#'   \item fontenc
#'   \item xcolor
#'   \item tikz
#'   \item tikz-qtree
#'   \item inconsolata
#' }
#'
#' For png figures, the requirements for pdf figures must be met, and in addition, \code{convert} from the
#' \href{https://www.imagemagick.org/script/index.php}{ImageMagick} suite must be installed and on the PATH.
#'
#' For svg figures, the requirements for pdf figures must be met, and in addition, \code{pdf2svg} must
#' be installed and on the PATH.
#'
#' @param expr expression
#'
#'   NB: the current version can't handle named arguments, neither in function calls nor definitions.
#' @param filename file name with extension (either .png, .svg, or .pdf) or NULL for not
#'   saving an output file.
#' @param path subdirectory in which to save the figure
#' @param dpi image density, only relevant for png output
#' @param keep_tex should the .tex file be saved as well?
#'
#'   This argument is ignored if filename is NULL.
#'
#' @return qtree string, invisibly.
#'   If a filename was provided, it will have node styles, otherwise it will be
#'   as plain as possible and printed to the console.
#' @examples
#' expr2tikz(y <- 2 * x, "fig.png")
#'
#' @export
expr2tikz <- function(expr, filename = NULL, path = getwd(), dpi = 600, keep_tex = FALSE) {

  # Generate qtree string
  make_outfile <- !is.null(filename)
  expr <- rlang::enexpr(expr)
  symlist <- .subexpr2tikz(expr, make_outfile)
  tree <- paste("\\Tree", .enbracket(symlist, make_outfile))

  # Check if outfile is wanted and possible
  if (!make_outfile) {
    cat(tree)
    return(invisible(tree))
  }
  ext <- tools::file_ext(filename)
  if (!ext %in% c("pdf", "png", "svg"))
    stop("file extension must be either pdf, png, or svg.")
  reqs <- switch (ext,
    pdf = "pdflatex",
    png = c("pdflatex", "convert"),
    svg = c("pdflatex", "pdf2svg")
  )
  if (any(nchar(Sys.which(reqs)) == 0L))
    stop(paste(reqs, collapse = " and "), " must be on system PATH in order to produce ",
         ext, " figures.")

  # Generate output in temporary folder
  texfile <- tempfile(fileext = ".tex")
  pdffile <- sub(".tex", ".pdf", texfile, fixed = TRUE)
  template <- system.file(file.path("extdata", "ast-template.tex"), package = "jsvm")
  tex <- readLines(template)
  tex <- sub("<TREE_STRING>", tree, tex, fixed = TRUE)
  writeLines(tex, texfile)
  failed <- system2("pdflatex", c("-interaction=nonstopmode", "-halt-on-error",
                                  paste("-output-directory", tempdir()), texfile),
                    stdout = NULL)
  if (failed) stop("pdflatex had exit status ", failed,
                   ". See the System requirements section of ?expr2tikz for more info.")

  # If the outfile is to be a png or svg, make it
  if (ext == "png") {
    pngfile <- sub(".tex", ".png", texfile, fixed = TRUE)
    system2("convert", c("-density", dpi, "-bordercolor white",
                         "-border 20", pdffile, pngfile))
    getfile <- pngfile
  } else if (ext == "svg") {
    svgfile <- sub(".tex", ".svg", texfile, fixed = TRUE)
    system2("pdf2svg", c(pdffile, svgfile))
    getfile <- svgfile
  } else {
    getfile <- pdffile
  }

  # Copy the desired files to specified file location
  file.copy(getfile, file.path(path, filename), overwrite = TRUE)
  if (keep_tex) {
    texout <- sub("\\.[[:alpha:]]{3}", "\\.tex", filename)
    file.copy(texfile, file.path(path, texout), overwrite = TRUE)
  }
  invisible(tree)
}


.subexpr2tikz <- function(x, style = TRUE) {
  if (rlang::is_quosure(x)) x <- rlang::quo_expr(x)
  if (rlang::is_syntactic_literal(x)) {
    if (is.character(x)) return(encodeString(x, quote = "\""))
    else if (style) return(paste0("\\node[lit] {", x, "};"))
    else return(paste0("{", x, "}"))
  }
  else if (rlang::is_symbol(x)) {
    x <- as.character(x)
    if (!make.names(x) == x) {
      if (style) return(paste0("\\node[sym] {\\`{}", .escapeLaTeX(x), "\\`{}};"))
      else return(paste0("{", encodeString(x, quote = "`"), "}"))
    } else if (style) return(paste0("\\node[sym] {", .escapeLaTeX(x), "};"))
    else return(paste0("{", x, "}"))
  }
  else if (!is.pairlist(x) && !is.call(x)) {
    return(paste0("<inline ", paste0(class(x), collapse = "/"), ">"))
  }

  # TODO: add support for named function arguments
  lapply(x, .subexpr2tikz, style = style)
}


.enbracket <- function(x, style = TRUE) {
  if (length(x) == 1L && !is.list(x)) {
    return(x)
  } else {
    sub <- lapply(x, .enbracket, style = style)
    if (style) return(paste("[.\\node[call] {};", paste(sub, collapse = " "), "]"))
    else return(paste("[.{<call>}", paste(sub, collapse = " "), "]"))
  }
}


.escapeLaTeX <- function(x) {
  x <- gsub("}", "\\}", x, fixed = TRUE)
  x <- gsub("{", "\\{", x, fixed = TRUE)
  x <- gsub("$", "\\$", x, fixed = TRUE)
  x <- gsub("%", "\\%", x, fixed = TRUE)
  x <- gsub("_", "\\_", x, fixed = TRUE)
  x <- gsub("^", "\\^{}", x, fixed = TRUE)
  x <- gsub("~", "\\~{}", x, fixed = TRUE)
  x
}