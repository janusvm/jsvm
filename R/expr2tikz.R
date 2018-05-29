#' Translate R expression into tikz-qtree and save figure
#'
#' Plots the abstract syntax tree of an expression in a pdf or png file.
#' Requires pdflatex and convert (from ImageMagick) to be on the system PATH.
#' The qtree string assumes the node styles \code{call}, \code{sym}, and \code{lit} to exist
#' (call, symbol, and literal nodes, respectively).
#'
#' @param expr Expression
#' @param filename File name with extension (either .png or .pdf) or NULL for not
#'   saving an output file.
#' @param path Subdirectory in which to save the figure (default: working directory)
#' @param dpi Image density, only relevant for png output (default: 600)
#'
#' @return qtree string, invisibly.
#'   If a filename was provided, it will have node styles, otherwise it will be
#'   as plain as possible.
#' @examples
#' expr2tikz(y <- 2 * x, "fig.png")
#'
#' @export
expr2tikz <- function(expr, filename = NULL, path = getwd(), dpi = 600) {

  # Generate qtree string
  make_outfile <- !is.null(filename)
  expr <- rlang::enexpr(expr)
  symlist <- .subexpr2tikz(expr, make_outfile)
  tree <- paste("\\Tree", .enbracket(symlist, make_outfile))

  # Check if outfile is wanted and possible
  if (is.null(filename)) {
    cat(tree)
    return(invisible(tree))
  }
  ext <- tools::file_ext(filename)
  if (!ext %in% c("pdf", "png"))
    stop("file extension must be either pdf or png.")
  if (any(nchar(Sys.which(c("pdflatex", "convert"))) == 0))
    stop("pdflatex and convert must be on system PATH.")

  # Generate output in temporary folder
  outfile <- tempfile(fileext = ".tex")
  template <- system.file(file.path("extdata", "ast-template.tex"), package = "jsvm")
  tex <- readLines(template)
  tex <- sub("<DPI>", dpi, tex, fixed = TRUE)
  tex <- sub("<TREE_STRING>", tree, tex, fixed = TRUE)
  writeLines(tex, outfile)

  # TODO: the following lines don't work
  # cmd <- shQuote(paste("pdflatex -interaction=batchmode -shell-escape -output-directory", tempdir(), outfile))
  # system(cmd)

  # Copy the desired file to specified file location
  # getfile <- paste0(tools::file_path_sans_ext(outfile), ".", ext)
  # file.copy(getfile, file.path(path, filename))
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
      if (style) return(paste0("\\node[sym] {\\`{}", x, "\\`{}};"))
      else return(paste0("{", encodeString(x, quote = "`"), "}"))
    } else if (style) return(paste0("\\node[sym] {", x, "};"))
    else return(paste0("{", x, "}"))
  }
  else if (!is.pairlist(x) && !is.call(x)) {
    return(paste0("<inline ", paste0(class(x), collapse = "/"), ">"))
  }
  sub <- lapply(x, .subexpr2tikz, style = style)
  nm <- names(sub)
  if (is.null(nm)) return(sub)
  has_name <- nm != ""
  label <- paste0(nm, " = ")
  sub[has_name] <- paste0(label[has_name], sub[has_name])
  sub
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