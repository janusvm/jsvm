
<!-- README.md is generated from README.Rmd. Please edit that file -->
jsvm
====

jsvm is my personal toolbelt package with functions that I often tend to use or had fun making for some specific purpose.

Exported functions
------------------

-   `whoami()`: get filename of executing script when run with Rscript
-   `expr2tikz()`: plot the AST of an expression with TikZ (tikz-qtree) in the style of the figures in [Advanced R: Expressions](https://adv-r.hadley.nz/expressions.html)

Examples
--------

`expr2tikz` creates a tikz string, that (when using the LaTeX package [tikz-qtree](https://ctan.org/pkg/tikz-qtree)) produces a graph representing the abstract syntax tree of an expression, optionally saving the figure as either a pdf or png.

``` r
filename <- file.path("man", "figures", "README-expr2tikz.png")
expr2tikz(y <- x * 2, filename)
knitr::include_graphics(filename, dpi = 600)
```

<img src="man/figures/README-expr2tikz.png" width="50%" />

More functions and examples to come...
