(TeX-add-style-hook
 "ast-template"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-class-options
                     '(("standalone" "dvipsnames" "convert={density=<DPI>,outext=.png}")))
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("fontenc" "T1")))
   (TeX-run-style-hooks
    "latex2e"
    "standalone"
    "standalone10"
    "fontenc"
    "tikz"
    "tikz-qtree"
    "xcolor"
    "inconsolata"))
 :latex)

