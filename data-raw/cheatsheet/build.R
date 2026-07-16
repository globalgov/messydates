# Rebuild the messydates cheat sheet from cheatsheet.tex.
#
# Requires a LaTeX distribution (pdflatex, with the tcolorbox, fontawesome5,
# inconsolata, and extsizes packages) and Ghostscript (`gs`) for the PNG.
# Run from the package root or this directory:
#   Rscript data-raw/cheatsheet/build.R
#
# It compiles the PDF, rasterises a PNG, and copies both into the locations
# used by the installed package, the README, and the pkgdown site.

# Locate this folder whether run from the package root or from here.
here <- if (file.exists("cheatsheet.tex")) "." else "data-raw/cheatsheet"
stopifnot(file.exists(file.path(here, "cheatsheet.tex")))
root <- normalizePath(file.path(here, "..", ".."))

owd <- setwd(here)
on.exit(setwd(owd))

# 1. Compile the PDF (twice, so any layout references settle).
for (i in 1:2)
  system2("pdflatex", c("-interaction=nonstopmode", "-halt-on-error",
                        "cheatsheet.tex"), stdout = FALSE)

# 2. Rasterise a PNG (150 dpi -> ~1650px wide).
system2("gs", c("-dSAFER", "-dBATCH", "-dNOPAUSE", "-sDEVICE=png16m",
                "-r150", "-dGraphicsAlphaBits=4", "-dTextAlphaBits=4",
                "-sOutputFile=cheatsheet.png", "cheatsheet.pdf"))

# 3. Distribute to the installed package, README, and pkgdown site.
copies <- c(
  file.path(root, "inst", "figures", "cheatsheet.pdf"),
  file.path(root, "docs", "reference", "figures", "cheatsheet.pdf"),
  file.path(root, "man", "figures", "cheatsheet.png"),
  file.path(root, "docs", "reference", "figures", "cheatsheet.png"))
for (dest in copies) {
  if (!dir.exists(dirname(dest))) next
  src <- if (grepl("\\.pdf$", dest)) "cheatsheet.pdf" else "cheatsheet.png"
  file.copy(src, dest, overwrite = TRUE)
}

# 4. Tidy LaTeX auxiliaries.
unlink(c("cheatsheet.aux", "cheatsheet.log", "build.log"))
message("Cheat sheet rebuilt and distributed.")
