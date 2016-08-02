# Rserve client for SWI-Prolog

## Rserve R-package:

  - https://rforge.net/Rserve/doc.html
  - https://github.com/s-u/Rserve

Installation:

    install.packages("Rserve")

Running

    library(Rserve)
    Rserve()

## Issues

  - ggplot: requires <-/1.  Seems to need capture.output().
