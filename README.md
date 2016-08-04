# Rserve client for SWI-Prolog

## Rserve R-package:

  - https://rforge.net/Rserve/doc.html
  - https://github.com/s-u/Rserve

Installation:

  - Default version (1.7.3):
    - install.packages("Rserve")
  - Latest (1.8.5, has lots of enhancements)
    - install.packages("Rserve",,"http://rforge.net/",type="source")

Running

    library(Rserve)
    Rserve()

## Chrooting Rserve

  - https://stat.ethz.ch/pipermail/r-help/2008-February/153293.html
  - https://github.com/rocker-org/rocker
  - https://blog.jessfraz.com/post/r-containers-for-data-science/

## Issues

  - ggplot: requires <-/1.  Seems to need capture.output().
