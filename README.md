# Rserve client for SWI-Prolog

## Rserve R-package:

  - https://rforge.net/Rserve/doc.html
  - https://github.com/s-u/Rserve

Installation:

    install.packages("Rserve")

Running

    library(Rserve)
    Rserve()

## Getting plots as SVG files

  - http://r.789695.n4.nabble.com/Save-generic-plot-to-file-before-rendering-to-device-td3659999.html
  - https://cran.r-project.org/web/packages/R.devices/vignettes/R.devices-overview.pdf
  - https://stat.ethz.ch/R-manual/R-devel/library/grDevices/html/cairo.html

  - Simple:
    - At start:
      options(device=svg(filename="test%04d.svg"))
    - After each query
      repeat, r_eval(r, 'dev.off()', X), X = [1], !.
    - Find files RplotXXX.svg

## SWISH integration

  - If A is approached
    - create an R connection (on demand)
    - set graphics to SVG
    - record to destroy the R session on thread exit
  - After each query
    - If there is an R session
      - Close the open graphics devices
      - Fetch the graphics (compress)?
      - Pass in pseudo variable

## Issues

  - Get console output, notably errors
  - Control the name of the graphics files (see above: filename=)
