# Rserve client for SWI-Prolog/SWISH

## Rserve R-package:

  - https://rforge.net/Rserve/doc.html
  - https://github.com/s-u/Rserve
  - Installation of Rserve:
    - Default version (1.7.3):
      - install.packages("Rserve")
    - Latest (1.8.5, has lots of enhancements)
      - install.packages("Rserve",,"http://rforge.net/",type="source")

## Compiling this package

With SWI-Prolog and its  `swipl-ld`   utility  installed,  compiling the
interface done using

  ```{shell}
  make
  ```

This is tested on Ubuntu (14.04 and 16.04).  It performs these steps:

  - Clone my fork of Rserve (some extensions to the C++ client)
  - Configure and build the C++ client library
  - Build the SWI-Prolog interface `rserve.so`

## Using this package

This     package     is     primarily       intended      to     support
[SWISH](http://swish.swi-prolog.org).    We    created      a    [Docker
container](https://github.com/JanWielemaker/rserve-sandbox)  that   runs
Rserve in a sandbox. The container   exposes  Rserve using a Unix-domain
socket at the following address:

    /home/rserve/socket

With SWISH and is interface  installed   in  adjacent directories, i.e.,
below the same parent, R may be linked to SWISH doing

    ```{prolog}
    :- use_module('../swipl-rserve-client/r_swish').
    :- use_module('../swipl-rserve-client/r_sandbox').
    ```

Alternatively, Rserve can be run without sandbox  and R may be linked to
SWISH doing the statements  below,  which   requires  users  to login to
SWISH.

    ```{prolog}
    :- use_module('../swipl-rserve-client/r_swish').
    :- use_module(lib/authenticate).
    ```

## Status

This is just a proof of context. Obviously missing functionality:

  - Cover more _Prolog term --> R_ translations, following Real.
  - Support OOB (Out Of Band) communication introduced in recent
    versions of Rserve to deal with R I/O.
  - Make it easy to create an R data frame from the solutions of
    a predicate.
