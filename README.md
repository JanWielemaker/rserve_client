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

This  package  is  primarily  intended  for    accessing   R  in  server
environments such as [SWISH](http://swish.swi-prolog.org).  We created a
[Docker container](https://github.com/JanWielemaker/rserve-sandbox) that
runs  Rserve  in  a  sandbox.  The  container  exposes  Rserve  using  a
Unix-domain socket at the following address:

    /home/rserve/socket

With SWISH and is interface  installed   in  adjacent directories, i.e.,
below the same parent, R may be linked to SWISH doing

    user:file_search_path(rserve, '../swipl-rserve-client').
    :- use_module(lib/r_swish).

Now, R is not safe. You should either run Rserve in a tight OS container
and load `rserve(r_sandbox)` or  run  SWISH   in  authenticated  mode by
loading `lib/authenticate.pl.`


## Status

This is just a proof of context. Obviously missing functionality:

  - Cover more _Prolog term --> R_ translations, following Real.
  - Support OOB (Out Of Band) communication introduced in recent
    versions of Rserve to deal with R I/O.
  - Turn this into a SWI-Prolog pack.

## Related projects

This           interface           is             inspired            by
[Real](http://stoics.org.uk/~nicos/sware/real/)        by         [Nicos
Angelopoulos](http://stoics.org.uk/~nicos/).  Main differences:

  - _Real_ is _embedded_ in SWI-Prolog.  This is more productive for
    local deployment as the communication is faster and R has access
    to its default environment.  Thus, R can open graphical windows
    and can read and write files.  _Real_ is also much more mature,
    notably in the supporting a much larger part of the R syntax
    from Prolog.

  - _Rserve_ runs typically using a different user in a different
    environment.  The R environment cannot easily communicate with
    your local development environment.  When used in a (web)
    server environment this comes with several advantages.  We can
    seriously sandbox the R executable, each query in SWISH gets
    its own R instance and information can thus nog leak between
    queries and users.
