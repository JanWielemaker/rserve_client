name('rserve_client').
version('1.1.3').
pack_version(2).
title('R Rserve client').
keywords(['R', 'statistics', 'chart', 'plot']).
author( 'Jan Wielemaker', 'jan@swi-prolog.org' ).
packager( 'Jan Wielemaker', 'jan@swi-prolog.org' ).
maintainer( 'Jan Wielemaker', 'jan@swi-prolog.org' ).
home( 'https://github.com/JanWielemaker/rserve_client' ).
download( 'https://github.com/JanWielemaker/rserve_client/releases/V*.zip' ).
% requires(prolog >= "9.3.8"). % TODO: stable version
requires(prolog:c_cxx(_)).
