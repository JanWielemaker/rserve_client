:- module('Rswish',
	  [ (<-)/2,
	    (<-)/1,
	    op(900,  fx, <-),
	    op(900, xfx, <-)
	  ]).
:- use_module(rserve).
:- use_module(library(debug)).
:- use_module(library(pengines)).
:- use_module(library(http/html_write)).
:- use_module(library(http/js_write)).

/** <module> R plugin for SWISH
*/

Var <- Value :-
	var(Var), !,
	r_eval($, Value, Var),
	send_images.
Var <- Value :-
	atom(Var), !,
	r_assign($, Var, Value).

<- Expression :-
	r_eval($, Expression, _),
	send_images.

		 /*******************************
		 *	       IMAGES		*
		 *******************************/

:- multifile rserve:r_open_hook/2.

rserve:r_open_hook($, R) :-
	nb_current('R', R), !.
rserve:r_open_hook($, R) :-
	r_open(R,
	       [ host("/tmp/R-socket-janw"),
		 port(-1)
	       ]),
	thread_at_exit(r_close(R)),
	debug(r, 'Created ~p', [R]),
	set_graphics_device(R),
	nb_setval('R', R), !.

:- if(false).
set_graphics_device(R) :-
	pengine_self(Me),
	format(string(Cmd),
	       "options(device=svg(filename=\"~w%03d.svg\"))", [Me]),
	r_eval(R, Cmd, X),
	debug(r, 'Devices: ~p', [X]),
	nb_setval('Rimage_base', Me),
	nb_setval('Rimage', 1).
:- else.
set_graphics_device(R) :-
	r_eval(R, "options(device=svg)", X),
	debug(r, 'Devices: ~p', [X]),
	nb_setval('Rimage_base', 'Rplot'),
	nb_setval('Rimage', 1).
:- endif.

send_images :-
	svg_files(Images), !,
	length(Images, Count),
	debug(r, 'Got ~d images~n', [Count]),
	svg_html(Images, HTMlString),
	pengine_output(HTMlString).
%	pengine_output(json{images: Images,
%			    format: "svg"}).
send_images.

svg_files(List) :-
	nb_current('R', _),
	r_eval($, "dev.cur()", [L]), L > 1, !,
	repeat, r_eval($, "dev.off()", [1]), !,
	fetch_images(List),
	nb_setval('Rimage', 1).			% restarts from 1

fetch_images(Files) :-
	nb_getval('Rimage', N),
	nb_getval('Rimage_base', Base),
	format(string(Name), "~w~|~`0t~d~3+.svg", [Base,N]),
	debug(r, 'Trying ~p~n', [Name]),
	(   catch(r_read_file($, Name, File), E, r_error_fail(E))
	->  debug(r, 'Got ~p~n', [Name]),
	    Files = [File|Rest],
	    (   debugging(r(plot))
	    ->  save_plot(Name, File)
	    ;	true
	    ),
	    N2 is N+1,
	    nb_setval('Rimage', N2),
	    fetch_images(Rest)
	;   Files = []
	).

r_error_fail(error(r_error(70),_)) :- !, fail.
r_error_fail(Error) :- print_message(warning, Error), fail.

save_plot(File, Data) :-
	setup_call_cleanup(
	    open(File, write, Out, [encoding(utf8)]),
	    format(Out, '~s', [Data]),
	    close(Out)).

%%	svg_html(+Images, -HTMlString) is det.
%
%	Turn a list of SVG images into an HTML string.

svg_html(Images, HTMlString) :-
	phrase(svg_html(Images), Tokens),
	with_output_to(string(HTMlString), print_html(Tokens)).

svg_html(Images) -->
	html(div(class('Rplots'), \rplots(Images))).

rplots([]) --> [].
rplots([H|T]) -->
	html(div(class(['reactive-size', 'R', svg]), \svg(H, []))),
	rplots(T).


svg(SVG, _Options) -->
	html(\[SVG]),
	pan_zoom,
	"".

pan_zoom -->
	html(\js_script({|javascript||
var svg  = node.node().find("svg");
//svg.removeAttr("width height");		// trying to remove white space
//svg.find("rect").first().remove();	// trying to remove white space
var data = { w0: svg.width(),
	     h0: svg.height()
	   };
var pan;

function updateSize() {
  var w = svg.closest("div.Rplots").innerWidth();
  console.log(w);

  function reactive() {
    if ( !data.reactive ) {
      var div = svg.closest("div.reactive-size");
      data.reactive = true;
      div.on("reactive-resize", updateSize);
    }
  }

  w = Math.max(w*0.85, 100);
  if ( w < data.w0 ) {
    svg.width(w);
    svg.height(w = Math.max(w*data.h0/data.w0, w/4));
    reactive();
    if ( pan ) {
      pan.resize();
      pan.fit();
      pan.center();
    }
  }
}

require(["svg-pan-zoom"], function(svgPanZoom) {
  updateSize()
  pan = svgPanZoom(svg[0], {
    maxZoom: 50
  });
});
		      |})).


