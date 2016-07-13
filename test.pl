:- use_module(rserve).
:- debug(r).

%%	rserve:r_open_hook(+Alias, -Connection)
%
%	Manage a dynamic binding to R.

:- multifile rserve:r_open_hook/2.

rserve:r_open_hook($, R) :-
	nb_current('R', R), !.
rserve:r_open_hook($, R) :-
	r_open(R,
	       [ host("/tmp/R-socket-janw"),
		 port(-1)
	       ]),
	debug(r, 'Created ~p', [R]),
	r_eval(R, "options(device=svg(filename=\"image%04d.svg\"))", X),
	debug(r, 'Devices: ~p', [X]),
	nb_setval('Rimage', 1),
	nb_setval('R', R), !.

svg_files(List) :-
	nb_current('R', _),
	r_eval($, "dev.cur()", [L]), L > 1, !,
	repeat, r_eval($, "dev.off()", [1]), !,
	fetch_images(List).
svg_files([]).

fetch_images(Files) :-
	nb_getval('Rimage', N),
	format(string(Name), "image~|~`0t~d~4+.svg", N),
	(   catch(r_read_file($, Name, File), E,
		  (print_message(warning, E), fail))
	->  Files = [File|Rest],
	    N2 is N+1,
	    nb_setval('Rimage', N2),
	    fetch_images(Rest)
	;   Files = []
	).

r :-
	r_open(R,
	       [ alias(r),
		 host("/tmp/R-socket-janw"),
		 port(-1)
	       ]),
	assertion(R==r).

get_strings(X) :-
	r_eval(r, "a<-c(\"aap\", \"noot\"); a", X).

get_boolean(X) :-
	r_eval(r, "a<-c(TRUE, FALSE, FALSE, TRUE); a", X).

cp(In, Out) :-
	r_assign(r, v, In),
	r_eval(r, v, Out).

mean(List, Mean) :-
	r_assign(r, v, List),
	r_eval(r, "mean(v)", Mean).

mean_1m :-
	numlist(1, 1 000 000, List),
	time(mean(List, Mean)),
	writeln(Mean).

leak :-
	forall(repeat,
	       (   numlist(1, 1 000, List),
		   mean(List, _Mean)
	       )).

iris(IRIS) :-
	r_eval(r, "data(iris); iris", IRIS).

svg(Data) :-
	r_assign(r, a, Data),
	r_eval(r, "svg(\"x.svg\")", _),
	r_eval(r, "plot(a)", _),
	r_eval(r, "dev.off()", _).

