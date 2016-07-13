:- use_module(rserve).

cp(In, Out) :-
	r_open(R, []),
	r_assign(R, v, In),
	r_eval(R, v, Out),
	r_close(R).

r :-
	r_open(R,
	       [ alias(r),
		 host("/tmp/R-socket-janw"),
		 port(-1)
	       ]),
	assertion(R==r).

mean(List, Mean) :-
	r_assign(r, v, List),
	r_eval(r, "mean(v)", Mean).

mean_1m :-
	numlist(1, 1 000 000, List),
	time(mean(List, Mean)),
	writeln(Mean).

iris(IRIS) :-
	r_eval(r, "data(iris); iris", IRIS).

svg(Data) :-
	r_assign(r, a, Data),
	r_eval(r, "svg(\"x.svg\")", _),
	r_eval(r, "plot(a)", _),
	r_eval(r, "dev.off()", _).
