:- use_module(rserve).

cp(In, Out) :-
	r_open(R, []),
	r_set(R, v, In),
	r_eval(R, v, Out),
	r_close(R).

r :-
	r_open(R, [alias(r)]),
	assertion(R==r).

mean(List, Mean) :-
	r_set(r, v, List),
	r_eval(r, "mean(v)", Mean).

iris(IRIS) :-
	r_eval(r, "data(iris); iris", IRIS).

svg(Data) :-
	r_set(r, a, Data),
	r_eval(r, "svg(\"x.svg\")", _),
	r_eval(r, "plot(a)", _),
	r_eval(r, "dev.off()", _).
