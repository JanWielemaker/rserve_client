:- use_module(rserve).

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
