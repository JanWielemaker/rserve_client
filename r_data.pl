/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2016, VU University Amsterdam
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

:- module(r_data,
	  [ r_data_frame/3		% +Rvar, +Columns, :Goal
	  ]).
:- use_module(r_swish).
:- use_module(library(apply)).
:- use_module(library(error)).

:- meta_predicate
	r_data_frame(+, +, 0).

%%	r_data_frame(+Rvar, +Columns, :Goal) is det.
%
%	Create an R data.frame from the solutions of Goal. The resulting
%	data frame is bound to the R variable Rvar.
%
%	@arg	Rvar is the name of the R output variable
%	@arg	Columns is a list Name=Var

r_data_frame(RVar, ColSpec, Goal) :-
	must_be(atom, RVar),
	maplist(arg(1), ColSpec, Names),
	maplist(arg(2), ColSpec, Vars),
	Templ =.. [v|Vars],
	functor(Templ, _, NCols),
	findall(Templ, Goal, Rows),
	col_data(1, NCols, Rows, ColData),
	create_r_dataframe(RVar, Names, ColData).

col_data(I, NCols, Rows, [ColI|ColR]) :-
	I =< NCols, !,
	maplist(arg(I), Rows, ColI),
	I2 is I + 1,
	col_data(I2, NCols, Rows, ColR).
col_data(_, _, _, []).

create_r_dataframe(RVar, Names, ColData) :-
	compound_name_arguments(Term, 'data.frame', ColData),
	RVar <- Term,
	colnames(RVar) <- Names.
