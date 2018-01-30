%:- use_module(library(http/json)).
:- use_module(library(http/json_convert)).

:- set_prolog_flag(verbose, silent).
:- initialization main.

market(ebr).
code(tess).

%:- json_object commodity(market:string, code:string).

main :-
 write('Start.'), nl,
 prolog_to_json(json([market='ebr', code='tess']), X),
 write(X), nl,
 write('Done.'), nl.
