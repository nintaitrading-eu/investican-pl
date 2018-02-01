#!/usr/local/bin/swipl
%
% Investican.pl:
% Check commodities, to see if they can be possible investment candidates,
% based on specific criteria.
%
% See LICENSE.txt for license info.
%

/* Needs:
  - to be a stock or a fund
  - current value at high value over 10 years, minus 30%
  - or minus 10% for a fund
  - have a downtrend, that is followed by consolidation
  - have a single tax
  - or a double tax that is < 20%
*/

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Import modules
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- use_module(library(csv)).
:- use_module(library(http/json)).
:- use_module(library(http/json_convert)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Init
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- set_prolog_flag(verbose, silent).
:- initialization main.

:- json_object commodity(market:string, code:string).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Facts
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Note: Changeable datasets for filling the knowledge
% base, are loaded from csv-files.

%%% Market information
% TODO: move to csv file.
stock_10y_low(cofb, 1).
stock_10y_low(tess, 1).
stock_10y_low(tnet, 1).
stock_10y_low(sbm, 1).
stock_10y_high(fme, 10).
stock_10y_high(cofb, 10).
stock_10y_high(tess, 10).
stock_10y_high(tnet, 10).
stock_10y_high(sbm, 10).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Predicates
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
import_facts :-
  % stocks
  write("Loading stocks from stocks.csv..."), nl,
  csv_read_file('stocks.csv', Stocks, [functor(stock), separator(0';)]),
  maplist(writeln, Stocks),
  maplist(assert, Stocks),
  % markets
  write("Loading markets from markets.csv..."), nl,
  csv_read_file('markets.csv', Markets, [functor(market), separator(0';)]),
  maplist(writeln, Markets),
  maplist(assert, Markets).

has_potential(Code, Market) :-
  stock(Code, Market),
  market(Market, 'low_tax').

%convert_to_json(Market, Code, Result) :-
%  Result is format("{\"market\": \"~w\",\"commodity\": \"~w\"}", [Market, Code]).

%dash_to_dots(AnElement, ConvertedAtom) :-
%  atomic_list_concat(Words, '-', AnElement),
%  atomic_list_concat(Words, '.', ConvertedAtom).

%make a list that changes
element_to_json(ListElement, Result) :-
  nth0(0, ListElement, ElemMarket),
  nth0(1, ListElement, ElemCode),
  prolog_to_json(json([market=ElemMarket, code=ElemCode]), Result).

elements_to_json([], NewList) :-
  write('test base case emptly list: '), write(NewList), nl.

elements_to_json([H], NewList) :-
  write('test base case list 1 element.'), nl,
  element_to_json(H, ElemHeadJson),
  append([], NewList, TmpList),
  (var(NewList) -> append([ElemHeadJson], [], X); append([ElemHeadJson], TmpList, X)),
  elements_to_json([], X).

elements_to_json([H|T], NewList) :-
  element_to_json(H, ElemHeadJson),
  append([], NewList, TmpList),
  (var(NewList) -> append([ElemHeadJson], [], X); append([ElemHeadJson], TmpList, X)),
  elements_to_json(T, X).

test_assignment(A, B, X) :-
  test_assignment(B, X).

test_assignment(B, X) :-
  append([B], [], X),
  write(X), nl.

main :-
  import_facts,
  test_assignment([['kak']], [['bee']], R0),
  write('test* R0: '), write(R0), nl,
  setof([Y, X], (has_potential(X, Y)), X0),
  write('test X0: '), write(X0), nl,
  elements_to_json(X0, Choco),
  % TODO: make a base case work, I don't think it properly exits test.
  write('test: after test call'), nl,
  write('test Choco: '), write(Choco), nl,
  %json_write(current_output, Choco),
  %open('result.txt', write, Stream),
  %json_write(Stream, Choco),
  %close(Stream),
  write('Done.'), nl.
