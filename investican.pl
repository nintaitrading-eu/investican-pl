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
%% import_facts
% Imports facts from csv-files.
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

%% has_potential(+Code, +Market)
% Checks if a commodity has potential, based on a set of criteria.
% Example: [tess, ebr] will give json([market=ebr,code=tess])
has_potential(Code, Market) :-
  stock(Code, Market),
  market(Market, 'low_tax').

%% element_to_json(+Atom_List, ?Result)
% Converts an element of a list to a json object.
% Example: [tess, ebr] will give json([market=ebr,code=tess])
element_to_json(ListElement, Result) :-
  nth0(0, ListElement, ElemMarket),
  nth0(1, ListElement, ElemCode),
  prolog_to_json(json([market=ElemMarket, code=ElemCode]), Result).

%% elements_to_json(+Empty_List, ?NewList)
% Base case for converting elements of a list, to json-objects.
% Deals with an empty list as input.
elements_to_json([], NewList) :-
  write('test base case emptly list: '), write(NewList), nl.

%% elements_to_json(+ListWith1Element, ?NewList)
% Base case for converting elements of a list, to json-objects.
% Deals with a list with just 1 element as input.
elements_to_json([H], NewList) :-
  write('test base case list 1 element.'), nl,
  element_to_json(H, ElemHeadJson),
  append([], NewList, TmpList),
  (var(NewList) -> append([ElemHeadJson], [], X); append([ElemHeadJson], TmpList, X)),
  elements_to_json([], X).

%% elements_to_json(+ListWithAtomPairListElements, ?NewList)
% Converts the elements of a list of atom-pair lists, to json-objects.
% E.g.: [[tess, ebr],[sbm, ams]]
% will result in NewList being
% [json(market=ebr,code=tess),json(market=ams,code=sbm)]
elements_to_json([H|T], NewList) :-
  element_to_json(H, ElemHeadJson),
  append([], NewList, TmpList),
  (var(NewList) -> append([ElemHeadJson], [], X); append([ElemHeadJson], TmpList, X)),
  elements_to_json([T], X).

test_assignment([A], [B], X) :-
  test_assignment([B], X).

test_assignment([B], X) :-
  append(B, [], X),
  write(X), nl.

%% main
% Main predicate of the application.
main :-
  import_facts,
  test_assignment([[kak]], [[bee]], R0),
  write('test* R0: '), write(R0), nl,
  setof([Y, X], (has_potential(X, Y)), X0),
  write('test X0: '), write(X0), nl,
  elements_to_json(X0, Choco),
  % TODO: make a base case work, I do not think it properly exits test.
  write('test: after test call'), nl,
  write('test Choco: '), write(Choco), nl,
  %json_write(current_output, Choco),
  %open('result.txt', write, Stream),
  %json_write(Stream, Choco),
  %close(Stream),
  write('Done.'), nl.
