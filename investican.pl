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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Init
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- set_prolog_flag(verbose, silent).
:- initialization main.

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

convert_to_json(Market, Code, Result) :-
  Result is format("{\"market\": \"~w\",\"commodity\": \"~w\"}", [Market, Code]).

%dash_to_dots(AnElement, ConvertedAtom) :-
%  atomic_list_concat(Words, '-', AnElement),
%  atomic_list_concat(Words, '.', ConvertedAtom).

%make a list that changes
%test([], NewList).
test([A, B|T], NewList) :-
  write('test: A='), write(A), nl,
  write('test: B='), write(B), nl,
  write('test: T='), write(T), nl,
  % TODO: Arguments not sufficiently instantiated.
  % TODO: Finish function and extend it to make [A,B] jsonified.
  append([{"lala":A, "lblb":B}], NewList),
  write('test: NewList after append='), write(NewList), nl.
  %test(T, NewList).

main :-
  import_facts,
  %dash_to_dots('market-stock', ConvertedAtom),
  % TODO: The call to convert... does not work. It gets added to the txt file.
  %convert_to_json(X, Y, R),
  setof([Y, X], (has_potential(X, Y)), X0),
  % TODO: make Choco be:
  % [{"market": "Y0", "code": "X0"}, ...]
  test(X0, Choco),
  write(Choco), nl,
  %write(X0), nl,
  %open('result.txt', write, Stream),
  %write(Stream, R),
  %close(Stream),
  write('Done.'), nl.
