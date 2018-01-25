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
% Facts
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% Market information
market(etr).
market(ebr).
market(ams).

has_low_tax(ebr).
has_low_tax(ams).

%%% Stocks
% etr
stock(fme, etr).
%% ebr
stock(cofb, ebr).
stock(tess, ebr).
stock(tnet, ebr).
stock(exm, ebr).
% ams
stock(sbm, ams).

stock_10y_low(fme, 1).
stock_10y_low(cofb, 1).
stock_10y_low(tess, 1).
stock_10y_low(tnet, 1).
stock_10y_low(sbm, 1).
stock_10y_high(fme, 10).
stock_10y_high(cofb, 10).
stock_10y_high(tess, 10).
stock_10y_high(tnet, 10).
stock_10y_high(sbm, 10).

%%% Funds
%fund(ebr, tst).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Predicates
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

has_potential(Code, Market) :-
  stock(Code, Market),
  has_low_tax(Market).

find_all :-
  has_potential(X, Y),
  write(Y), write('.'), write(X), nl.
  %open('result.txt', write, Stream),
  %setof(X, (has_potential(X, _)), X0), 
  %write(X0), nl,
  %write(Stream, X0),
  %forall(member(X, X0), ),
  %close(Stream).
