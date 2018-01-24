/*
 * Investican.pl:
 * Check commodities, to see if they can be possible investment candidates,
 * based on specific criteria.
 *
 * See LICENSE.txt for license info.
 */

/* Needs:
  - to be a stock or a fund
  - current value at high value over 10 years, minus 30%
  - or minus 10% for a fund
  - have a downtrend, that is followed by consolidation
  - have a single tax
  - or a double tax that is < 20%
*/

/* Facts */

market(etr).
market(ebr).
market(ams).

has_low_tax(ebr).
has_low_tax(ams).

stock(fme).
stock(cofb).
stock(tess).
stock(tnet).
stock(sbm).
market_of_stock(fme, etr).
market_of_stock(cofb, ebr).
market_of_stock(tess, ebr).
market_of_stock(tnet, ebr).
market_of_stock(sbm, ams).

fund(ebr, tst).


/* Predicates */

has_potential(Market, Stock) :-
  stock(Stock),
  market_of_stock(Stock, Market),
  has_low_tax(Market).

find_all_for_market(Market) :-
  setof(X, (has_potential(Market, X)), X0),
  nl,
  write(Market),
  write('.'),
  write(X0), nl.
