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

/** Market information **/
market(etr).
market(ebr).
market(ams).

has_low_tax(ebr).
has_low_tax(ams).

/** Stocks **/
/*** etr ***/
stock(fme).
/*** ebr ***/
stock(cofb).
stock(tess).
stock(tnet).
stock(exm).
/*** ams ***/
stock(sbm).

/*** etr ***/
market_of_stock(fme, etr).
/*** ebr ***/
market_of_stock(cofb, ebr).
market_of_stock(tess, ebr).
market_of_stock(tnet, ebr).
market_of_stock(exm, ebr).
/*** ams ***/
market_of_stock(sbm, ams).

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

/** Funds **/
/*fund(ebr, tst).*/

/* Predicates */

has_potential(Market, Stock) :-
  stock(Stock),
  market_of_stock(Stock, Market),
  has_low_tax(Market).

find_all(Market) :-
  forall(Market, write_all(Market)). 

write_all(Market) :-
  open('result.txt', write, Stream),
  setof(X, (has_potential(Market, X)), X0),
  write(Stream, Market),
  write(Stream, '.'),
  write(Stream, X0),
  write(Stream, nl),
  close(Stream).
