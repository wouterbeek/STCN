:- module(
  stcn,
  [
    stcn_script/0
  ]
).

/** <module> STCN

This module only contains predicates that have a domain-specific use.
These predicate should be converted to some other module or be removed.

@author Wouter Beek
@see http://www.kb.nl/kbhtml/stcnhandleiding/frames.html
@version 2013/09
*/

:- use_module(library(debug)).
:- use_module(os(datetime_ext)).
:- use_module(rdf(rdf_serial)).
:- use_module(stcn(stcn_parse)).
:- use_module(stcn(stcn_schema)).

:- debug(stcn).



stcn_script:-
  % Begin time.
  date_time(Start),
  debug(stcn, 'Loading started at: ~w.\n', [Start]),

  stcn_schema('STCNV'),
  rdf_save2('STCNV'),

  collect_lines,
gtrace,
  parse_redactiebladen('STCN'),
  rdf_save2('STCN'),

  %stcn_scrape,

  %stcn_clean,

  % End time.
  date_time(End),
  debug(stcn, 'Loading ended at: ~w.\n', [End]).

