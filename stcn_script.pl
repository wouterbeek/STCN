:- module(
  stcn_script,
  [
    stcn_script/0
  ]
).

/** <module> STCN script

This module only contains predicates that have a domain-specific use.
These predicate should be converted to some other module or be removed.

@author Wouter Beek
@see http://www.kb.nl/kbhtml/stcnhandleiding/frames.html
@version 2013/09-2013/10, 2013/12, 2015/02
*/

:- use_module(library(lists), except([delete/3,subset/2])).
:- use_module(library(semweb/rdf_db), except([rdf_node/1])).

:- use_module(plRdf(management/rdf_save_any)).

:- use_module(stcn(stcn_schema)).
:- use_module(stcn(stcn_void)).
:- use_module(stcn(parse/stcn_parse_script)).
:- use_module(stcn(scrape/stcn_scrape_script)).





stcn_script:-
  % Assert the STCN Ontology.
  OGraph = 'STCN-Ontology',
  stcn_schema(OGraph),
  rdf_save_any([format(turtle),graph(OGraph)]),

  % Parse the redactiebladen.
  stcn_parse_script(PGraph),

  % Scrape Picarta.
  stcn_scrape_script(PGraph),

  % STCN-VoID
  VGraph = 'STCN-VoID',
  stcn_void(VGraph),
  rdf_save_any([format(turtle),graph(VGraph)]).

