% The load file for the STCN project.

:- use_module(library(ansi_term)).

:- dynamic(user:project/3).
:- multifile(user:project/3).
user:project('STCN', 'Short Title Catalogue of the Netherlands', stcn).

:- use_module(load_project).
:- load_project([
  plc-'Prolog-Library-Collection',
  plGraph,
  plHtml,
  plHttp,
  plLangTag,
  plRdf,
  plSet,
  plSparql,
  plTree,
  plUri,
  plXml,
  plXsd
]).



:- use_module(library(semweb/rdf_db), except([rdf_node/1])).

:- rdf_register_prefix(picarta, 'http://picarta.pica.nl/').
:- rdf_register_prefix(stcn, 'http://stcn.org/resource/').
:- rdf_register_prefix(stcno, 'http://stcn.org/ontology/').


:- use_module(plRdf(management/rdf_prefixes)).
:- assert_cc_prefixes.

