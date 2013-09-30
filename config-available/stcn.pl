:- module(conf_stcn, []).

/** <module> STCN

The STCN SW package.

@author Wouter Beek
@version 2013/04
*/

:- assert(user:file_search_path(kmc, stcn('KMC'))).
:- assert(user:file_search_path(datasets,     stcn('Datasets'))).
:- assert(user:file_search_path(generic,      stcn('Generics'))).
:-   assert(user:file_search_path(graph_theory, generic('Graph Theory'))).
:-   assert(user:file_search_path(math,         generic('Math'))).
:-   assert(user:file_search_path(owl,          generic('OWL'))).
:-   assert(user:file_search_path(rdf,          generic('RDF'))).
:-   assert(user:file_search_path(rdfs,         generic('RDFS'))).
:-   assert(user:file_search_path(skos,         generic('SKOS'))).
:-   assert(user:file_search_path(sparql,       generic('SPARQL'))).
:- assert(user:file_search_path(standards,    stcn('Standards'))).
:- assert(user:file_search_path(vocabularies, stcn('Vocabularies'))).

:- use_module(generic(file_ext)).
:- use_module(standards(html)).

:-
  % Data files directory.
  absolute_file_name(stcn('Debug'), DebugDirectory),
  create_directory(DebugDirectory),
  assert(user:file_search_path(debug, stcn('Debug'))),
  
  % Data files directory.
  absolute_file_name(stcn('RDF-store'), DataDirectory),
  create_directory(DataDirectory),
  assert(user:file_search_path(data, stcn('RDF-store'))),

  % Standards-supporting data files.
  absolute_file_name(data('Standards'), StandardsDirectory),
  create_directory(StandardsDirectory),
  assert(user:file_search_path(data_standards, data('Standards'))),

  % STCN data files directory.
  absolute_file_name(data('STCN'), STCN_Directory),
  create_directory(STCN_Directory),
  assert(user:file_search_path(data_stcn, data('STCN'))),

  % Wordnet data files directory.
  absolute_file_name(data('Wordnet'), WordnetDirectory),
  create_directory(WordnetDirectory),
  assert(user:file_search_path(data_wordnet, data('Wordnet'))).

:- use_module(cliopatria(hooks)).
:- use_module(library(http/http_dispatch)).
:- use_module(stcn(void_load)).

:- db_add_novel(http:location(stcn, cliopatria(stcn), [])).

:- http_handler(stcn(stcn_main), stcn_main, []).

cliopatria:menu_item(100=stcn/stcn_main, 'Dataset info').
cliopatria:menu_item(200=stcn/load_stcn, 'Load STCN').
cliopatria:menu_item(300=stcn/load_void, 'Load VoID').
cliopatria:menu_popup_order(stcn, 120).



stcn_main(_Request):-
  reply_html_file(cliopatria(default), stcn_main).
