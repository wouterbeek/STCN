:- module(
  stcn_scrape,
  [
    stcn_scrape/0
  ]
).

/** <module> STCN_SCRAPE

Fully automated scrape for the STCN.

@author Wouter
@tbd Automate the generation of the list of initial publication PPNs
     based on the redactiebladen (using module STCN_PARSE).
@version 2013/06, 2013/09
*/

:- use_module(dcg(dcg_generic)).
:- use_module(generics(list_script)).
:- use_module(generics(meta_ext)).
:- use_module(library(semweb/rdf_db)).
:- use_module(picarta(picarta_scrape)).
:- use_module(rdf(rdf_clean)).
:- use_module(rdf(rdf_graph)).
:- use_module(rdf(rdf_lit)).
:- use_module(rdf(rdf_read)).
:- use_module(rdf(rdf_serial)).
:- use_module(rdfs(rdfs_build)).
:- use_module(stcn(stcn_generic)).
:- use_module(stcn(stcn_schema)).
:- use_module(xml(xml_namespace)).

:- xml_register_namespace(foaf,  'http://xmlns.com/foaf/0.1/').
:- xml_register_namespace(stcnv, 'http://stcn.data2semantics.org/resource/vocab/').



%! stcn_content(+Category:atom, +Graph:atom) is det.
% First we check whether the STCN content that we are after is already
% available locally (either in memory or stored in a file).
% Otherwise, we start scraping.
%
% The following categories are used:
%   * `author`
%   * `printer_publisher`
%   * `publications`
%   * `translator_editor`

% Scrape from remote Website (Picarta),
% Based on an existing TODO file.
stcn_content(publications, G):-
  stcn_scrape(publications, G), !.
% Scrape from remote Website (Picarta),
% Without a prior TODO file.
stcn_content(Category, G):-
  % We assume that the PPNs of the given category are currently loaded.
  rdf_global_id(stcnv:Category, Predicate),
  setoff(
    PPN,
    (
      rdf_literal(_PublicationPPN, Predicate, Literal, _OtherGraph),
      dcg_phrase(ppn('Publication', PPN), Literal)
    ),
    PPNs
  ),
  % Write the PPNs of the given category to a TODO file.
  atomic_list_concat(['TODO',Category], '_', FileName),
  absolute_file_name(data(FileName), File, [access(read),extensions([txt])]),
  open(File, write, Stream, []),
  forall(
    member(PPN, PPNs),
    format(Stream, '~w\n', [PPN])
  ),
  close(Stream),
  % Scraping uses the TODO file.
  stcn_scrape(Category, G).

%! stcn_scrape is det.
% Scrapes all STCN content from the Picarta Website.
%
% The final content that this generates is stored in
% =STCN_Agents= and =STCN_Publications=.

stcn_scrape:-
  AuthorsGraph = 'Picarta_Authors',
  PrinterPublishersGraph = 'Picarta_PrinterPublishers',
  PublicationsGraph = 'Picarta_Publications',
  TranslatorEditorsGraph = 'Picarta_TranslatorEditors',

  % 1. The schema.
  stcn_schema('STCNV'),

  % 2. Scrape the publications from Picarta.
  stcn_content(publications, PublicationsGraph),

  % 3. Assert occurrences in literal enumerations as separate triples.
  rdf_split_literal(_, stcnv:printer_publisher, PublicationsGraph, '; '),
  rdf_strip_literal([' '], _, stcnv:printer_publisher, PublicationsGraph),
  rdf_split_literal(_, stcnv:topical_keyword, PublicationsGraph, '; '),
  rdf_split_literal(
    _,
    stcnv:typographic_information,
    PublicationsGraph,
    ' , '
  ),

  % 4. Scrape the authors from Picarta.
  stcn_content(author, AuthorsGraph),

  % 5. Scrape the printers, publishers, translators, and editors from
  %    Picarta.
  stcn_content(printer_publisher, PrinterPublishersGraph),
  stcn_content(translator_editor, TranslatorEditorsGraph),

/*
  % 6. Assert all translator/editors as FOAF agents.
  % @tbd Check out why some translators/editors are not already asserted
  %      as FOAF agents and fix the issue there.
  forall(
    rdf(Agent, _Predicate, _Object, TranslatorEditorsGraph),
    rdf_assert_individual(Agent, foaf:'Agent', TranslatorEditorsGraph)
  ),
  % 7. Authors and translators/editors are all agents in the STCN world.
*/

  % 8. Keep the Picarta and STCN graphs separate.
  rdf_graph:rdf_graph_merge(
    [AuthorsGraph, PrinterPublishersGraph, TranslatorEditorsGraph],
    'STCN_Agents'
  ),
  rdf_graph:rdf_graph_merge([PublicationsGraph], 'STCN_Publications'),

  % Save results.
  maplist(
    rdf_save2,
    [
      AuthorsGraph,
      PrinterPublishersGraph,
      PublicationsGraph,
      'STCN_Agents',
      'STCN_Publications',
      TranslatorEditorsGraph
    ]
  ).

%! stcn_scrape(+Category:atom, +Graph:atom) is det.
% Srapes PPNs of the given category.
% We assume that these PPNs are stored in a TODO file.
%
% @param Category One of the following atoms: =author=, =printer_publisher=,
%        =publications=, =translator_editor=.
% @param Graph The atomic name of a graph.

stcn_scrape(Category, G):-
  atomic_list_concat(['TODO',Category], '_', TodoFileName),
  absolute_file_name(
    data(TodoFileName),
    TodoFile,
    [access(read),extensions([txt])]
  ),
  atomic_list_concat(['DONE',Category], '_', DoneFileName),
  absolute_file_name(data(DoneFileName), DoneFile, [extensions([txt])]),
  list_script(picarta_scrape(G, Category), TodoFile, DoneFile).

