:- module(
  stcn_scrape,
  [
    stcn_scrape/3 % +FromGraph:atom
                  % +Category:oneof(['Author','PrinterPublisher','Publication','Topic','TranslatorEditor'])
                  % +ToGraph:atom
  ]
).

/** <module> STCN scrape

Fully automated scrape for the STCN.

@author Wouter Beek
@tbd Automate the generation of the list of initial publication PPNs
     based on the redactiebladen (using module STCN_PARSE).
@version 2013/06, 2013/09-2013/10
*/

:- use_module(generics(atom_ext)).
:- use_module(generics(meta_ext)).
:- use_module(generics(list_script)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(os(file_ext)).
:- use_module(picarta(picarta_query)).
:- use_module(rdf(rdf_build)).
:- use_module(rdf(rdf_lit_build)).
:- use_module(rdfs(rdfs_build)).
:- use_module(stcn(stcn_generic)).
:- use_module(xml(xml_namespace)).

:- xml_register_namespace(picarta, 'http://picarta.pica.nl/').
:- xml_register_namespace(stcn, 'http://stcn.data2semantics.org/resource/').
:- xml_register_namespace(stcnv, 'http://stcn.data2semantics.org/vocab/').

:- rdf_meta(stcn_scrape_category_relation(r,r)).



%! stcn_scrape(
%!   +FromGraph:atom,
%!   +RDFS_Class:oneof(['Author','PrinterPublisher','Publication','Topic','TranslatorEditor']),
%!   +ToGraph:atom
%! ) is det.
% First we check whether the STCN content that we are after is already
% available locally (either in memory or stored in a file).
% Otherwise, we start scraping.
%
% The following categories are used:
%   * `Author`
%   * `PrinterPublisher`
%   * `Publication`
%   * `Topic`
%   * `TranslatorEditor`

% Scrape from remote Website (Picarta),
% Without a prior TODO file.
stcn_scrape(FromG, C, ToG):-
  stcn_scrape_ppns(FromG, C, PPNs),

  % Write the PPNs of the given category to a TODO file.
  atomic_list_concat(['TODO',C], '_', TODO_FileName),
  create_file(data('.'), TODO_FileName, text, TODO_File),
  setup_call_cleanup(
    open(TODO_File, write, Stream, []),
    forall(
      member(PPN, PPNs),
      (
        format(Stream, '~w', [PPN]),
        nl(Stream),
        flush_output(Stream)
      )
    ),
    close(Stream)
  ),

  % Start the actual scraping, using the TODO file.
  % DONE file.
  atomic_list_concat(['DONE',C], '_', DONE_FileName),
  create_file(data('.'), DONE_FileName, text, DONE_File),

  % Process the TODO file.
  list_script(picarta_scrape(C, ToG), TODO_File, DONE_File).

stcn_scrape_category_relation('Author', picarta:author).
stcn_scrape_category_relation('PrinterPublisher', picarta:printer_publisher).
stcn_scrape_category_relation('TranslatorEditor', picarta:translator_editor).

stcn_scrape_ppns(_G, 'Publication', PPNs):- !,
  setoff(
    PPN3,
    (
      % @tbd Restrict this to triples in graph `G`.
      rdfs_individual_of(PPN1, stcnv:'Publication'),
      rdf_global_id(stcn:PPN2, PPN1),
      split_atom_exclusive('/', PPN2, ['Publication',PPN3])
    ),
    PPNs
  ).
stcn_scrape_ppns(G, C, PPNs):-
  once(stcn_scrape_category_relation(C, P)),
  setoff(
    PPN3,
    (
      rdf(_, P, PPN1, G),
      rdf_global_id(stcn:PPN2, PPN1),
      split_atom_exclusive('/', PPN2, [C,PPN3])
    ),
    PPNs
  ).

picarta_scrape(Category1, G, PPN_Name):-
  picarta_query_ppn(PPN_Name, ScrapeSite, NVPairs),
  ppn_resource(Category1, PPN_Name, PPN),
  rdf_global_id(stcnv:Category1, Category2),
  rdf_assert_individual(PPN, Category2, G),
  rdfs_assert_seeAlso(PPN, ScrapeSite, G),
  forall(
    member(N/V, NVPairs),
    (
      rdf_global_id(picarta:N, Pred),
      rdf_assert_literal(PPN, Pred, V, G)
    )
  ).
