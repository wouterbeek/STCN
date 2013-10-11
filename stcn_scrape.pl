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
:- use_module(os(file_ext)).
:- use_module(picarta(picarta_query)).
:- use_module(rdf(rdf_build)).
:- use_module(rdf(rdf_lit)).
:- use_module(rdfs(rdfs_build)).
:- use_module(rdfs(rdfs_read)).
:- use_module(stcn(stcn_generic)).
:- use_module(xml(xml_namespace)).

:- xml_register_namespace(picarta, 'http://picarta.pica.nl/').
:- xml_register_namespace(stcn, 'http://stcn.data2semantics.org/resource/').
:- xml_register_namespace(stcnv, 'http://stcn.data2semantics.org/vocab/').



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
stcn_scrape(FromG, C1, ToG):-
  rdf_global_id(stcnv:C1, C2),
  
  % We assume that the PPNs of the given category are currently loaded.
  setoff(
    PPN3,
    (
      rdfs_individual(m(t,f,f), PPN1, C2, FromG),
      rdf_global_id(stcn:PPN2, PPN1),
      split_atom_exclusive('/', PPN2, [C1,PPN3])
    ),
    PPNs
  ),

  % Write the PPNs of the given category to a TODO file.
  atomic_list_concat(['TODO',C1], '_', TODO_FileName),
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
  atomic_list_concat(['DONE',C1], '_', DONE_FileName),
  create_file(data('.'), DONE_FileName, text, DONE_File),

  % Process the TODO file.
  list_script(picarta_scrape(C1, ToG), TODO_File, DONE_File).

picarta_scrape(Category, G, PPN_Name):-
  picarta_query_ppn(PPN_Name, ScrapeSite, NVPairs),
  ppn_resource(Category, PPN_Name, PPN),
  rdf_assert_individual(PPN, Category, G),
  rdfs_assert_seeAlso(PPN, ScrapeSite, G),
  forall(
    member(N/V, NVPairs),
    (
      rdf_global_id(picarta:N, Pred),
      rdf_assert_literal(PPN, Pred, V, G)
    )
  ).
