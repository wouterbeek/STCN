:- module(
  stcn_scrape,
  [
    stcn_scrape/2 % +Category:oneof(['Author','Printer','Publication','Topic'])
                  % +Graph:atom
  ]
).

/** <module> STCN scrape

Fully automated scrape for the STCN.

@author Wouter Beek
@tbd Automate the generation of the list of initial publication PPNs
     based on the redactiebladen (using module STCN_PARSE).
@version 2013/06, 2013/09
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
:- xml_register_namespace(stcnv, 'http://stcn.data2semantics.org/resource/vocab/').



%! stcn_scrape(
%!   +Category:oneof([author,printer_publisher,'Publication','Topic',translator_editor]),
%!   +Graph:atom
%! ) is det.
% First we check whether the STCN content that we are after is already
% available locally (either in memory or stored in a file).
% Otherwise, we start scraping.
%
% The following categories are used:
%   * `Publication`
%   * `author`
%   * `printer_publisher`
%   * `translator_editor`

% Scrape from remote Website (Picarta),
% Without a prior TODO file.
stcn_scrape(Category1, G):-
  rdf_global_id(stcnv:Category1, Category2),
  % We assume that the PPNs of the given category are currently loaded.
  setoff(
    PPN3,
    (
      (
        rdfs_individual(m(t,f,f), PPN1, Category2, G)
      ;
        rdf_literal(_, Category2, PPN1, G)
      ),
      rdf_global_id(stcn:PPN2, PPN1),
      split_atom_exclusive('/', PPN2, [Category1,PPN3])
    ),
    PPNs
  ),

  % Write the PPNs of the given category to a TODO file.
  atomic_list_concat(['TODO',Category1], '_', TODO_FileName),
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
  atomic_list_concat(['DONE',Category1], '_', DONE_FileName),
  create_file(data('.'), DONE_FileName, text, DONE_File),

  % Process the TODO file.
  list_script(picarta_scrape(Category1, G), TODO_File, DONE_File).

picarta_scrape(Category, G, PPN_Name):-
  picarta_query_ppn(PPN_Name, ScrapeSite, NVPairs),
  ppn_resource(G, Category, PPN_Name, PPN),
  rdfs_assert_seeAlso(PPN, ScrapeSite, G),
  forall(
    member(N/V, NVPairs),
    (
      rdf_global_id(picarta:N, Pred),
      rdf_assert_literal(PPN, Pred, V, G)
    )
  ).
