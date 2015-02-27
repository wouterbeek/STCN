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
@version 2013/06, 2013/09-2013/10, 2014/03, 2015/02
*/

:- use_module(library(aggregate)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).

:- use_module(plc(process/list_script)).

:- use_module(plRdf(api/rdf_build)).
:- use_module(plRdf(api/rdfs_build)).

:- use_module(stcn(stcn_generics)).
:- use_module(stcn(picarta/picarta_query)).

:- rdf_meta(stcn_scrape_category_relation(r,r)).





%! stcn_scrape(
%!   +FromGraph:atom,
%!   +Class:oneof(['Author','PrinterPublisher','Publication','Topic','TranslatorEditor']),
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
  list_script(
    picarta_scrape(C, ToG),
    PPNs,
    [message('PPN'),overview(true)]
  ).

stcn_scrape_category_relation('Author', picarta:author).
stcn_scrape_category_relation('PrinterPublisher', picarta:printer_publisher).
stcn_scrape_category_relation('TranslatorEditor', picarta:translator_editor).

stcn_scrape_ppns(_G, 'Publication', PPNs):- !,
  aggregate_all(
    set(PPN3),
    (
      % @tbd Restrict this to triples in graph `G`.
      rdfs_individual_of(PPN1, stcno:'Publication'),
      rdf_global_id(stcn:PPN2, PPN1),
      atomic_list_concat(['Publication',PPN3], '/', PPN2) % split
    ),
    PPNs
  ).
stcn_scrape_ppns(G, C, PPNs):-
  once(stcn_scrape_category_relation(C, P)),
  aggregate_all(
    set(PPN3),
    (
      rdf(_, P, PPN1, G),
      rdf_global_id(stcn:PPN2, PPN1),
      atomic_list_concat([C,PPN3], '/', PPN2) % split
    ),
    PPNs
  ).

picarta_scrape(Category1, G, PPN_Name):-
  picarta_query_ppn(PPN_Name, ScrapeSite, NVPairs),
  ppn_resource(Category1, PPN_Name, PPN),
  rdf_global_id(stcno:Category1, Category2),
  rdf_assert_instance(PPN, Category2, G),
  rdfs_assert_seeAlso(PPN, ScrapeSite, G),
  forall(
    member(N/V, NVPairs),
    (
      rdf_global_id(picarta:N, Pred),
      rdf_assert_string(PPN, Pred, V, G)
    )
  ).

