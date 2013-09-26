:- module(
  picarta_scrape,
  [
    picarta_scrape/3 % +Graph:atom
                     % +Category:atom
                     % +PPN:uri
  ]
).

/** <module> PICARTA SCRAPE

In order to scraping Picarta one needs only a little extra code on top
of the API for querying Picarta.

@author Wouter Beek
@version 2013/06
*/

:- use_module(library(semweb/rdf_db)).
:- use_module(picarta(picarta_query)).
:- use_module(rdf(rdf_build)).
:- use_module(stcn(stcn_generic)).
:- use_module(xml(xml_namespace)).

:- xml_register_namespace(
  stcnv,
  'http://stcn.data2semantics.org/resource/vocab/'
).



%! picarta_scrape(+Graph:atom, +Category:atom, +PPN:uri) is det.

picarta_scrape(Graph, Category, PPN):-
  picarta_query_ppn(PPN, URI, Pairs),
  ppn_resource(Graph, Category, PPN, Subject),
  rdf_assert(Subject, stcnv:scrapedFrom, URI, Graph),
  forall(
    member(Name/Object, Pairs),
    (
      rdf_global_id(stcnv:Name, Predicate),
      rdf_assert_literal(Subject, Predicate, Object, Graph)
    )
  ).

