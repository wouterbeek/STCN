:- module(
  stcn_schema,
  [
    stcn_schema/1 % +Graph:atom
  ]
).

/** <module> STCN_SCHEMA

Schema for the STCN database.

@author Wouter Beek
@tbd Use XLink for resources that denote Web pages.
@version 2013/01-2013/03, 2013/06, 2013/09-2013/10, 2015/02
*/

:- use_module(library(semweb/rdf_db), except([rdf_node/1])).

:- use_module(plRdf(api/rdf_build)).
:- use_module(plRdf(api/rdfs_build)).

:- use_module(stcn(stcn_kmc)).
:- use_module(stcn(picarta/picarta_query)). % Make private call.





stcn_schema(G):-
  % Publication
  rdfs_assert_class(stcno:'Publication', G),
  rdfs_assert_label(stcno:'Publication', publication, G),
  rdfs_assert_label(stcno:'Publication', [nl]-publicatie, G),
  
  % Topic
  rdfs_assert_class(stcno:'Topic', G),
  rdfs_assert_label(stcno:'Topic', topic, G),
  rdfs_assert_label(stcno:'Topic', [nl]-onderwerp, G),

  % Denotes relations between a PPN and the location from which information
  % about that PPN was extracted.
  rdf_assert_property(stcno:scrapedFrom, G),
  rdfs_assert_label(stcno:scrapedFrom, 'scraped from', G),
  rdfs_assert_comment(
    stcno:scrapedFrom,
    'Some information of this PPN was scraped from this location.',
    G
  ),
  rdfs_assert_domain(stcno:scrapedFrom, stcno:'Publication', G),
  
  % The KB uses this predicate letter to denote the same property.
  rdf_assert_property(stcno:kb_name, G),
  rdfs_assert_domain(stcno:kb_name, rdf:'Property', G),
  rdfs_assert_range(stcno:kb_name, rdfs:'Literal', G),
  
  % Picarta uses this predicate letter to denote the same property.
  rdf_assert_property(stcno:picarta_name, G),
  rdfs_assert_domain(stcno:picarta_name, rdf:'Property', G),
  rdfs_assert_range(stcno:picarta_name, rdfs:'Literal', G),
  
  % Picarta-derived properties
  forall(
    picarta_query:picarta_attribute(_Match, AttributeName),
    (
      rdf_global_id(stcno:AttributeName, Attribute),
      rdf_assert_property(Attribute, G)
    )
  ),

  assert_schema_kmcs(G).

