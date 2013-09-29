:- module(
  stcn_generic,
  [
    ppn//1 % -PPN:atom
  ]
).

/** <module> STCN generics

Things that are used throughout the STCN project,
but are not generic enough to be in PGC.

@author Wouter Beek
@version 2013/06, 2013/09
*/

:- use_module(dcg(dcg_ascii)).
:- use_module(dcg(dcg_multi)).
:- use_module(library(semweb/rdf_db)).
:- use_module(xml(xml_namespace)).

:- xml_register_namespace(foaf,  'http://xmlns.com/foaf/0.1/').
:- xml_register_namespace(stcn,  'http://stcn.data2semantics.org/resource/').
:- xml_register_namespace(stcnv, 'http://stcn.data2semantics.org/resource/vocab/').

:- rdf_meta(category_class(?,r)).



%! ppn(-PPN:iri)// is det.
% Parses a PPN identifier and returns its IRI.
%
% There are 3*10**8 possibilities, so don't use this for generation, please.

ppn(PPN) -->
  dcg_multi1(ppn_char, 9, Codes),
  {
    atom_codes(Name1, Codes),
    atomic_list_concat(['Publication',Name1], '/', Name2),
    rdf_global_id(stcn:Name2, PPN)
  }.

ppn_char(C) -->
  decimal_digit(C).
ppn_char(C) -->
  x(C).



/*
%! category_class(?Category:atom, ?Class:uri) is nondet.

category_class(author, foaf:'Agent').
category_class(printer_publisher, foaf:'Agent').
category_class(publications, stcnv:'Publication').
category_class(topical_keyword, stcnv:'Topic').
category_class(translator_editor, foaf:'Agent').

ppn_resource(Graph, Category, PPN, Individual):-
  rdf_global_id(stcn:PPN, Individual),
  category_class(Category, Class),
  (
    rdfs_individual_of(Individual, Class), !
  ;
    rdf_assert_individual(Individual, Class, Graph)
  ).
*/

