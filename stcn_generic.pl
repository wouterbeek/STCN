:- module(
  stcn_generic,
  [
    ppn//3, % +Graph:atom
            % +Category:oneof(['Author','Printer','Publication','Topic'])
            % -PPN:atom
    ppn_resource/4 % +Graph:atom
                   % +Category:oneof(['Author','Printer','Publication','Topic'])
                   % +PPN_Name:atom
                   % -PPN:iri
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
:- use_module(rdf(rdf_build)).
:- use_module(rdfs(rdfs_read)).
:- use_module(xml(xml_namespace)).

:- xml_register_namespace(foaf,  'http://xmlns.com/foaf/0.1/').
:- xml_register_namespace(stcn,  'http://stcn.data2semantics.org/resource/').
:- xml_register_namespace(stcnv, 'http://stcn.data2semantics.org/vocab/').

:- rdf_meta(category_class(?,r)).



%! ppn(
%!   +Graph:atom,
%!   +Category:oneof(['Author','Printer','Publication','Topic']),
%!   -PPN:iri
%! )// is det.
% Parses a PPN identifier and returns its IRI.
%
% There are 3*10**8 possibilities, so don't use this for generation.

ppn(G, Category, PPN) -->
  dcg_multi1(ppn_char, 9, Atom, [convert(codes_atom)]),
  {ppn_resource(G, Category, Atom, PPN)}.

ppn_char(C) -->
  decimal_digit(C).
ppn_char(C) -->
  x(C).

%! ppn_resource(
%!   +Graph:atom,
%!   +Category:oneof(['Author','Printer','Publication','Topic']),
%!   +PPN_Name:atom,
%!   -PPN:iri
%! ) is det.

ppn_resource(G, Category1, PPN1, PPN3):-
  atomic_list_concat([Category1,PPN1], '/', PPN2),
  rdf_global_id(stcn:PPN2, PPN3),
  rdf_global_id(stcnv:Category1, Category2),
  (
    rdfs_individual(m(t,f,f), PPN3, Category2, G), !
  ;
    rdf_assert_individual(PPN3, Category2, G)
  ).

