:- module(
  stcn_generic,
  [
    ppn//1, % -PPN:atom
    ppn_resource/4, % +Graph:atom
                    % +Category:atom
                    % +PPN:atom
                    % -Individual:uri
    stcn_graph/1 % +Graph:atom
  ]
).

/** <module> STCN generic stuff

Things that are used throughout the STCN project,
but are not generic enough to be in PGC.

@author Wouter Beek
@version 2013/06, 2013/09
*/

:- use_module(dcg(dcg_ascii)). % Used in phrase/[2,3].
:- use_module(dcg(dcg_cardinal)).
:- use_module(dcg(dcg_generic)). % Used in phrase/[2,3].
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(rdf(rdf_build)).
:- use_module(rdf(rdf_serial)).
:- use_module(rdfs(rdfs_build)).
:- use_module(xml(xml_namespace)).

:- xml_register_namespace(foaf,  'http://xmlns.com/foaf/0.1/').
:- xml_register_namespace(stcn,  'http://stcn.data2semantics.org/resource/').
:- xml_register_namespace(stcnv, 'http://stcn.data2semantics.org/resource/vocab/').

:- rdf_meta(category_class(?,r)).



%! category_class(?Category:atom, ?Class:uri) is nondet.

category_class(author, foaf:'Agent').
category_class(printer_publisher, foaf:'Agent').
category_class(publications, stcnv:'Publication').
category_class(topical_keyword, stcnv:'Topic').
category_class(translator_editor, foaf:'Agent').

%! ppn// is nondet.
% There are 3*10**8 possibilities, so don't use this for generation, please.

ppn(PPN) -->
  {Codes = [_X1,_X2,_X3,_X4,_X5,_X6,_X7,_X8,_X9]},
  Codes,
  {
    forall(
      member(Code, Codes),
      phrase((digit(_) ; x), [Code])
    ),
    atom_codes(PPN, Codes)
  }.

ppn_resource(Graph, Category, PPN, Individual):-
  rdf_global_id(stcn:PPN, Individual),
  category_class(Category, Class),
  (
    rdfs_individual_of(Individual, Class), !
  ;
    rdf_assert_individual(Individual, Class, Graph)
  ).

%! stcn_graph(+Graph:atom) is semidet.

% Already loaded.
stcn_graph(Graph):-
  rdf_graph(Graph), !.
% Already in a file, load it.
stcn_graph(Graph):-
  catch(
    absolute_file_name(data(Graph), File, [access(read),file_type(rdf)]),
    _Exception,
    fail
  ),
  rdf_load2(File).

