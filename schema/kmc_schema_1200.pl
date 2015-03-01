:- module(kmc_schema_1200, []).

/** <module> Schema: KMC 1200 (Typographic properties)

@author Wouter Beek
@version 2015/02
*/

:- use_module(library(semweb/rdf_db), except([rdf_node/1])).

:- use_module(plRdf(api/rdf_build)).
:- use_module(plRdf(api/rdfs_build)).

:- use_module(stcn(kmc/kmc_1200)).

:- multifile(kmc:assert_kmc_schema/2).





kmc:assert_kmc_schema(1200, Graph):-
  % Parent class of typographic values.
  rdfs_assert_class(stcno:'TypografischKenmerk', Graph),

  % Classes of typograhic values.
  forall(
    kmc_1200_table(Category1, Label, Comment),
    (
      atomic_concat(Category1, 'TypografischKenmerk', Category2),
      rdf_global_id(stcno:Category2, Category3),
      rdfs_assert_subclass(Category3, stcno:'TypografischKenmerk', Graph),
      rdfs_assert_label(Category3, Label, Graph),
      (Comment == '', ! ; rdfs_assert_comment(Category3, Comment, Graph))
    )
  ),

  % Typographic values.
  forall(
    kmc_1200_table(Category1, Char1, Label, Comment),
    (
      atomic_concat(Category1, 'TypografischKenmerk', Category2),
      rdf_global_id(stcno:Category2, Category3),
      atomic_concat(tk_, Char1, Char2),
      rdf_global_id(stcno:Char2, Char3),
      rdf_assert_instance(Char3, Category3, Graph),
      rdfs_assert_label(Char3, Label, Graph),
      (   Comment == ''
      ->  true
      ;   rdfs_assert_comment(Char3, Comment, Graph)
      )
    )
  ),

  % Typographic value relation.
  rdf_assert_property(stcno:typographic_property, Graph),
  rdfs_assert_label(
    stcno:typographic_property,
    'typographic property',
    Graph
  ),
  rdfs_assert_label(
    stcno:typographic_property,
    [nl]-'typografische eigenschap',
    Graph
  ),
  rdf_assert_string(
    stcno:typographic_property,
    stcno:kb_name,
    'KMC 1200',
    Graph
  ),
  rdfs_assert_seeAlso(
    stcno:typographic_property,
    'http://www.kb.nl/kbhtml/stcnhandleiding/1200.html',
    Graph
  ),
  rdf_assert_langstring(
    stcno:typographic_property,
    stcno:picarta_name,
    [nl]-'Typografische informatie',
    Graph
  ),
  rdfs_assert_domain(stcno:typographic_property, stcno:'Publication', Graph),
  rdfs_assert_range(
    stcno:typographic_property,
    stcno:'TypografischKenmerk',
    Graph
  ),

  rdfs_assert_subproperty(
    stcno:boekenlijst,
    stcno:typographic_property,
    Graph
  ),
  rdfs_assert_label(stcno:boekenlijst, 'list of books', Graph),
  rdfs_assert_label(stcno:boekenlijst, [nl]-boekenlijst, Graph),
  rdfs_assert_range(
    stcno:boekenlijst,
    stcn:'TypografischKenmerk/Boekenlijsten',
    Graph
  ),

  rdfs_assert_subproperty(
    stcno:lettertype,
    stcno:typographic_property,
    Graph
  ),
  rdfs_assert_label(stcno:lettertype, 'font type', Graph),
  rdfs_assert_label(stcno:lettertype, [nl]-lettertype, Graph),
  rdfs_assert_range(
    stcno:lettertype,
    stcno:'TypografischKenmerk/Lettertype',
    Graph
  ),

  rdfs_assert_subproperty(
    stcno:illustraties,
    stcno:typographic_property,
    Graph
  ),
  rdfs_assert_label(stcno:illustraties, illustrations, Graph),
  rdfs_assert_label(stcno:illustraties, [nl]-illustraties, Graph),
  rdfs_assert_range(
    stcno:illustraties,
    stcno:'TypografischKenmerk/Illustraties',
    Graph
  ),

  rdfs_assert_subproperty(stcno:diversen, stcno:typographic_property, Graph),
  rdfs_assert_label(stcno:diversen, miscellaneous, Graph),
  rdfs_assert_label(stcno:diversen, [nl]-diversen, Graph),
  rdfs_assert_range(
    stcno:diversen,
    stcno:'TypografischKenmerk/Diversen',
    Graph
  ),

  rdfs_assert_subproperty(
    stcno:titelpagina,
    stcno:typographic_property,
    Graph
  ),
  rdfs_assert_label(stcno:titelpagina, titlepage, Graph),
  rdfs_assert_label(stcno:titelpagina, [nl]-titelpagina, Graph),
  rdfs_assert_range(
    stcno:titelpagina,
    stcno:'TypografischKenmerk/Titelpagina',
    Graph
  ).
