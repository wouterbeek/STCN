:- module(kmc_schema_0500, []).

/** <module> Schema: KMC 0500

@author Wouter Beek
@see http://www.kb.nl/kbhtml/stcnhandleiding/0500.html
@see http://support.oclc.org/ggc/richtlijnen/php/showPresentation.php?id=12&ln=nl&sec=k-0500
@see http://support.oclc.org/ggc/richtlijnen/php/showPresentation.php?id=13&ln=nl&par=p-103
@version 2015/02
*/

:- use_module(library(semweb/rdf_db), except([rdf_node/1])).

:- use_module(plRdf(api/rdf_build)).
:- use_module(plRdf(api/rdf_read)).
:- use_module(plRdf(api/rdfs_build)).

:- multifile(kmc:assert_kmc_schema/2).





kmc:assert_kmc_schema(500, G):-
  % Status values
  rdfs_assert_class(stcno:'StatusValue', G),
  rdfs_assert_label(stcno:'StatusValue', 'status value', G),
  rdfs_assert_comment(
    stcno:'StatusValue',
    'The GGC assigns meaning to each status code individually.',
    G
  ),
  rdfs_assert_seeAlso(
    stcno:'StatusValue',
    'http://support.oclc.org/ggc/richtlijnen/php/showPresentation.php?id=12&ln=nl&sec=k-0500',
    G
  ),

  % STCN status values
  rdfs_assert_subclass(stcno:'StatusValue/STCN', stcno:'StatusValue', G),
  rdfs_assert_label(stcno:'StatusValue/STCN', 'STCN status value', G),
  rdfs_assert_comment(
    stcno:'StatusValue/STCN',
    'The STCN assigns extra meaning to the three status codes combined.',
    G
  ),
  rdfs_assert_seeAlso(
    stcno:'StatusValue/STCN',
    'http://www.kb.nl/kbhtml/stcnhandleiding/0500.html',
    G
  ),

  % Monograph
  rdf_assert_instance(stcno:'Aav', stcno:'StatusValue/STCN', G),
  rdfs_assert_label(stcno:'Aav', monograph, G),
  rdfs_assert_label(stcno:'Aav', [nl]-monografie, G),

  % Multipart work
  rdf_assert_instance(stcno:'Abv', stcno:'StatusValue/STCN', G),
  rdfs_assert_label(stcno:'Abv', 'multipart work', G),
  rdfs_assert_label(stcno:'Abv', [nl]-'meerdelig werk', G),

  % Magazine
  rdf_assert_instance(stcno:'Acv', stcno:'StatusValue/STCN', G),
  rdfs_assert_label(stcno:'Acv', magazine, G),
  rdfs_assert_label(stcno:'Acv', [nl]-tijdschrift, G),

  % First position status values.
  rdfs_assert_subclass(stcno:'StatusValue/Pos1', stcno:'StatusValue', G),
  forall(
    'kmc_0500-1'(Char, Label, Comment),
    (
      kmc_0500_status_value('kmc_0500-1', Char, Value, _Pred1),
      rdf_assert_instance(Value, stcno:'StatusValue/Pos1', G),
      rdfs_assert_label(Value, [nl]-Label, G),
      rdfs_assert_comment(Value, [nl]-Comment, G)
    )
  ),

  % Second position status values.
  rdfs_assert_subclass(stcno:'StatusValue/Pos2', stcno:'StatusValue', G),
  forall(
    'kmc_0500-2'(Char, Label, Comment),
    (
      kmc_0500_status_value('kmc_0500-2', Char, Value, _Pred2),
      rdf_assert_instance(Value, stcno:'StatusValue/Pos2', G),
      rdfs_assert_label(Value, [nl]-Label, G),
      rdfs_assert_comment(Value, [nl]-Comment, G)
    )
  ),

  % Third position status values.
  rdfs_assert_subclass(stcno:'StatusValue/Pos3', stcno:'StatusValue', G),
  rdfs_assert_label(stcno:'StatusValue/Pos3', 'PAR 103 status value', G),
  rdfs_assert_comment(
    stcno:'StatusValue/Pos3',
    'Status values as defined by PAR 103.',
    G
  ),
  forall(
    par_103(Char, Label, Comment),
    (
      kmc_0500_status_value(par_103, Char, Value, _Pred3),
      rdf_assert_instance(Value, stcno:'StatusValue/Pos3', G),
      rdfs_assert_label(Value, [nl]-Label, G),
      rdfs_assert_comment(Value, [nl]-Comment, G)
    )
  ),

  % The status relationship.
  rdf_assert_property(stcno:status, G),
  rdfs_assert_label(stcno:status, status, G),
  rdfs_assert_label(stcno:status, [nl]-status, G),
  rdf_assert_string(stcno:status, stcno:kb_name, 'KMC 0500', G),
  rdfs_assert_seeAlso(
    stcno:status,
    'http://www.kb.nl/kbhtml/stcnhandleiding/0500.html',
    G
  ),
  rdfs_assert_domain(stcno:status, stcno:'Publication', G),
  rdfs_assert_range(stcno:status, stcno:'StatusValue', G),

  % Status 1 property.
  rdfs_assert_subproperty(stcno:status_pos1, stcno:status, G),
  rdfs_assert_label(stcno:status_pos1, [nl]-'status positie 1', G),
  rdfs_assert_comment(
    stcno:status_pos1,
    [nl]-'Algemene inhoudsaanduiding.',
    G
  ),
  rdfs_assert_seeAlso(
    stcno:status_pos1,
    'http://support.oclc.org/ggc/richtlijnen/php/showPresentation.php?id=12&ln=nl&sec=k-0500#positie_1_kmc_0500',
    G
  ),

  % Status 2 property.
  rdfs_assert_subproperty(stcno:status_pos2, stcno:status, G),
  rdfs_assert_label(stcno:status_pos2, [nl]-'status positie 2', G),
  rdfs_assert_comment(
    stcno:status_pos2,
    [nl]-'Het correct invullen van positie 2 van kmc 0500 is van groot \c
          belang. \c
          Wijzig positie 2 alleen als dat noodzakelijk is en breng bij een \c
          belangrijke wijziging (bijv. \'a\' wordt veranderd in \'b\') de \c
          medegebruikers van de titel via Picamail op de hoogte
          (zie PAR 052).',
     G
  ),
  rdfs_assert_seeAlso(
    stcno:status_pos2,
    'http://support.oclc.org/ggc/richtlijnen/php/showPresentation.php?id=12&ln=nl&sec=k-0500#positie_2_kmc_0500',
    G
  ),

  % Status 3 property.
  rdfs_assert_subproperty(stcno:status_pos3, stcno:status, G),
  rdfs_assert_label(stcno:status_pos3, [nl]-'status positie 3', G),
  rdf_assert_string(stcno:status_pos3, stcno:kb_name, 'PAR 103', G),
  rdfs_assert_seeAlso(
    stcno:status_pos3,
    'http://support.oclc.org/ggc/richtlijnen/php/showPresentation.php?id=13&ln=nl&par=p-103',
    G
  ).
