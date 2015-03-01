:- module(kmc_schema_1500, []).

/** <module> Schema: KMC 1500

@author Wouter Beek
@version 2015/02
*/

:- use_module(library(semweb/rdf_db), except([rdf_node/1])).

:- use_module(plRdf(api/rdf_build)).
:- use_module(plRdf(api/rdfs_build)).
:- use_module(plRdf(management/rdf_load_any)).

:- multifile(kmc:assert_kmc_schema/2).





kmc:assert_kmc_schema(1500, Graph):-
  rdf_assert_property(stcno:language, G),
  rdfs_assert_label(stcno:language, language, G),
  rdfs_assert_label(stcno:language, [nl]-taal, G),
  rdf_assert_string(stcno:language, stcno:kb_name, 'KMC 1500', G),
  rdfs_assert_seeAlso(
    stcno:language,
    'http://www.kb.nl/kbhtml/stcnhandleiding/1500.html',
    G
  ),
  rdfs_assert_seeAlso(
    stcno:language,
    'http://support.oclc.org/ggc/richtlijnen/?id=12&ln=nl&sec=k-1500',
    G
  ),

  rdf_assert_instance(stcno:actual_language, stcno:'LanguageProperty', G),
  rdfs_assert_label(stcno:actual_language, 'actual language', G),
  rdfs_assert_label(stcno:actual_language, [nl]-'daadwerkelijke taal', G),
  rdfs_assert_subproperty(stcno:actual_language, stcno:language, G),

  rdfs_assert_subproperty(stcno:translated_via, stcno:language, G),
  rdfs_assert_label(stcno:actual_language, 'translated via', G),
  rdfs_assert_label(stcno:translated_via, [nl]-'vertaald via', G),

  rdfs_assert_subproperty(stcno:translated_from, stcno:language, G),
  rdfs_assert_label(stcno:actual_language, 'translated from', G),
  rdfs_assert_label(stcno:translated_from, [nl]-'vertaald uit', G),
  
  rdf_load_any(stcn('rdf/iso639-3.ttl'), [format(turtle),graph('iso639-3')]),
  % Use OCLC information to add Dutch labels to ISO 639-3 language codes.
  forall(
    (
      rdfs_individual_of(Lang1, 'iso639-3':'Language'),
      rdf_global_id('iso639-3':Lang2, Lang1)
    ),
    (   recognized_language(Lang2, Name)
    ->  rdfs_assert_label(Lang1, [nl]-Name, G)
    ;   true
    )
  ),
  rdf_load_any(stcn('rdf/iso639-5.ttl'), [format(turtle),graph('iso639-5')]).
