:- module(kmc_schema_30xx, []).

/** <module> Schema: KMC 3000-3099

@author Wouter Beek
@see http://www.kb.nl/kbhtml/stcnhandleiding/0500.html
@see http://support.oclc.org/ggc/richtlijnen/php/showPresentation.php?id=12&ln=nl&sec=k-0500
@see http://support.oclc.org/ggc/richtlijnen/php/showPresentation.php?id=13&ln=nl&par=p-103
@version 2015/02
*/

:- use_module(library(semweb/rdf_db), except([rdf_node/1])).

:- use_module(plRdf(api/owl_build)).
:- use_module(plRdf(api/rdf_build)).
:- use_module(plRdf(api/rdfs_build)).

:- multifile(kmc:assert_kmc_schema/2).





kmc:assert_kmc_schema(KmcCode, Graph):-
  between(3000, 3099, KmcCode), !,
  
  % Part A: KMC 3000-3099
  % Author.
  rdfs_assert_subclass(stcno:'Author', foaf:'Agent', Graph),
  rdfs_assert_label(stcno:'Author', author, Graph),
  rdfs_assert_label(stcno:'Author', [nl]-auteur, Graph),
  % Has author.
  rdf_assert_property(stcno:author, Graph),
  rdfs_assert_label(stcno:author, 'has author', Graph),
  rdfs_assert_label(stcno:author, [nl]-'heeft auteur', Graph),
  % Author name.
  rdf_assert_property(stcn:author_name, Graph),
  rdfs_assert_label(stcno:author_name, 'has author name', Graph),
  rdfs_assert_label(stcno:author_name, [nl]-'heeft auteursnaam', Graph),
  
  % Part B: KMC 3000
  rdfs_assert_subproperty(stcno:primary_author, stcno:author, Graph),
  rdfs_assert_label(stcno:primary_author, 'primary author', Graph),
  rdfs_assert_label(stcno:primary_author, [nl]-'primaire autheur', Graph),
  rdf_assert_string(stcno:primary_author, stcno:kb_name, 'KMC 3000', Graph),
  owl_assert_identity(stcno:primary_author, stcno:'3000', Graph),
  rdfs_assert_seeAlso(
    stcno:primary_author,
    'http://www.kb.nl/kbhtml/stcnhandleiding/3000.html',
    Graph
  ),
  rdf_assert_langstring(
    stcno:primary_author,
    stcno:picarta_name,
    [nl]-'Auteur',
    Graph
  ),
  
  % Part C: KMC 3011
  rdfs_assert_subproperty(stcno:secondary_author, stcno:author, Graph),
  rdfs_assert_label(stcno:secondary_author, 'secundairy author', Graph),
  rdfs_assert_label(stcno:secondary_author, [nl]-'secundaire auteur', Graph),
  rdf_assert_string(stcno:secondary_author, stcno:kb_name, 'KMC 301X', Graph),
  owl_assert_identity(stcno:secondary_author, stcno:'3011', Graph),
  rdfs_assert_seeAlso(
    stcno:secondary_author,
    'http://www.kb.nl/kbhtml/stcnhandleiding/301X.html',
    Graph
  ),
  rdf_assert_langstring(
    stcno:secondary_author,
    stcno:picarta_name,
    [nl]-'Vertaler / Bewerker',
    Graph
  ).
