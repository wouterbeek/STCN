:- module(
  stcn_void,
  [
    stcn_void/1 % +Graph:atom
  ]
).

/** <module> STCN VoID

Generates the VoID description of the STCN dataset.

@author Wouter Beek
@version 2013/10, 2014/03, 2015/02
*/

:- use_module(library(semweb/rdf_db), except([rdf_node/1])).

:- use_module(plc(os/datetime_ext)).

:- use_module(plRdf(api/rdf_build)).
:- use_module(plRdf(api/rdf_read)).
:- use_module(plRdf(api/rdfs_build)).
:- use_module(plRdf(api/rdfs_read)).
:- use_module(plRdf(term/rdf_datatype)).





stcn_void(G):-
  % STCN dataset
  rdf_assert_instance(stcn:'STCN', void:'DatasetDescription', G),
  
  % Contributors.
  % Albert
  rdf_assert_instance(stcn:'AlbertMeroñoPeñuela', foaf:'Person', G),
  rdfs_assert_label(
    stcn:'AlbertMeroñoPeñuela',
    [sp]-'Albert Meroño Peñuela',
    G
  ),
  rdf_assert(
    stcn:'AlbertMeroñoPeñuela',
    foaf:mbox,
    'mailto:albert.merono@vu.nl',
    G
  ),
  rdf_assert(stcn:'STCN', dcterms:contributor, stcn:'AlbertMeroñoPeñuela', G),
  % Rinke
  rdf_assert_instance(stcn:'RinkeHoekstra', foaf:'Person', G),
  rdfs_assert_label(stcn:'RinkeHoekstra', [nl]-'Rinke Hoekstra', G),
  rdf_assert(
    stcn:'RinkeHoekstra',
    foaf:mbox,
    'mailto:rinke.hoekstra@vu.nl',
    G
  ),
  rdf_assert(stcn:'STCN', dcterms:contributor, stcn:'RinkeHoekstra', G),
  % Wouter
  rdf_assert_instance(stcn:'WouterBeek', foaf:'Person', G),
  rdfs_assert_label(stcn:'WouterBeek', [nl]-'Wouter Beek', G),
  rdf_assert(stcn:'WouterBeek', foaf:mbox, 'mailto:me@wouterbeek.com', G),
  rdf_assert(stcn:'STCN', dcterms:contributor, stcn:'WouterBeek', G),
  
  % Creation time.
  get_time(POSIX_TS),
  posix_timestamp_to_xsd_dateTime(POSIX_TS, XSD_DT),
  rdf_assert_typed_literal(
    stcn:'STCN',
    dcterms:created,
    XSD_DT,
    xsd:dateTime,
    G
  ),
  
  % Title
  rdf_assert_langstring(
    stcn:'STCN',
    dcterms:title,
    'Short-Title Catalogue, Netherlands',
    G
  ),
  
  % Description
  rdf_assert_langstring(
    stcn:'STCN',
    dcterms:description,
    [nl]-'De STCN (Short Title Catalogue Netherlands) is een lijst \c
          beschrijvingen van voor 1 januari 1801 gedrukte Nederlandse
          boeken.',
    G
  ),
  
  % Vocabulary namespaces.
  rdf_current_prefix(stcn, StcnNs),
  rdf_current_prefix(stcno, StcnoNs),
  rdf_assert(stcn:'STCN', void:vocabulary, StcnNs, G),
  rdf_assert(stcn:'STCN', void:vocabulary, StcnoNs, G),
  
  % VoID Feature: RDF serialization format.
  % @tbd Automate this based on rdf_save/3.
  rdf_assert(stcn:'STCN', void:feature, format:'Turtle', G),
  
  % SPARQL endpoint.
  rdf_assert(
    stcn:'STCN',
    void:sparqlEndpoint,
    'http://semanticweb.cs.vu.nl/prasem/sparql/',
    G
  ),
  
  % Web pages.
  rdf_assert(
    stcn:'STCN',
    foaf:homepage,
    'http://www.kb.nl/kbhtml/stcnhandleiding/frames.html',
    G
  ),
  rdf_assert(
    stcn:'STCN',
    foaf:page,
    'http://support.oclc.org/ggc/richtlijnen/',
    G
  ),
  
  % Example resource
  % @tbd What is this?
  rdf_assert(stcn:'STCN', void:exampleResource, stcno:'Publication', G),
  
  stcn_void_authors(G),
  rdf_assert(stcn:'STCN', void:'ClassPartition', stcn:'STCN_Authors', G),
  stcn_void_printers(G),
  rdf_assert(stcn:'STCN', void:'ClassPartition', stcn:'STCN_Printers', G),
  stcn_void_publications(G),
  rdf_assert(stcn:'STCN', void:'ClassPartition', stcn:'STCN_Publications', G),
  stcn_void_topics(G),
  rdf_assert(stcn:'STCN', void:'ClassPartition', stcn:'STCN_Topics', G),
  
  stcn_void_dbpedia(G),
  rdf_assert(stcn:'STCN', void:subset, stcn:'DBpedia_STCN', G),
  rdf_assert(stcn:'STCN', void:subset, stcn:'STCN2DBpedia', G),
  stcn_void_stcnv(G),
  rdf_assert(stcn:'STCN', void:subset, stcn:'STCNV', G),
  
  % @tbd Relate this to stcn:'STCN'.
  stcn_void_authors2dbpedia(G).

stcn_void_authors(G):-
  rdf_assert_instance(stcn:'STCN_Authors', void:'Dataset', G),
  % Title.
  rdf_assert_langstring(
    stcn:'STCN_Authors',
    dcterms:title,
    'STCN authors',
    G
  ),
  % Description.
  rdf_assert_langstring(
    stcn:'STCN_Authors',
    dcterms:description,
    'The authors in the STCN dataset.',
    G
  ),
  % Main class.
  rdf_assert(stcn:'STCN_Authors', void:class, stcno:'Author', G),
  % Datadump location.
  rdf_assert(stcn:'STCN_Authors', void:dataDump, 'STCN_Authors.ttl', G).

stcn_void_authors2dbpedia(G):-
  rdf_assert_instance(stcn:'STCN_Authors2DBpedia', void:'Linkset', G),
  % Title.
  rdf_assert_langstring(
    stcn:'STCN_Authors2DBpedia',
    dcterms:title,
    'STCN author, DBpedia entry mappings.',
    G
  ),
  % Description.
  rdf_assert_langstring(
    stcn:'STCN_Authors2DBpedia',
    dcterms:description,
    'Identity mappings between STCN authors and DBpedia entries.',
    G
  ),
  % Datadump location.
  rdf_assert(
    stcn:'STCN_Authors2DBpedia',
    void:dataDump,
    'STCN_Authors2DBpedia.ttl',
    G
  ),
  % Link predicate.
  rdf_assert(stcn:'STCN_Authors2DBpedia', void:linkPredicate, owl:sameAs, G),
  % From target.
  rdf_assert(
    stcn:'STCN_Authors2DBpedia',
    void:target,
    stcn:'STCN_Authors',
    G
  ),
  % To target,
  rdf_assert(
    stcn:'STCN_Authors2DBpedia',
    void:target,
    stcn:'DBpedia_STCN',
    G
  ).

stcn_void_dbpedia(G):-
  % DBpedia
  rdf_assert_instance(stcn:'DBpedia_STCN', void:'Dataset', G),
  % Title
  rdf_assert_langstring(
    stcn:'DBpedia_STCN',
    dcterms:title,
    'DBpedia for STCN dataset',
    G
  ),
  % Description
  rdf_assert_langstring(
    stcn:'DBpedia_STCN',
    dcterms:description,
    'The subset of the DBpedia that is relevant for the STCN dataset.',
    G
  ),
  % Datadump
  rdf_assert(stcn:'DBpedia_STCN', void:dataDump, 'DBpedia_STCN.ttl', G),
  % VoID feature: RDF serialization format.
  rdf_assert(stcn:'DBpedia_STCN', void:feature, format:'Turtle', G),
  % Web page
  rdf_assert(stcn:'DBpedia_STCN', foaf:page, 'http://dbpedia.org/About', G).

stcn_void_printers(G):-
  rdf_assert_instance(stcn:'STCN_Printers', void:'Dataset', G),
  % Title.
  rdf_assert_langstring(
    stcn:'STCN_Printers',
    dcterms:title,
    'STCN printers, publishers and booksellers.',
    G
  ),
  % Description.
  rdf_assert_langstring(
    stcn:'STCN_Printers',
    dcterms:description,
    'The printers, publishers and booksellers in the STCN dataset.',
    G
  ),
  % Main class.
  rdf_assert(stcn:'STCN_Printers', void:class, stcno:'Printer', G),
  % Datadump location.
  rdf_assert(stcn:'STCN_Printers', void:dataDump, 'STCN_Printers.ttl', G).

stcn_void_publications(G):-
  rdf_assert_instance(stcn:'STCN_Publications', void:'Dataset', G),
  rdf_assert_langstring(
    stcn:'STCN_Publications',
    dcterms:description,
    'The publications in the STCN dataset.',
    G
  ),
  rdf_assert_langstring(
    stcn:'STCN_Publications',
    dcterms:title,
    'STCN publications',
    G
  ),
  % Main class
  rdf_assert(stcn:'STCN_Publications', void:class, stcno:'Publication', G),
  % Datadump location.
  rdf_assert(
    stcn:'STCN_Publications',
    void:dataDump,
    'STCN_Publications.ttl',
    G
  ).

stcn_void_stcnv(G):-
  rdf_assert_instance(stcn:'STCNV', void:'Dataset', G),
  % Titles
  rdf_assert_langstring(stcn:'STCNV', dcterms:title, 'STCN vocabulary', G),
  rdf_assert_langstring(
    stcn:'STCNV',
    dcterms:title,
    [nl]-'STCN vocabularium',
    G
  ),
  % Descriptions
  rdf_assert_langstring(
    stcn:'STCNV',
    dcterms:description,
    'The vocabulary for the STCN dataset.',
    G
  ),
  rdf_assert_langstring(
    stcn:'STCNV',
    dcterms:description,
    [nl]-'Het vocabularium voor de STCN dataset.',
    G
  ),
  % Datadump location.
  rdf_assert(stcn:'STCNV', void:dataDump, 'STCNV.ttl', G).

stcn_void_topics(G):-
  rdf_assert_instance(stcn:'STCN_Topics', void:'Dataset', G),
  % Title
  rdf_assert_langstring(stcn:'STCN_Topics', dcterms:title, 'STCN topics', G),
  % Description.
  rdf_assert_langstring(
    stcn:'STCN_Topics',
    dcterms:description,
    'The topics that occur in the STCN dataset.',
    G
  ),
  % Main class.
  rdf_assert(stcn:'STCN_Topics', void:class, stcno:'Topic', G),
  % Datadump location.
  rdf_assert(stcn:'STCN_Topics', void:dataDump, 'STCN_Topics.ttl', G).

