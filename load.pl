% The load file for the STCN project.

:- use_module(library(ansi_term)).

:- dynamic(user:project/3).
:- multifile(user:project/3).
user:project('STCN', 'Short Title Catalogue of the Netherlands', stcn).

:- use_module(load_project).
:- load_project([
  lodCache,
  mt-'ModelTheory',
  plc-'Prolog-Library-Collection',
  plGraph,
  plGraphDraw,
  plGraphViz,
  plHtml,
  plHttp,
  plLangTag,
  plLattice,
  plLatticeDraw,
  plNlp,
  plRdf,
  plRdfDraw,
  plRdfEntailment,
  plRdfHtml,
  plSet,
  plSparql,
  plSvg,
  plTms,
  plTree,
  plTreeDraw,
  plUri,
  plXml,
  plXsd
]).


:- use_module(library(semweb/rdf_db), except([rdf_node/1])).

:- rdf_register_prefix(formats, 'http://www.w3.org/ns/formats/').
:- rdf_register_prefix('iso3166-1', 'http://lexvo.org/id/iso3166/').
:- rdf_register_prefix('iso639-1', 'http://www.wouterbeek.com/langtag/iso639-1/').
:- rdf_register_prefix('iso639-2', 'http://www.wouterbeek.com/langtag/iso639-2/').
:- rdf_register_prefix('iso639-3', 'http://www.wouterbeek.com/langtag/iso639-3/').
:- rdf_register_prefix('iso639-5', 'http://www.wouterbeek.com/langtag/iso639-5/').
:- rdf_register_prefix(picarta, 'http://picarta.pica.nl/').
:- rdf_register_prefix(stcn, 'http://stcn.org/resource/').
:- rdf_register_prefix(stcno, 'http://stcn.org/ontology/').


:- use_module(library(filesex)).

:- initialization(set_rdf_subdirectory).

set_rdf_subdirectory:-
  absolute_file_name(stcn(.), Dir, [access(write),file_type(directory)]),
  directory_file_path(Dir, rdf, RdfDir),
  make_directory_path(RdfDir).


:- use_module(stcn(stcn_script)).
