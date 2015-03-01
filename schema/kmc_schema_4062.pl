:- module(kmc_schema_4062, []).

/** <module> Schema: KMC 4062

@author Wouter Beek
@version 2015/02
*/

:- use_module(library(semweb/rdf_db), except([rdf_node/1])).

:- use_module(plRdf(api/rdf_build)).
:- use_module(plRdf(api/rdf_read)).
:- use_module(plRdf(api/rdfs_build)).

:- use_module(stcn(kmc/kmc_4062)).

:- multifile(kmc:assert_kmc_schema/2).





kmc:assert_kmc_schema(4062, Graph):-
  rdfs_assert_class(stcno:'FormatValue', Graph),
  rdfs_assert_label(stcno:'FormatValue', [nl]-'formaat waarde', Graph),
  
  rdfs_assert_subclass(
    stcno:'PrimaryFormatValue',
    stcno:'FormatValue',
    Graph
  ),
  rdfs_assert_label(
    stcno:'PrimaryFormatValue',
    [nl]-'eerste formaat waarde',
    Graph
  ),
  
  rdfs_assert_subclass(
    stcno:'SecondaryFormatValue',
    stcno:'FormatValue',
    Graph
  ),
  rdfs_assert_label(
    stcno:'SecondaryFormatValue',
    [nl]-'tweede formaat waarde',
    Graph
  ),
  
  rdf_assert_property(stcno:image, Graph),
  rdfs_assert_label(stcno:image, [nl]-'heeft afbeelding', Graph),
  
  rdf_assert_property(stcno:format, Graph),
  rdfs_assert_label(stcno:format, [nl]-'heeft formaat', Graph),
  rdf_assert_string(stcno:format, stcno:kb_name, 'KMC 4062', Graph),
  rdf_assert(
    stcno:format,
    stcno:documentatie,
    'http://www.kb.nl/kbhtml/stcnhandleiding/4062.html',
    Graph
  ),
  rdf_assert_langstring(
    stcno:format,
    stcno:picarta_name,
    [nl]-'formaat',
    Graph
  ),
  
  forall(
    format(FormatID, _, _, _, _, _),
    (
      rdf_global_id(stcn:FormatID, Format),
      rdf_assert_instance(Format, stcno:'PrimaryFormatValue', Graph),
      rdfs_assert_label(Format, [nl]-FormatID, Graph),
      absolute_file_name(
        images(FormatID),
        Image,
        [access(read),file_type(png)]
      ),
      rdf_assert_string(Format, stcno:image, Image, Graph)
    )
  ),
  rdf_assert_instance(stcno:agenda, stcno:'SecondaryFormatValue', Graph),
  rdfs_assert_label(stcno:agenda, [nl]-agenda, Graph),
  
  rdf_assert_instance(stcno:broadsheet, stcno:'SecondaryFormatValue', Graph),
  rdfs_assert_label(stcno:broadsheet, [nl]-broadsheet, Graph),
  
  rdf_assert_instance(stcno:oblong, stcno:'SecondaryFormatValue', Graph),
  rdfs_assert_label(stcno:oblong, [nl]-oblong, Graph),
  
  rdf_assert_instance(stcno:other_format, stcno:'FormatValue', Graph),
  rdfs_assert_label(stcno:other_format, [nl]-'ander formaat', Graph),
  
  rdf_assert_instance(stcno:unknown_format, stcno:'FormatValue', Graph),
  rdfs_assert_label(stcno:unknown_format, [nl]-'onbekend formaat', Graph).
