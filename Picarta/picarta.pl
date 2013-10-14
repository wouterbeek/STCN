:- module(
  picarta,
  [
    assert_schema_picarta/1, % +G:atom

% PROCESSING
    process_life_years/1, % +G:atom
    process_name_normals/1, % +G:atom
    process_professions/1, % +G:atom
    process_pseudonyms/1, % +G:atom

% SCRAPING
    scrape_picarta/2, % +PicartaGraph:atom
                      % +Class:oneof([stcnv:'Author',stcnv:'Printer',stcnv:'Publication',stcnv:'Topic'])
    scrape_picarta_progress/2 % +PicartaGraph:atom
                              % +STCN_Graph:atom
  ]
).

/** <module> Picarta

We make a distinction between three portions of code in this module:
    * The processing of previously scraped results.
    * Querying / retrieving of a single PPN.
    * Scraping the entire Picarta, using per-PPN queries in multiple threads.

# STCN information that is not in Picarta

    * KMC 0500, publication type.
    * KMC 1500, languages of publication.
    * KMC 1700, country of publication.

@author Wouter Beek
@version 
*/

:- use_module(generic(atom_ext)).
:- use_module(generic(list_ext)).
:- use_module(generic(meta_ext)).
:- use_module(generic(parse_ext)).
:- use_module(generic(thread_ext)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(library(xpath)).
:- use_module(owl(owl_build)).
:- use_module(rdf(rdf_build)).
:- use_module(rdf(rdf_namespace)).
:- use_module(rdf(rdf_read)).
:- use_module(rdf(rdf_stat)).
:- use_module(rdfs(rdfs_build)).
:- use_module(rdfs(rdfs_read)).
:- use_module(skos(skos_build)).
:- use_module(standards(html)).

:- rdf_meta(assert_schema_picarta(+,r)).
:- rdf_meta(scrape_picarta(+,r)).
:- rdf_meta(translate_profession(+,r)).

:- rdf_register_namespace(foaf).
:- rdf_register_namespace(rdf).
:- rdf_register_namespace(rdfs).
:- rdf_register_namespace(stcn, 'http://stcn.data2semantics.org/resource/').
:- rdf_register_namespace(stcnv, 'http://stcn.data2semantics.org/vocab/').
:- rdf_register_namespace(xsd).

:- debug(picarta).



% PROCESSING: LIFE YEARS %

%! process_life_years(+G:atom) is det.
% Processes the life years attribute of the Picarta dataset: from a string
% literal to a more explicit assertion.

process_life_years(G):-
  setoff(
    Agent/LifeYears,
    rdf_datatype(Agent, stcn:life_years, string, LifeYears, G),
    Pairs
  ),
  run_on_sublists(Pairs, process_life_years(G)).

process_life_years(G, Pairs):-
  maplist(process_lifeyears1(G), Pairs).

process_lifeyears1(G, Agent/LifeYears):-
  atom_codes(LifeYears, LifeYearsCodes),
  parse_date(Birth/Death, LifeYearsCodes-[]),
  rdf_assert_datatype(Agent, stcnv:birth, gYear, Birth, G),
  rdf_assert_datatype(Agent, stcnv:death, gYear, Death, G),
  rdf_retractall_datatype(Agent, stcn:life_years, string, G).



% PROCESSING: NAME NORMAL %

process_name_normals(G):-
  setoff(
    Agent/UnparsedName,
    rdf_datatype(Agent, stcn:name_normal, string, UnparsedName, G),
    Pairs
  ),
  run_on_sublists(Pairs, process_name_normals(G)).

process_name_normals(G, Pairs):-
  maplist(process_name_normal(G), Pairs).

process_name_normal(G, Agent/UnparsedName):-
  parse_name_normal(UnparsedName, ParsedName),
  rdf_assert_datatype(Agent, foaf:name, string, ParsedName, G),
  rdf_retractall_datatype(Agent, stcn:name_normal, string, G).

parse_name_normal(UnparsedName, ParsedName):-
  sub_atom(
    UnparsedName,
    EndOfLastNames,
    LengthOfSeparator,
    LengthOfFirstNames,
    ', '
  ),
  !,
  BeginOfFirstNames is EndOfLastNames + LengthOfSeparator,
  sub_atom(
    UnparsedName,
    BeginOfFirstNames,
    LengthOfFirstNames,
    0,
    FirstNames
  ),
  sub_atom(UnparsedName, 0, EndOfLastNames, _LengthOfLastNames, LastNames),
  atomic_list_concat([FirstNames, LastNames], ' ', ParsedName).
parse_name(AuthorName, AuthorName).



% PROCESSING: PROFESSION %

%! assert_schema_profession(+G:atom) is det.
% This schema is needed in a graph in which Picarta professions will be
% processed.

assert_schema_profession(G):-
  rdfs_assert_class(stcnv:'Profession', G),
  forall(
    profession(URI),
    rdf_assert_individual(URI, stcnv:'Profession', G)
  ),
  rdf_assert_property(stcn:has_profession, G),
  rdf_assert_property(stcn:profession,     G),
  rdf_assert_property(stcn:active,         G),
  rdf_assert_property(stcn:active_start,   G),
  rdf_assert_property(stcn:active_end,     G),
  rdfs_assert_subproperty(stcn:active_start, stcn:active, G),
  rdfs_assert_subproperty(stcn:active_end,   stcn:active, G).

process_professions(G):-
  assert_schema_profession(G),
  setoff(
    Agent/Profession,
    rdf_datatype(Agent, stcn:profession, string, Profession, G),
    Pairs
  ),
  run_on_sublists(Pairs, process_professions(G)).

process_professions(G, Pairs):-
  maplist(process_profession(G), Pairs).

process_profession(G, Agent/Literal):-
  split_atom_exclusive(Literal, ', ', [ProfessionName|YearNames]),
  translate_profession(ProfessionName, Profession),
  process_profession(Agent, Profession, YearNames, G).

process_profession(Agent, Profession, ['-'], G):- !,
  rdf_assert(Agent, stcn:profession, Profession, G),
  rdf_retractall_datatype(Agent, stcn:profession, string, G).
process_profession(Agent, Profession, YearNames, G):- !,
  rdf_bnode(HasProfession),
  rdf_assert(Agent, stcn:has_profession, HasProfession, G),
  rdf_assert(HasProfession, stcn:profession, Profession, G),
  findall(
    YearName,
    (
      member(YearName, YearNames),
      atom_codes(YearName, C1),
      (
        parse_date(Point, C1-[])
      ->
        rdf_assert_datatype(Agent, stcn:active, gYear, Point, G)
      ;
        parse_date(Begin, End, C1-[])
      ->
        rdf_assert_datatype(Agent, stcn:active_start, gYear, Begin, G),
        rdf_assert_datatype(Agent, stcn:active_end, gYear, End, G)
      )
    ),
    _YearNames1
  ).
/*
  % When all life_years are explicitly asserted, the implicit literal can be
  % removed from the graph.
  (
    length(YearNames, Length),
    length(YearNames1, Length)
  ->
    rdf_retractall_datatype(Agent, stcn:profession, string, G)
  ;
    true
  ).
*/

profession(URI):-
  translate_profession(_Name, URI).

translate_profession(bookseller, URI):-
  rdf_global_id(stcn:bookseller, URI).
translate_profession('paper seller', URI):-
  rdf_global_id(stcn:paper_seller, URI).
translate_profession(printer, URI):-
  rdf_global_id(stcnv:printer, URI).



% PROCESSING: PSEUDONYMS %

process_pseudonyms(G):-
  forall(
    (
      rdf_datatype(Agent1, stcnv:pseudonym, string, Pseudonym, G),
      rdf_datatype(Agent2, stcn:author_name, string, Pseudonym, G)
    ),
    (
      owl_assert_resource_identity(Agent1, Agent2, G),
      debug(picarta, '~w is a pseudonym of ~w.', [Agent1, Agent2])
    )
  ).



% PROCESSING : TOPICS HIERARCHY %

export_topics_hierarchy:-
  rdf_global_id(stcnv:'Topic', Root),
  rdf_global_id(skos:broader, Predicate),
  beam([], Root, [Predicate], Topics, _Edges),
  findall(
    [Label, Size],
    (
      member(Topic, Topics),
      topic_label(Topic, Label),
      topic_size(Topic, Size)
    ),
    Rows
  ),
  write(Rows).

%! process_topics_hierarchy(+G:atom) is det.
% Constructs the topics hierarchy. This is added to the graph in wich these
% topics reside.
%
% This method uses a copied version of trees:all_subpaths_to_tree/2,
% adapted to work with pairs.
%
% @param G The atomic name of the graph containing the topic resources.

process_topics_hierarchy(G):-
  setoff(
    List/Topic,
    (
      rdfs_individual_of(Topic, stcnv:'Topic'),
      rdf_datatype(Topic, stcn:synonym, string, TopicCode1, G),
      atom_codes(TopicCode1, TopicCode2),
      contains([], [decimal_digit], TopicCode2-[]),
      split_atom_exclusive(TopicCode1, ['.', ' '], List)
    ),
    Pairs
  ),
  rdf_global_id(stcn:'Topic', RootTopic),
  rdf_global_id(stcn:'dummy', Dummy),
  subtopics([['15','70']/Dummy | Pairs], []/RootTopic, Tree),
  skos_assert_hierarchy(Tree, stcn:'TopicScheme', G).

subtopics(AllPairs, List/Topic, Topic-Trees):-
  setoff(
    Tree,
    (
      member(LongerList/SubTopic, AllPairs),
      append(List, [_], LongerList),
      subtopics(AllPairs, LongerList/SubTopic, Tree)
    ),
    Trees
  ).

topic_label(Topic, Label):-
  once(rdf_datatype(Vertex, stcn:synonym, string, Label, _Graph1)).

topic_size(Topic, Size):-
  beam([], Topic, [Predicate], SubTopics, _SubEdges),
  setoff(
    Publication,
    (
      member(SubVertex, SubVertices),
      rdf(Publication, stcnv:topic, SubVertex)
    ),
    Publications
  ),
  length(Publications, Size).



% SCRAPING %

%! scrape_picarta(+G:atom, +Class:uri) is det.
% Perform a Picarta Web scrape for individuals of the given class.
%
% This predicate uses multi-threading to perform its task.
%
% @param G The atomic name of the Picarta graph.
% @param Class

scrape_picarta(G, Class):-
  setoff(
    Individual,
    rdfs_individual_of(Individual, Class),
    Individuals
  ),
  length(Individuals, Length),
  debug(
    picarta,
    'About to scrape ~w individuals of type ~w.\n',
    [Length, Class]
  ),
  run_on_sublists(Individuals, scrape_picarta1(G, Class)).

%! scrape_picarta_progress(+PicartaGraph:atom, +STCN_Graph:atom) is det.
% Sends scraping statistics to the debug console.
%
% @param PicartaGraph The atomic name of the Picarta graph.
% @param STCN_Graph The atomic name of the STCN graph.

scrape_picarta_progress(PicartaGraph, STCN_Graph):-
  count_individuals(stcnv:'Author', PicartaGraph, X1),
  count_individuals(stcnv:'Author', STCN_Graph, Y1),
  debug(picarta, 'Authors: ~w of ~w.\n', [X1, Y1]),
  count_individuals(stcnv:'Printer', PicartaGraph, X2),
  count_individuals(stcnv:'Printer', STCN_Graph, Y2),
  debug(picarta, 'Printers: ~w of ~w.\n', [X2, Y2]),
  count_individuals(stcnv:'Publication', PicartaGraph, X3),
  count_individuals(stcnv:'Publication', STCN_Graph, Y3),
  debug(picarta, 'Publications: ~w of ~w.\n', [X3, Y3]),
  count_individuals(stcnv:'Topic', PicartaGraph, X4),
  count_individuals(stcnv:'Topic', STCN_Graph, Y4),
  debug(picarta, 'Topics: ~w of ~w.\n', [X4, Y4]).

