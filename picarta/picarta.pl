:- module(
  picarta,
  [
% PROCESSING
    process_life_years/1, % +G:atom
    process_name_normals/1, % +G:atom
    process_professions/1, % +G:atom
    process_pseudonyms/1, % +G:atom

% SCRAPING
    scrape_picarta/2, % +PicartaGraph:atom
                      % +Class:oneof([stcno:'Author',stcno:'Printer',stcno:'Publication',stcno:'Topic'])
    scrape_picarta_progress/2 % +PicartaGraph:atom
                              % +StcnGraph:atom
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
@version 2013/01-2013/04, 2013/06, 2013/09-2013/10, 2014/03, 2015/02
*/

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(debug)).
:- use_module(library(lists), except([delete/3,subset/2])).
:- use_module(library(semweb/rdf_db), except([rdf_node/1])).
:- use_module(library(semweb/rdfs)).
:- use_module(library(xpath)).

:- use_module(plc(generics/atom_ext)).
:- use_module(plc(generics/lambda_meta)).
:- use_module(plc(generics/list_ext)).
:- use_module(plc(process/thread_ext)).

:- use_module(plRdf(api/owl_build)).
:- use_module(plRdf(api/rdf_build)).
:- use_module(plRdf(api/rdf_read)).
:- use_module(plRdf(api/rdfs_build)).
:- use_module(plRdf(api/rdfs_read)).
:- use_module(plRdf(vocabulary/rdf_stat)).

:- use_module(plRdfEntailment(rdf_ent_stat)).

:- rdf_meta(scrape_picarta(+,r)).
:- rdf_meta(translate_profession(+,r)).





% PROCESSING: LIFE YEARS %

%! process_life_years(+G:atom) is det.
% Processes the life years attribute of the Picarta dataset: from a string
% literal to a more explicit assertion.

process_life_years(G):-
  aggregate_all(
    set(Agent/LifeYears),
    rdf_string(Agent, stcn:life_years, LifeYears, G),
    Pairs
  ),
  run_on_sublists(Pairs, process_life_years(G), 10).

process_life_years(G, Pairs):-
  maplist(process_lifeyears1(G), Pairs).

process_lifeyears1(G, Agent/LifeYears):-
  atom_codes(LifeYears, LifeYearsCodes),
  parse_date(Birth/Death, LifeYearsCodes-[]),
  rdf_assert_datatype(Agent, stcno:birth, Birth, xsd:gYear, G),
  rdf_assert_datatype(Agent, stcno:death, Death, xsd:gYear, G),
  rdf_retractall_string(Agent, stcn:life_years, G).





% PROCESSING: NAME NORMAL %

process_name_normals(G):-
  aggregate_all(
    set(Agent/UnparsedName),
    rdf_assert_string(Agent, stcn:name_normal, UnparsedName, G),
    Pairs
  ),
  run_on_sublists(Pairs, process_name_normals(G), 10).

process_name_normals(G, Pairs):-
  maplist(process_name_normal(G), Pairs).

process_name_normal(G, Agent/UnparsedName):-
  parse_name_normal(UnparsedName, ParsedName),
  rdf_assert_string(Agent, foaf:name, ParsedName, G),
  rdf_retractall_string(Agent, stcn:name_normal, G).

parse_name_normal(UnparsedName, ParsedName):-
  sub_atom(
    UnparsedName,
    EndOfLastNames,
    LengthOfSeparator,
    LengthOfFirstNames,
    ', '
  ), !,
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
  rdfs_assert_class(stcno:'Profession', G),
  forall(
    profession(URI),
    rdf_assert_instance(URI, stcno:'Profession', G)
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
  aggregate_all(
    set(Agent/Profession),
    rdf_string(Agent, stcn:profession, Profession, G),
    Pairs
  ),
  run_on_sublists(Pairs, process_professions(G), 10).

process_professions(G, Pairs):-
  maplist(process_profession(G), Pairs).

process_profession(G, Agent/Literal):-
  atomic_list_concar([ProfessionName|YearNames], ', ', Literal), % split
  translate_profession(ProfessionName, Profession),
  process_profession(Agent, Profession, YearNames, G).

process_profession(Agent, Profession, ['-'], G):- !,
  rdf_assert(Agent, stcn:profession, Profession, G),
  rdf_retractall_string(Agent, stcn:profession, G).
process_profession(Agent, Profession, YearNames, G):- !,
  rdf_bnode(HasProfession),
  rdf_assert(Agent, stcn:has_profession, HasProfession, G),
  rdf_assert(HasProfession, stcn:profession, Profession, G),
  forall(
    member(YearName, YearNames),
    (
      atom_codes(YearName, C1),
      (
        parse_date(Point, C1-[])
      ->
        rdf_assert_datatype(Agent, stcn:active, Point, xsd:gYear, G)
      ;
        parse_date(Begin, End, C1-[])
      ->
        rdf_assert_datatype(Agent, stcn:active_start, Begin, xsd:gYear, G),
        rdf_assert_datatype(Agent, stcn:active_end, End, xsd:gYear, G)
      )
    )
  ).

profession(URI):-
  translate_profession(_, URI).

translate_profession(bookseller, URI):-
  rdf_global_id(stcn:bookseller, URI).
translate_profession('paper seller', URI):-
  rdf_global_id(stcn:paper_seller, URI).
translate_profession(printer, URI):-
  rdf_global_id(stcno:printer, URI).





% PROCESSING: PSEUDONYMS %

process_pseudonyms(G):-
  forall(
    (
      rdf_assert_string(Agent1, stcno:pseudonym, Pseudonym, G),
      rdf_assert_string(Agent2, stcn:author_name, Pseudonym, G)
    ),
    (
      owl_assert_resource_identity(Agent1, Agent2, G),
      debug(picarta, '~w is a pseudonym of ~w.', [Agent1, Agent2])
    )
  ).





% PROCESSING : TOPICS HIERARCHY %

export_topics_hierarchy:-
  rdf_global_id(stcno:'Topic', RootTopic),
  rdf_global_id(skos:broader, Predicate),
  graph_closure([RootTopic], \X^Y^rdf_has(X, Predicate, Y), Topics, []),
  findall(
    [Label,Size],
    (
      member(Topic, Topics),
      topic_label(Topic, Label),
      topic_size(Topic, Size)
    ),
    Rows
  ),
  write(Rows).

%! process_topics_hierarchy(+Graph:atom) is det.
% Constructs the topics hierarchy.
% This is added to the graph in which these topics reside.
%
% This method uses a copied version of trees:all_subpaths_to_tree/2,
% adapted to work with pairs.
%
% @arg G The atomic name of the graph containing the topic resources.

process_topics_hierarchy(G):-
  aggregate_all(
    set(List/Topic),
    (
      rdfs_individual_of(Topic, stcno:'Topic'),
      rdf_string(Topic, stcn:synonym, TopicCode1, G),
      atom_codes(TopicCode1, TopicCode2),
      contains([], [decimal_digit], TopicCode2-[]),
      atom_splits(['.',' '], TopicCode1, List)
    ),
    Pairs
  ),
  rdf_global_id(stcn:'Topic', RootTopic),
  rdf_global_id(stcn:'dummy', Dummy),
  subtopics([['15','70']/Dummy|Pairs], []/RootTopic, Tree),
  skos_assert_hierarchy(Tree, stcn:'TopicScheme', G).

subtopics(AllPairs, List/Topic, Topic-Trees):-
  aggregate_all(
    set(Tree),
    (
      member(LongerList/SubTopic, AllPairs),
      append(List, [_], LongerList),
      subtopics(AllPairs, LongerList/SubTopic, Tree)
    ),
    Trees
  ).

topic_label(Topic, Label):-
  once(rdf_string(Topic, stcn:synonym, Label, _)).

topic_size(RootTopic, Size):-
  rdf_global_id(skos:broader, Predicate),
  graph_closure([RootTopic], \X^Y^rdf_has(X, Predicate, Y), SubTopics, []),
  aggregate_all(
    set(Publication),
    (
      member(SubTopic, SubTopics),
      rdf(Publication, stcno:topic, SubTopic)
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
% @arg G The atomic name of the Picarta graph.
% @arg Class

scrape_picarta(G, Class):-
  aggregate_all(
    set(Individual),
    rdfs_individual_of(Individual, Class),
    Individuals
  ),
  length(Individuals, Length),
  debug(
    picarta,
    'About to scrape ~w individuals of type ~w.\n',
    [Length,Class]
  ),
  run_on_sublists(Individuals, scrape_picarta1(G, Class), 10).

%! scrape_picarta_progress(+PicartaGraph:atom, +StcnGraph:atom) is det.
% Sends scraping statistics to the debug console.
%
% @arg PicartaGraph The atomic name of the Picarta graph.
% @arg StcnGraph The atomic name of the STCN graph.

scrape_picarta_progress(PicartaGraph, StcnGraph):-
  % Author.
  count_instances_by_class(stcno:'Author', PicartaGraph, X1),
  count_instances_by_class(stcno:'Author', StcnGraph, Y1),
  debug(picarta, 'Authors: ~w of ~w.\n', [X1, Y1]),
  
  % Printer.
  count_instances_by_class(stcno:'Printer', PicartaGraph, X2),
  count_instances_by_class(stcno:'Printer', StcnGraph, Y2),
  debug(picarta, 'Printers: ~w of ~w.\n', [X2, Y2]),
  
  % Publications.
  count_instances_by_class(stcno:'Publication', PicartaGraph, X3),
  count_instances_by_class(stcno:'Publication', StcnGraph, Y3),
  debug(picarta, 'Publications: ~w of ~w.\n', [X3, Y3]),
  
  % Topics.
  count_instances_by_class(stcno:'Topic', PicartaGraph, X4),
  count_instances_by_class(stcno:'Topic', StcnGraph, Y4),
  debug(picarta, 'Topics: ~w of ~w.\n', [X4, Y4]).

