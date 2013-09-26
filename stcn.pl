:- module(
  stcn,
  [
    stcn_script/0
  ]
).

/** <module> STCN

This module only contains predicates that have a domain-specific use.
These predicate should be converted to some other module or be removed.

We want to integrate this module with STCN_LOAD at some point.

@author Wouter Beek
@see http://www.kb.nl/kbhtml/stcnhandleiding/frames.html
@tbd Automatically generate TODO_publications.txt based on Redactiebladen.txt
@tbd See whether there is a more efficient way to parse the redactiebladen.
@version 2013/06, 2013/09
*/

:- use_module(generics(cowspeak)).
:- use_module(library(debug)).
:- use_module(os(datetime_ext)).
:- use_module(rdf(rdf_serial)).
:- use_module(stcn(stcn_clean)).
:- use_module(stcn(stcn_generic)).
:- use_module(stcn(stcn_parse)).
:- use_module(stcn(stcn_schema)).
:- use_module(stcn(stcn_scrape)).

:- set_prolog_stack(local, limit(2*10**9)).

:- debug(stcn).



stcn_script:-
  % Begin time.
  date_time(StartDateTime),
  debug(stcn, 'Loading started at: ~w.\n', [StartDateTime]),
  
  % STCN vocabulary / schema.
  stcn_schema('STCNV'),
  rdf_save2('STCNV'),
  
  % STCN publications
  parse_redactiebladen,
  % VERIFIED UNTIL THIS LINE, RESULT SAVED AS `RedactiebladenParse.ttl`.
  
  % STCN agents and publications.
  once((
    STCN_Agents = 'STCN_Agents',
    STCN_Publications = 'STCN_Publications',
    maplist(stcn_graph, [STCN_Agents, STCN_Publications]),
    cowspeak:cowspeak(
      'Loaded graphs <~w> and <~w>.',
      [STCN_Agents, STCN_Publications]
    )
  ;
    stcn_scrape,
    cowspeak:cowspeak('Done scraping the STCN.', [])
  )),

  % Add information coming from the redactiebladen.
  once((
    Redactiebladen = 'Redactiebladen',
    stcn_graph(Redactiebladen),
    cowspeak:cowspeak('Loaded graph <~w>.', [Redactiebladen])
  ;
    parse_redactiebladen,
    cowspeak:cowspeak('Done parsing redactiebladen.', [])
  )),

  % STCN cleaning.
  cowspeak:cowspeak('I\'m gonna clean the STCN now.', []),
  stcn_clean,

  % End time.
  date_time(EndDateTime),
  debug(stcn, 'Loading ended at: ~w.\n', [EndDateTime]).

/*
rem:-
  publication_size(before, 'Arminius, Jacobus', B1),
  publication_size(after, 'Arminius, Jacobus', A1),
  format(user, 'Arminius: ~w --> ~w.\n', [B1, A1]),
  publication_size(after, 'Gomarus, Franciscus', B2),
  publication_size(before, 'Gomarus, Franciscus', A2),
  format(user, 'Gomarus: ~w --> ~w.\n', [B2, A2]).

publication_size(BeforeAfter, Name, Average):-
  findall(
    Size,
    (
      rdf_read:rdf_datatype(Arminius, stcn:name_full, string, Name, _),
      rdf_has(Publication, stcnv:author, Arminius),
      rdf_read:rdf_datatype(Publication, _, gYear, Year, _),
      (BeforeAfter == before -> Year < 1619 ; Year > 1619),
      rdf(Publication, stcnv:format, Format),
      kmc_4062:translate_format(Format, Size, _)
    ),
    Sizes
  ),
  length(Sizes, Length),
  sum_list(Sizes, S),
  Average is S / Length.

contra:-
  setoff(
    row(arminius, ArminiusPublication, Year),
    (
      rdf_global_id(stcn:'069852812', Arminius),
      rdf_has(ArminiusPublication, stcnv:author, Arminius),
      rdf_has_datatype(ArminiusPublication, stcn:publication_year, gYear, Year)
    ),
    ArminiusPublications
  ),
  setoff(
    row(gomarus, GomarusPublication, Year),
    (
      rdf_global_id(stcn:'070053243', Gomarus),
      rdf_has(GomarusPublication, stcnv:author, Gomarus),
      rdf_has_datatype(GomarusPublication, stcn:publication_year, gYear, Year)
    ),
    GomarusPublications
  ),
  append(ArminiusPublications, GomarusPublications, Publications),
  append([Publications, Publications, Publications, Publications], X_Y),
  absolute_file_name(data(x_y), File, [access(write), file_type(csv)]),
  csv_write_file(File, X_Y).

foreign_language_publications(Publications):-
  setoff(
    Publication,
    (
      rdfs_individual_of(Publication, stcnv:'Publication'),
      \+ rdf(
        Publication,
        stcn:actual_language,
        'http://lexvo.org/id/iso639-3/nld'
      )
    ),
    Publications
  ).

publications_during(Begin/End, Publications):-
  setoff(
    Publication,
    (
      rdf_has_datatype(Publication, stcn:publication_year, gYear, Year),
      Begin =< Year,
      Year =< End
    ),
    Publications
  ).

table:-
  findall(
    row(PPN, Topic, Title, TitleLength, Year),
    (
      rdf(P, stcnv:topic, T),
      (
        rdf_datatype(T, stcn:term, string, Topic, _)
      ;
        rdf_datatype(T, stcn:synonym, string, Topic, _)
      ),
      rdf_has_datatype(P, stcn:publication_year, gYear, Year),
      rdf_datatype(P, stcn:title, string, Title, _),
      atom_length(Title, TitleLength),
      rdf_global_id(stcn:PPN, P)
    ),
    Rows
  ),
  absolute_file_name(data(stcn), File, [access(write), file_type(csv)]),
  csv_write_file(File, Rows).

reynaert:-
  setoff(
    row(PPN, Year),
    (
      rdf_datatype(P, stcn:alternative_title, string, Title, _),
      isub(Title, 'Reynaert die Vos', true, V),
      V > 0.9,
      rdf_global_id(stcn:PPN, P),
      rdf_has_datatype(P, stcn:publication_year, gYear, Year)
    ),
    Rows
  ),
  absolute_file_name(data(reynaert), File, [access(write), file_type(csv)]),
  csv_write_file(File, Rows).
*/

