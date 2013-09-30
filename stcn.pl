:- module(
  stcn,
  [
    stcn_script/0
  ]
).

/** <module> STCN

This module only contains predicates that have a domain-specific use.
These predicate should be converted to some other module or be removed.

@author Wouter Beek
@see http://www.kb.nl/kbhtml/stcnhandleiding/frames.html
@version 2013/09
*/

:- use_module(generics(replace_in_file)).
:- use_module(library(debug)).
:- use_module(library(semweb/rdf_db)).
:- use_module(os(datetime_ext)).
:- use_module(os(file_ext)).
:- use_module(rdf(rdf_build)).
:- use_module(rdf(rdf_serial)).
:- use_module(rdfs(rdfs_build)).
:- use_module(rdfs(rdfs_read)).
:- use_module(stcn(collect_lines)).
:- use_module(stcn(stcn_parse)).
:- use_module(stcn(stcn_schema)).
:- use_module(stcn(stcn_scrape)).

:- debug(stcn).



stcn_script:-
  % Begin time.
  date_time(Start),
  debug(stcn, 'Loading started at: ~w.\n', [Start]),

  % STCN schema.
  stcn_schema('STCNV'),
  rdf_save2('STCNV'),

  % Parse redactiebladen file.
  (
    absolute_file_name2(data(redactiebladen_5), F3, [access(read),file_type(text)]),
    access_file(F3, read), !
  ;
    absolute_file_name2(data(redactiebladen_1), F1, [access(read),file_type(text)]),
    collect_lines(F1, F2),
    replace_in_file(F2, F3)
  ),

  (
    absolute_file_name(stcn('STCN'), F, [access(read),file_type(turtle)]),
    rdf_load2(F, [format(turtle),graph('STCN')]), !
  ;
    parse_redactiebladen(F3, 'STCN'),
    rdf_save2('STCN')
  ),

  thread_create(
    stcn_scrape('Publication','STCN'),
    _Id,
    []
  ),

  %stcn_clean,

  % End time.
  date_time(End),
  debug(stcn, 'Loading ended at: ~w.\n', [End]).

replace_in_file(F1, F4):-
  trim_spaces(F1, F2),
  replace_in_file(F2, "Â°", "°", F3),
  replace_in_file(F3, "Ãª", "°", F4).
