:- module(
  stcn_script,
  [
    stcn_script/0
  ]
).

/** <module> STCN script

This module only contains predicates that have a domain-specific use.
These predicate should be converted to some other module or be removed.

@author Wouter Beek
@see http://www.kb.nl/kbhtml/stcnhandleiding/frames.html
@version 2013/09-2013/10
*/

:- use_module(generics(replace_in_file)).
:- use_module(generics(thread_ext)).
:- use_module(library(debug)).
:- use_module(os(datetime_ext)).
:- use_module(os(file_ext)).
:- use_module(rdf(rdf_serial)).
:- use_module(stcn(collect_lines)).
:- use_module(stcn(stcn_clean)).
:- use_module(stcn(stcn_parse)).
:- use_module(stcn(stcn_schema)).
:- use_module(stcn(stcn_scrape)).

:- debug(stcn).



stcn_script:-
  % Start time.
  date_time(Start),
  debug(stcn, 'Loading started at: ~w.\n', [Start]),

  % Asserts the schema for STCN.
  stcn_schema('STCNV'),
  absolute_file_name(data('STCNV'), F0, [access(write),file_type(turtle)]),
  rdf_save2(F0, [format(turtle),graph('STCNV')]),

  % Prepares the redactiebladen file.
  (
    absolute_file_name2(
      data(redactiebladen_5),
      F5,
      [access(read),file_type(text)]
    ),
    access_file(F5, read), !
  ;
    absolute_file_name2(
      data(redactiebladen_1),
      F1,
      [access(read),file_type(text)]
    ),
    collect_lines(F1, F2),

    % Performs some in-file replacements.
    trim_spaces(F2, F3),
    replace_in_file(F3, "Â°", "°", F4),
    replace_in_file(F4, "Ãª", "°", F5)
  ),

  % Parse the redactiebladen.
  (
    absolute_file_name2(
      data('Redactiebladen'),
      F6,
      [access(read),file_type(turtle)]
    ),
    rdf_load2(F6, [format(turtle),graph('Redactiebladen')]), !
  ;
    parse_redactiebladen(F5, 'Redactiebladen'),
    absolute_file_name(
      data('Redactiebladen'),
      F6,
      [access(write),file_type(turtle)]
    ),
    rdf_save2(F6, [format(turtle),graph('Redactiebladen')])
  ),
  
  % Picarta scraping.
  stcn_scrape('Redactiebladen', 'Publication', 'PicartaPublications'),
  forall_thread(
    (
      member(
        Category-ToG,
        [
          author-'PicartaAuthors',
          printer_publisher-'PicartaPrintersPublishers',
          translator_editor-'PicartaTranslatorEditor'
        ]
      ),
      format(atom(Msg), 'Scraping Picarta for ~w', [Category])
    ),
    stcn_scrape('Redactiebladen', Category, ToG),
    stcn,
    Msg
  ),

  %stcn_clean('STCN'),

  % End time.
  date_time(End),
  debug(stcn, 'Loading ended at: ~w.\n', [End]).

