:- module(
  stcn_load,
  [
    load_stcn/1 % +Request:list
  ]
).

/** <module> Load STCN

Loads the STCN dataset. If the dataset is not present but the redactiebladen
are present, then these are parsed and turned into the STCN dataset.

@author Wouter Beek
@tbd Add fully automated script for redactiebladen parsing.
@version 2013/04
*/

:- use_module(datasets(picarta)).
:- use_module(generic(file_ext)).
:- use_module(generic(os_ext)).
:- use_module(generic(thread_ext)).
:- use_module(library(http/html_write)).
:- use_module(library(pce)).
:- use_module(rdf(rdf_graph)).
:- use_module(rdf(rdf_serial)).
:- use_module(stcn(stcn_statistics)).
:- use_module(vocabularies('VoID')).

:- assert(user:prolog_file_type(txt, text)).

:- http_handler(root(stcn_load), load_stcn, []).



load_stcn(_Request):-
  catch(
    (
      load_stcn0,
      reply_html_page(
        cliopatria(main),
        [title('STCN Loaded')],
        [p('The file VoID.ttl was loaded.')]
      )
    ),
    error(existence_error(source_sink, _Spec), _Context),
    reply_html_page(
      cliopatria(main),
      [title('STCN Error')],
      [p('The file VoID.ttl could not be loaded from your personal STCN directory.')]
    )
  ).

load_stcn0:-
  absolute_file_name(
    data_stcn('VoID'),
    VoID_File,
    [access(read),file_type(turtle)]
  ),
  void_load_library(VoID_File, _Graph).

% This produces the STCN dataset based on the redactiebladen file.

master_script1:-
  % STCN/STCN_Author
  scrape_picarta('STCN/STCN_Authors', stcnv:'Author'),
  % STCN/STCN_Printer
  scrape_picarta('STCN/STCN_Printers', stcnv:'Printer'),
  % STCN/STCN_Publication2
  scrape_picarta('STCN/STCN_Publications2', stcnv:'Publication'),
  % STCN/STCN_Topic
  scrape_picarta('STCN/STCN_Topics', stcnv:'Topic').

master_script2:-
  thread_recover,
  thread_overview.

master_script3:-
  rdf_copy_graph('STCN/STCN_Publications2', 'STCN/STCN_Publications'),
  findall(
    ThreadId,
    (
      member(
        Graph,
        ['STCN/STCN_Authors', 'STCN/STCN_Printers', 'STCN/STCN_Publications']
      ),
      thread_create(rdf_save2(Graph), _Id, [])
    ),
    ThreadIds
  ),
  forall(
    member(ThreadId, ThreadIds),
    thread_join(ThreadId, true)
  ),
  stcn_statistics(Rows),
  forall(
    member(Row, Rows),
    format(user, '~w\n', [Row])
  ).

