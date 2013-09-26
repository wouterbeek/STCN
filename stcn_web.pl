:- module(
  stcn_web,
  [
    ppn_web/2, % +PPN:atom
               % -Markup:dom
    stcn_statistics_web/1, % -Markup:dom
    stcn_web/1 % -Markup:dom
  ]
).

/** <module> STCN Web

Web front-end for STCN methods.

@author Wouter Beek
@version 2012/12-2013/01, 2013/03, 2013/05-2013/06, 2013/09
*/

:- use_module(generics(atom_ext)).
:- use_module(generics(db_ext)).
:- use_module(generics(list_ext)).
:- use_module(graph_theory(graph_web)).
:- use_module(library(http/html_head)).
:- use_module(library(http/html_write)).
:- use_module(server(error_web)).
:- use_module(html(html)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_server_files)).
:- use_module(stcn(stcn_statistics)).

:- http_handler(root(stcn), stcn, [prefix, priority(10)]).

% Fixate the location of the HTML pages.
:- db_add_novel(http:location(html, root(html), [])).
:- db_add_novel(user:file_search_path(stcn_html, stcn('HTML'))).
:- http_handler(html(.), serve_files_in_directory(stcn_html), [prefix, priority(10)]).

:- html_resource(css('wallace.css'), []).



user:body(stcn, Body) -->
  html(body(Body)).

user:head(stcn, Head) -->
  html(head([\html_requires(css('wallace.css')) | Head])).

ppn_web(PPN, Markup):-
  load_stcn,
  catch_web(vertex_web(stcn, PPN), Markup).

stcn(Request):-
  memberchk(path(Path), Request),
  split_atom_exclusive(['/'], Path, Terms),
  last(Terms, PPN),
  (
    (PPN == '' ; PPN == 'stcn')
  ->
    reply_html_file(stcn, stcn)
  ;
    catch_web(
      vertex_web(stcn, PPN),
      Markup
    ),
    reply_html_page(stcn, [], Markup)
  ).

stcn_statistics_web([HTML_Table]):-
  load_stcn,
  stcn_statistics(Rows),
  list_to_table([header(false)], Rows, HTML_Table).

stcn_web(Body):-
  load_stcn,
  absolute_file_name(
    html(stcn),
    File,
    [access(read), file_type(html)]
  ),
  file_to_html(File, HTML_DOM),
  contains_term(element(body, _, Body), HTML_DOM).

