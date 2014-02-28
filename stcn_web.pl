:- module(stcn_web, []).

/** <module> STCN Web

Web front-end for the STCN.

@author Wouter Beek
@version 2012/12-2013/01, 2013/03, 2013/05-2013/06, 2013/09-2013/10,
         2013/12-2014/01
*/

:- use_module(generics(atom_ext)).
:- use_module(generics(db_ext)).
:- use_module(graph_theory(graph_web)).
:- use_module(html(html)).
:- use_module(html(html_table)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/html_head)).
:- use_module(library(http/http_server_files)).
:- use_module(library(http/html_write)).
:- use_module(library(lists)).
:- use_module(server(app_ui)).
:- use_module(server(web_error)).
:- use_module(server(web_modules)).
:- use_module(stcn(stcn_statistics)).

:- http_handler(root(stcn), stcn_web, [prefix,priority(10)]).
:- initialization(web_module_add('STCN', stcn_web)).

% /html
:- db_add_novel(http:location(html, root(html), [])).
:- db_add_novel(user:file_search_path(stcn_html, stcn('HTML'))).
:- http_handler(html(.), serve_files_in_directory(stcn_html), [prefix]).
:- html_resource(css('wallace.css'), []).



stcn_web(Request):-
  memberchk(path(Path), Request),
  atomic_list_concat(Terms, '/', Path), % split
  last(Terms, PPN),
  stcn(Request, PPN).
  (
    (PPN == '' ; PPN == 'stcn')
  ->
    reply_html_file(app_style, stcn)
  ;
    reply_html_page(
      app_style,
      title('STCN'),
      [\vertex_web(stcn, PPN),\stcn_statistics_web]
    )
  ).

stcn_statistics_web -->
  {stcn_statistics(Rows)},
  html(
    \html_table(
      [header_row(false)],
      html('Overview of STCN statistics.'),
      Rows
    )
  ).
