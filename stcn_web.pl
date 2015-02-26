:- module(stcn_web, []).

/** <module> STCN Web

Web front-end for the STCN.

@author Wouter Beek
@version 2012/12-2013/01, 2013/03, 2013/05-2013/06, 2013/09-2013/10,
         2013/12-2014/01, 2015/02
*/

:- use_module(library(http/http_dispatch)).
:- use_module(library(http/html_head)).
:- use_module(library(http/http_server_files)).
:- use_module(library(http/html_write)).
:- use_module(library(lists), except([delete/3,subset/2])).

:- use_module(plc(generics/atom_ext)).
:- use_module(plc(generics/db_ext)).

:- use_module(plHtml(html)).
:- use_module(plHtml(web_modules)).
:- use_module(plHtml(elements/html_table)).

:- use_module(stcn(stcn_statistics)).

:- http_handler(root(stcn), stcn_web, [prefix,priority(10)]).

:- dynamic(http:location/3).
:- multifile(http:location/3).
http:location(html, root(html), []).
user:file_search_path(stcn_html, stcn('HTML')).

:- http_handler(html(.), serve_files_in_directory(stcn_html), [prefix]).

:- html_resource(css('wallace.css'), []).

user:web_module('STCN', stcn_web).

:- rdf_meta(stcn_web(+,r)).





stcn_web(_):-
  reply_html_file(app_style, stcn).

stcn_web(_, PPN):-
  reply_html_page(
    app_style,
    title('STCN'),
    html([
      \vertex_web(PPN),
      \stcn_statistics_web
    ])
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

