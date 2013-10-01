:- module(
  stcn_web,
  [
    ppn_web/2, % +PPN:atom
               % -Markup:dom
    r_web/3, % +Graph:atom
             % +Predicate:iri
             % -DOM:list
    stcn_statistics_web/1, % -Markup:dom
    stcn_web/1 % -Markup:dom
  ]
).

/** <module> STCN Web

Web front-end for STCN methods.

@author Wouter Beek
@version 2012/12-2013/01, 2013/03, 2013/05-2013/06, 2013/09-2013/10
*/

:- use_module(generics(atom_ext)).
:- use_module(generics(db_ext)).
:- use_module(generics(meta_ext)).
:- use_module(graph_theory(graph_web)).
:- use_module(html(html)).
:- use_module(html(html_table)).
:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/html_head)).
:- use_module(library(http/http_server_files)).
:- use_module(library(http/html_write)).
:- use_module(library(lists)).
:- use_module(library(real)).
:- use_module(library(semweb/rdf_db)).
:- use_module(rdf(rdf_lit)).
:- use_module(rdf(rdf_name)).
:- use_module(rdfs(rdfs_read)).
:- use_module(server(error_web)).
:- use_module(server(web_console)).
:- use_module(stcn(stcn_statistics)).
:- use_module(svg(svg_file)).

:- register_module(stcn_web).

:- rdf_meta(r_web(+,r,-)).

:- http_handler(root(stcn), stcn, [prefix,priority(10)]).

% Fixate the location of the HTML pages.
:- db_add_novel(http:location(html, root(html), [])).
:- db_add_novel(user:file_search_path(stcn_html, stcn('HTML'))).
:- http_handler(html(.), serve_files_in_directory(stcn_html), [prefix, priority(10)]).

:- html_resource(css('wallace.css'), []).



user:body(stcn, Body) -->
  html(body(Body)).

user:head(stcn, Head) -->
  html(head([\html_requires(css('wallace.css'))|Head])).

ppn_web(PPN, Markup):-
  catch_web(vertex_web(stcn, PPN), Markup).

resource_class(Resource, Class):-
  rdf_is_typed_literal(Resource), !,
  rdf_typed_literal(_, Resource, Class, _).
resource_class(Resource, Class):-
  rdf(Resource, rdf:type, Class), !.
resource_class(Resource, Class):-
  rdfs_individual(m(t,f,f), Resource, Class, _).

r_comp_web(G, P11, P21, SVG):-
  rdf_global_id(P11, P12),
  rdf_global_id(P21, P22),
  once(rdf(SomeS1, P12, SomeO1, G)),
  once(rdf(SomeS2, P12, SomeO2, G)),
  resource_class(SomeS1, S_Class),
  resource_class(SomeS2, S_Class),
  resource_class(SomeO1, O_Class),
  resource_class(SomeO2, O_Class),
  with_output_to(atom(Y_Axis), rdf_term_name([], S_Class)),
  with_output_to(atom(X_Axis), rdf_term_name([], O_Class)),
  setoff(
    O,
    (rdf(_S1, P12, O, G), once(rdf(_S2, P22, O, G))),
    Os1
  ),
  findall(
    NumberOfS1,
    (
      member(O, Os1),
      aggregate_all(count, rdf(_, P12, O, G), NumberOfS1)
    ),
    Bars1
  ),
  findall(
    NumberOfS2,
    (
      member(O, Os1),
      aggregate_all(count, rdf(_, P22, O, G), NumberOfS2)
    ),
    Bars2
  ),
  maplist(rdf_term_name([]), Os1, Os2),
  Bars <- t(matrix([Bars1,Bars2], ncol=2)),
  absolute_file_name(test, File, [access(write),extensions([svg])]),
  <- svg(+File),
  <- barplot(
    Bars,
    % Columns are not stacked on top of each other,
    % but are placed beside each other.
    beside='TRUE',
    % Scaling of the font size of x-axis labels.
    cex..names=0.8,
    % Labels perpendicular to axis.
    las=2,
    % Logarithmic scale.
    log=+y,
    % Text labels for x-axis.
    names..arg=Os2,
    ylab=+Y_Axis
  ),
  % Label for x-axis needs special positioning.
  <- title(xlab=+X_Axis, line=5),
  with_output_to(atom(Legend1), rdf_term_name([], P12)),
  with_output_to(atom(Legend2), rdf_term_name([], P22)),
  <- legend(
    % Legend position.
    +"topleft",
    % Text for the legend
    legend=[Legend1,Legend2],
    % Legend color map.
    fill=[blue,red]
  ),
  <- dev..off(.),
  file_to_svg(File, SVG).

r_web(G, P1, SVG):-
  rdf_global_id(P1, P2),
  once(rdf(SomeS, P2, SomeO, G)),
  resource_class(SomeS, S_Class),
  resource_class(SomeO, O_Class),
  with_output_to(atom(Y_Axis), rdf_term_name([], S_Class)),
  with_output_to(atom(X_Axis), rdf_term_name([], O_Class)),

  setoff(O, rdf(_, P2, O, G), Os1),
  maplist(rdf_term_name([]), Os1, Os2),
  findall(
    NumberOfS,
    (
      member(O, Os1),
      aggregate_all(count, rdf(_, P2, O), NumberOfS)
    ),
    Bars
  ),
  with_output_to(codes(Main), rdf_term_name([], P2)),
  absolute_file_name(test, File, [access(write),extensions([svg])]),
  <- svg(+File),
  <- barplot(
    Bars,
    % Scaling of the font size of x-axis labels.
    cex..names=0.8,
    %col=rainbow(5),
    % Number of stripes in columns.
    % Incompatible with logarithmic scale.
    %%%%density=10,
    % Labels perpendicular to axis.
    las=2,
    %log=+y,
    % Caption text.
    main=+Main,
    % Text labels for x-axis.
    names..arg=Os2,
    ylab=+Y_Axis
  ),
  % Label for x-axis needs special positioning.
  <- title(xlab=+X_Axis, line=5),
  <- dev..off(.),
  file_to_svg(File, SVG).

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
  stcn_statistics(Rows),
  html_table([header(false)], Rows, HTML_Table).

stcn_web(Body):-
  absolute_file_name(html(stcn), File, [access(read),file_type(html)]),
  file_to_html(File, DOM),
  contains_term(element(body, _, Body), DOM).

