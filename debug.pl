% Debug file for the STCN project.

% Indicate to the support modules that we are running in debug mode.
:- assert(user:debug_project).

:- [load].

:- use_module(server(dev_server)).
:- use_module(server(web_console)).

:- start_dev_server.

:- use_module(rdf(rdf_web)).
:- register_module(rdf_web).

