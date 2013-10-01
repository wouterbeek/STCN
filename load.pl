% The load file for the STCN project.

project_name('STCN').

:- initialization(load_stcn).

load_stcn:-
  source_file(load_stcn, ThisFile),
  file_directory_name(ThisFile, ThisDirectory),
  % By asserting the STCN directory as mnemonic =project= we can refer
  % to this from within the PGC (which does not 'know' that STCN is
  % using it).
  assert(user:file_search_path(project, ThisDirectory)),
  assert(user:file_search_path(stcn, ThisDirectory)),
  
  assert(user:file_search_path(data, stcn('Data'))),
  assert(user:file_search_path(kmc, stcn('KMC'))),
  assert(user:file_search_path(picarta, stcn('Picarta'))),
  
  % Load the PGC.
  assert(user:file_search_path(pgc, stcn('PGC'))),
  (
    predicate_property(debug, visible)
  ->
    ensure_loaded(pgc(debug))
  ;
    ensure_loaded(pgc(load))
  ),
  
  % Debug server.
  use_module(server(dev_server)),
  use_module(server(web_console)),
  start_dev_server(3333),
  
  % Load the STCN Web predicates.
  use_module(stcn(stcn_web)),
  
  % RDF Web front-end.
  use_module(rdf(rdf_web)),
  register_module(rdf_web),
  
  % Create the STCN SW dataset.
  use_module(stcn(stcn_script)),
  thread_create(stcn_script, _Id, []).

