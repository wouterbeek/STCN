% The load file for the STCN project.

:- multifile(user:project_name/1).
user:project_name('STCN').

:- initialization(load_stcn).

load_stcn:-
  % Entry point.
  source_file(load_stcn, ThisFile),
  file_directory_name(ThisFile, ThisDir),

  % STCN
  assert(user:file_search_path(stcn, ThisDir)),
  assert(user:file_search_path(data, stcn('Data'))),
  assert(user:file_search_path(kmc, stcn('KMC'))),
  assert(user:file_search_path(picarta, stcn('Picarta'))),
  use_module(stcn(stcn_web)).

