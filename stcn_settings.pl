:- module(stcn_settings, []).

/** <module> STCN: Settings

@author Wouter Beek
@version 2015/02
*/

:- use_module(library(settings)).





:- setting(
  input_location,
  atom,
  'http://truthberead.com/wb-files/redactiebladen.tar.gz',
  'The location at which the input to the STCN parser resides.'
).
