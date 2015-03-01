:- module(
  kmc_4062,
  [
    format/4, % ?Name:atom
              % ?Symbol:atom
              % ?NumberOfLeaflets:list(atom)
              % ?Kettinglijnen:oneof([horizontal,vertical])
              % ?Visual:atom
    malformed_format/2, % +From:atom
                        % -To:atom
    translate_format/3 % ?Format:uri
                       % ?Number:nonneg
                       % ?Name:atom
  ]
).

/** <module> Bakcground Knowledge: KMC 4062 (Format)

@author Wouter Beek
@version 2015/02
*/

:- use_module(library(dcg/basics)).
:- use_module(library(debug)).
:- use_module(library(semweb/rdf_db), except([rdf_node/1])).

:- use_module(plc(dcg/dcg_ascii)).
:- use_module(plc(dcg/dcg_cardinal)).
:- use_module(plc(dcg/dcg_content)).
:- use_module(plc(dcg/dcg_generics)).
:- use_module(plc(generics/db_ext)).

:- use_module(plRdf(api/rdf_build)).
:- use_module(plRdf(api/rdfs_build)).
:- use_module(plRdf(api/rdfs_read)).
:- use_module(plRdf(vocabulary/rdf_stat)).

:- rdf_meta(kmc_4062_extra(?,r)).
:- rdf_meta(translate_format(r,?)).

user:file_search_path(images, project('Images')).
user:prolog_file_type(png, png).





%! format(
%!   ?Name:atom,
%!   ?Symbol:atom,
%!   ?NumberOfLeaflets:list(atom),
%!   ?Kettinglijnen:oneof([horizontal,vertical]),
%!   ?Visual:atom
%! ) is nondet.

format('plano', '1°', '1', horizontal, '1/4 [lengteas] halfway [breedteas]', '[[plano.jpg]]').
format('folio', '2°', '2', vertical, middle, '').
format('quarto', '4°', '4', horizontal, 'part: halfway the inner margin', '').
format('octavo', '8°', '8', vertical, 'part: at the top, in the innner margin', '').
format('duodecimo', '12°', '12', vertical, 'part: halfway of the top margin', '').
%format('duodecimocommon', '12°', '12, 8, 6, 4', horizontal, '1/3 outer margin', '').
%format('duodecimolong', '12°', '12', vertical, 'part: halfway of the top margin', '').
format('16mo', '16°', '8', horizontal, 'part: at the top, in the outer margin', '').
format('18mo', '18°', '18, 12/6, 3x6 or 10/8', vertical, 'middle leaflet', '').
%format('24mo_long', '24°', '3x8 or 2x12', vertical, 'part: halfway the outer margin', '').
format('24mo', '24°', '3x8 or 2x12', horizontal, 'part: at the top, in the inner margin', '').
format('32mo', '32°', '32', vertical, 'Not mentioned on the STCN website!', '').
format('48mo', '48°', '48', unknown, 'Not mentioned on the STCN website!', '').
format('64mo', '64°', '64', horizontal, 'Not mentioned on the STCN website!', '').


%! malformed_format(+From:atom, -To:atom) is semidet.

% Some PPNs have no specific format information but do have this KMC.
% In these cases a special format (or non-format) is given:
% 'stcn:OtherFormat'.
malformed_format('Other', other_format).
% Probably a variant of 'OtherFormat', e.g. PPN 317408364.
malformed_format('Other°', other_format).
% Some PPNs seem to have the malformed_format value '?ê', e.g. PPN 109361423.
malformed_format('?ê', unknown_format).
% Some PPNs seem to have the malformed value '°', e.g., PPN 292525966.
malformed_format('°', unknown_format).
% Some PPNs seem to have an alternative value for unknown, e.g.,
% PPN 152445323.
malformed_format('?', unknown_format).
malformed_format('0', unknown_format).



%! translate_format(?Format:uri, ?Number:nonneg, ?Name:atom) is nondet.

translate_format(Format, 1, '1°'):-
  rdf_equal(Format, stcno:plano).
translate_format(Format, 2, '2°'):-
  rdf_equal(Format, stcno:folio).
translate_format(Format, 4, '4°'):-
  rdf_equal(Format, stcno:quarto).
translate_format(Format, 8, '8°'):-
  rdf_equal(Format, stcno:octavo).
translate_format(Format, 12, '12°'):-
  rdf_equal(Format, stcno:duodecimo).
translate_format(Format, 16, '16°'):-
  rdf_equal(Format, stcno:'16mo').
translate_format(Format, 18, '18°'):-
  rdf_equal(Format, stcno:'18mo').
translate_format(Format, 24, '24°'):-
  rdf_equal(Format, stcno:'24mo').
translate_format(Format, 32, '32°'):-
  rdf_equal(Format, stcno:'32mo').
translate_format(Format, 48, '48°'):-
  rdf_equal(Format, stcno:'48mo').
translate_format(Format, 64, '64°'):-
  rdf_equal(Format, stcno:'64mo').
