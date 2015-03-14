:- module(
  stcn_generics,
  [
    ppn//0,
    ppn//1, % -PpnCode:atom
    ppn//2, % +Category:atom
            % -Ppn:iri
    ppn_resource/3 % +Category:atom
                   % +PpnName:atom
                   % -Ppn:iri
  ]
).

/** <module> STCN: Generics

Things that are used throughout the STCN project,
but are not generic enough to be in PGC.

@author Wouter Beek
@version 2013/06, 2013/09-2013/10, 2015/02
*/

:- use_module(library(semweb/rdf_db), except([rdf_node/1])).

:- use_module(plc(dcg/dcg_ascii)).
:- use_module(plc(generics/atom_ext)).

:- rdf_meta(category_class(?,r)).





ppn -->
  ppn(_).



%! ppn(-PpnCode:atom)//

ppn(PpnCode) -->
  ppn_char(C1),
  ppn_char(C2),
  ppn_char(C3),
  ppn_char(C4),
  ppn_char(C5),
  ppn_char(C6),
  ppn_char(C7),
  ppn_char(C8),
  ppn_char(C9),
  {atom_codes(PpnCode, [C1,C2,C3,C4,C5,C6,C7,C8,C9])}.

ppn_char(C) --> decimal_digit(_, C).
ppn_char(C) --> x(C).



%! ppn(+Category:atom, -Ppn:iri)// is det.
% Parses a PPN identifier and returns its IRI.
%
% There are 3*10^8 possibilities, so don't use this for generation.

ppn(Category, Ppn) -->
  ppn(PpnCode), !,
  {ppn_resource(Category, PpnCode, Ppn)}.



%! ppn_resource(+Category:atom, +PpnName:atom, -PPN:iri) is det.

ppn_resource(Category, PpnName0, Ppn):-
  atomic_list_concat([Category,PpnName0], '/', PpnName),
  rdf_global_id(stcn:PpnName, Ppn).

