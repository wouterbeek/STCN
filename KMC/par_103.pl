:- module(
  par_103,
  [
    assert_schema_par_103/1, % +Graph:atom
    value_par_103/3 % ?Code:char
                    % ?Label:atom
                    % ?Comment:atom
  ]
).

/** <module> PAR 103

## Old prints

At position 3 only the following codes are allowed:

| *Code* | *Description*                | *Mutatable*                                                                                                                              |
| a      | Acquisition title.           | Yes                                                                                                                                      |
| r      | Retro-conversion.            | Yes, provided that it can be assertained that the same [editie] (see STCN §7). Otherwise a new description (i.e., PPN) has to be added.  |
| x      | Bibliographical completeness | Yes, but only on good grounds.                                                                                                           |
| v      | Bibliographical completeness | No. The v-status is only then allowed to be assigned if the description has been constructed based on the regulations of the subset. [?] |

@author Wouter Beek
@compat http://support.oclc.org/ggc/richtlijnen/php/showPresentation.php?id=13&ln=nl&par=p-103
@version 2013/01, 2013/03, 2013/06, 2013/09
*/

:- use_module(library(debug)).
:- use_module(library(semweb/rdf_db)).
:- use_module(rdf(rdf_build)).
:- use_module(rdfs(rdfs_build)).
:- use_module(xml(xml_namespace)).

:- xml_register_namespace(stcn, 'http://stcn.data2semantics.org/resource/').
:- xml_register_namespace(stcnv, 'http://stcn.data2semantics.org/resource/vocab/').

:- nodebug(par_103).



assert_schema_par_103(G):-
  rdfs_assert_subclass(stcnv:'PAR_StatusValue', stcnv:'StatusValue', G),
  rdfs_assert_label(stcnv:'PAR_StatusValue', 'PAR 103 status value', G),
  rdfs_assert_comment(stcnv:'PAR_StatusValue', en,
    'Status values as defined by PAR 103.', G),
  
  forall(
    value_par_103(Char, Label, Comment),
    (
      atomic_concat(par, Char, PAR_StatusValue1),
      rdf_global_id(stcn:PAR_StatusValue1, PAR_StatusValue2),
      rdfs_assert_label(PAR_StatusValue2, nl, Label, G),
      rdfs_assert_comment(PAR_StatusValue2, nl, Comment, G),
      rdf_assert_individual(PAR_StatusValue2, stcnv:'PAR_StatusValue', G)
    )
  ).

%! value_par_103(?Code:char, ?Description:atom, ?Label:atom) is nondet.

value_par_103(a, acquisitie, acquisitie).
value_par_103('B', 'tape-invoer', 'Titels, afkomstig van tape-invoer,\c
  waarvan na matching is gebleken dat ze wellicht al in het GGC aanwezig\c
  waren. Deze titels kunnen worden omgewerkt naar "normale" titels, maar\c
  zullen veelal verwijderd worden.').
value_par_103(c, 'C.I.P.', 'C.I.P.').
value_par_103(p,'bibliografische onvolledigheid', 'Bibliografisch\c
  onvolledig record van een openbare bibliotheek in Nederland in het\c
  kader van het NBC project').
value_par_103(r, retrospectief, retrospectief).
value_par_103(v, 'bibliografisch volledig', 'Bibliografisch volledig,\c
  niet muteerbaar.').
value_par_103(x, 'tape titel', 'Tape titel/bibliografisch. Volledig,\c
  muteerbaar.').
value_par_103(y, 'bibliografisch onvolledig', 'Bibliografisch onvolledig.').

