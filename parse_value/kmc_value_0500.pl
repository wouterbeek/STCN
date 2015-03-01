:- module(kmc_value_0500, []).

/** <module> Parse KMC Value: 0500

Parses the 'status' of a work.

@author Wouter Beek
@see http://www.kb.nl/kbhtml/stcnhandleiding/0500.html
@see http://support.oclc.org/ggc/richtlijnen/php/showPresentation.php?id=12&ln=nl&sec=k-0500
@see http://support.oclc.org/ggc/richtlijnen/php/showPresentation.php?id=13&ln=nl&par=p-103
@version 2015/02
*/

:- use_module(library(debug)).
:- use_module(library(semweb/rdf_db), except([rdf_node/1])).

:- use_module(plRdf(api/rdf_build)).
:- use_module(plRdf(api/rdf_read)).
:- use_module(plRdf(api/rdfs_build)).
:- use_module(plRdf(vocabulary/rdf_stat)).

:- use_module(plRdfHtml(rdf_html_triple_table)).

:- multifile(kmc:kmc_value//3).

:- meta_predicate(kmc_0500_oclc(+,+,+,3)).
:- meta_predicate(kmc_0500_status_value(3,+,-,-)).





kmc:kmc_value(500, G, PPN) -->
  [C1],
  {kmc_0500_oclc(G, PPN, C1, 'kmc_0500-1')},

  [C2],
  {kmc_0500_oclc(G, PPN, C2, 'kmc_0500-2')},

  [C3],
  {kmc_0500_oclc(G, PPN, C3, par_103)},

  % The STCN assigns extra meaning to the three codes combined.
  !, {
    atom_codes(Atom, [C1,C2,C3]),
    kmc_0500_stcn(G, PPN, Atom)
  }.
kmc:kmc_value(500, G, PPN) -->
  {debug(kmc_0500, '[PPN ~w] Unrecognized KMC 0500 status value.', [PPN])}.

kmc_0500_stcn(G, PPN, 'Aav'):- !,
  rdf_assert(PPN, stcno:status, stcno:'StatusValue/STCN/Aav', G).
kmc_0500_stcn(G, PPN, 'Abv'):- !,
  rdf_assert(PPN, stcno:status, stcno:'StatusValue/STCN/Abv', G).
kmc_0500_stcn(G, PPN, 'Acv'):- !,
  rdf_assert(PPN, stcno:status, stcno:'StatusValue/STCN/Acv', G).
kmc_0500_stcn(_G, PPN, Value):-
  debug(kmc_0500, '[PPN ~w] Unrecognized compound type: ~w.', [PPN,Value]).



% SUPPORT PREDICATES %

kmc_0500_oclc(G, PPN, Code, Base):-
  char_code(Char, Code),
  % Make sure this is a legal value.
  once(call(Base, Char, _Label, _Comment)), !,
  kmc_0500_status_value(Base, Char, Value, Pred),
  rdf_assert(PPN, Pred, Value, G).
kmc_0500_oclc(_G, PPN, Code, Base):-
  char_code(Char, Code),
  debug(kmc_0500, 'Illegal status value ~w:~w for PPN ~w.', [Base,Char,PPN]).

kmc_0500_status_value(_Mod:Base1, Char, Value, Pred):-
  (  Base1 == 'kmc_0500-1'
  -> Base2 = 'StatusValue/Pos1',
     rdf_global_id(stcno:status_pos1, Pred)
  ;  Base1 == 'kmc_0500-2'
  -> Base2 = 'StatusValue/Pos2',
     rdf_global_id(stcno:status_pos2, Pred)
  ;  Base1 == par_103
  -> Base2 = 'StatusValue/Pos3',
     rdf_global_id(stcno:status_pos3, Pred)),
  atomic_list_concat([Base2,Char], '/', Name),
  rdf_global_id(stcno:Name, Value).



% DOMAIN KNOWLEDGE %

'kmc_0500-1'('A', 'gedrukte tekst',
  '- Gedrukte tekst.\c
   - Gedrukt niet-bewegend beeld in boekvorm uitgegeven.\c
   - Gedrukt cartografisch materiaal in boekvorm uitgegeven.\c
   - Gedrukte brieven in boekvorm uitgegeven.\c
   - Bladmuziek valt niet onder deze categorie.').
'kmc_0500-1'('B', 'bewegend beeld', 'Bewegend beeld (o.a. streaming video).').
'kmc_0500-1'('D', 'losse brieven',
  'Losse brieven al dan niet in een band of een map.\c
   Dit is een specifieke categorie handschriften.\c
   Het gaat hierbij dus niet om in boekvorm verschenen brieven.').
'kmc_0500-1'('F', handschriften,
  'Handschriften inclusief typoscript (m.u.v. losse brieven).').
'kmc_0500-1'('G', geluid, 'Geluid').
'kmc_0500-1'('I', 'niet-bewegend beeld',
  'Niet-bewegend beeld in losse items al dan niet in een band of\c
   een map (bijvoorbeeld losse foto\'s, losse prenten en dia\'s).\c
   Het gaat hierbij dus niet om in boekvorm verschenen niet-bewegend\c
   beeld.').
'kmc_0500-1'('K', 'cartografisch materiaal',
  'Cartografisch materiaal in losse items al dan niet in een band\c
   of een map (bijvoorbeeld losse plattegronden van steden)\c
   (m.u.v. handschriften).').
'kmc_0500-1'('M', bladmuziek, 'Bladmuziek (m.u.v. handschriften).').
'kmc_0500-1'('O', 'online resources',
  'Online resources (hoofdzakelijk tekst), zoals online e-books en online\c
   e-journals.').
'kmc_0500-1'('R', archiefmateriaal, 'Archiefmateriaal').
'kmc_0500-1'('S', software,
  'Software, d.w.z.\c
   - Algemene computerprogramma\'s\c
   - Computergames
   - Multimedia op computerdrager
   - E-books en e-journals op computerdrager\c
     (tekst; geen online resource)).').
'kmc_0500-1'('T', thesaurus, 'Thesaurus').
'kmc_0500-1'('V', objecten,
  'Objecten. De overige materiaalsoorten gelden als \'primair\'.\c
   Alleen \'objecten\' die niet vallen onder een van de meer specifieke\c
   categorieën (A, B, F e.d.), krijgen een \'V\' op de 1e positie van\c
   kmc 0500 met de betekenis van \'voorwerp/object\'. ').
'kmc_0500-1'('X', 'NBD Biblion',
  'Uitsluitend gebruikt door NBD Biblion om records in te voeren t.b.v.\c
   de aanschafinformaties (a.i\'s) voor openbare bibliotheken.').

'kmc_0500-2'('a', 'eendelig document',
  'Eendelig document al dan niet in een reeks.').
'kmc_0500-2'('b', periodiek,
  '- Periodiek (tijdschrift, jaarboek, krant e.d.)\c
   - Koepeltitel van een reeks.').
'kmc_0500-2'('c', koepeltitel, 'Koepeltitel meerdelige publicatie (mp).').
'kmc_0500-2'('e', 'onzelfstandige tussentitel',
  'Onzelfstandige tussentitel binnen meerdelige publicatie (mp).').
'kmc_0500-2'('E', 'zelfstandige tussentitel',
  'Zelfstandige tussentitel binnen meerdelige publicatie (mp).').
'kmc_0500-2'('f', 'onzelfstandige deeltitel',
  'Onzelfstandige deeltitel binnen meerdelige publicatie (mp).').
'kmc_0500-2'('F', 'zelfstandige deeltitel',
  'Zelfstandige deeltitel binnen meerdelige publicatie (mp).').
'kmc_0500-2'('n', 'Online Contents',
  'Abstract gerelateerd aan Online Contents artikel.').
'kmc_0500-2'('N', 'Online Contents',
  'Abstract gerelateerd aan Online Contents artikel.').
'kmc_0500-2'('o', 'Online Contents',
  'Abstract gerelateerd aan Online Contents artikel.').
'kmc_0500-2'('r', convoluut,
  'Convoluut (aantal handschriften in één band) of compilatie.').
'kmc_0500-2'('s', onderdeelbeschrijving,
  'Onderdeelbeschrijving, artikelbeschrijving.').

par_103(a, acquisitie, acquisitie).
par_103('B', 'tape-invoer', 'Titels, afkomstig van tape-invoer,\c
  waarvan na matching is gebleken dat ze wellicht al in het GGC aanwezig\c
  waren. Deze titels kunnen worden omgewerkt naar "normale" titels, maar\c
  zullen veelal verwijderd worden.').
par_103(c, 'C.I.P.', 'C.I.P.').
par_103(p,'bibliografische onvolledigheid', 'Bibliografisch onvolledig record\c
  van een openbare bibliotheek in Nederland in het kader van het NBC project').
par_103(r, retrospectief, retrospectief).
par_103(v, 'bibliografisch volledig', 'Bibliografisch volledig, niet muteerbaar.').
par_103(x, 'tape titel', 'Tape titel/bibliografisch. Volledig, muteerbaar.').
par_103(y, 'bibliografisch onvolledig', 'Bibliografisch onvolledig.').
