:- module(
  kmc_0500,
  [
    assert_schema_kmc_0500/1, % +Graph:atom
    kmc_0500//2, % +Graph:atom
                 % +PPN:uri
    statistics_kmc_0500/2 % +Graph:atom
                          % -Rows:list(list)
  ]
).

/** <module> KMC 0500 - STATUS

# STCN manual

From: http://www.kb.nl/kbhtml/stcnhandleiding/frames.html

Required field. Cannot be repeated.

KMC 0500 contains the status[?].

# Dedicated STCN values (not in GGC)

| *Code* | *Status*                          |
| =Aav=  | Monograph                         |
| =Abv=  | Work consisting of multiple parts |
| =Acv=  | Magazine                          |

# GGC values for the three positions

## Position 1

!!kmc_0500!!value_kmc_055_1!!3!!

## Position 2

!!kmc_0500!!value_kmc_055_2!!3!!

## Position 3

!!par_103!!position3!!3!!

@author Wouter Beek
@see http://www.kb.nl/kbhtml/stcnhandleiding/0500.html
@see http://support.oclc.org/ggc/richtlijnen/php/showPresentation.php?id=12&ln=nl&sec=k-0500
@version 2013/01-2013/04, 2013/06, 2013/09
*/

:- use_module(dcg(dcg_content)).
:- use_module(dcg(dcg_generic)).
:- use_module(kmc(par_103)).
:- use_module(library(debug)).
:- use_module(library(semweb/rdf_db)).
:- use_module(rdf(rdf_build)).
:- use_module(rdf(rdf_statistics)).
:- use_module(rdfs(rdfs_build)).
:- use_module(xml(xml_namespace)).

:- xml_register_namespace(stcn, 'http://stcn.data2semantics.org/resource/').
:- xml_register_namespace(stcnv, 'http://stcn.data2semantics.org/resource/vocab/').

:- debug(kmc_0500).



all_three(G, PPN) -->
  atom('Aav'), !,
  {rdf_assert(PPN, stcnv:status, stcnv:monograph, G)}.
all_three(G, PPN) -->
  atom('Abv'), !,
  {rdf_assert(PPN, stcnv:status, stcnv:multipart_work, G)}.
all_three(G, PPN) -->
  atom('Acv'), !,
  {rdf_assert(PPN, stcnv:status, stcnv:magazine, G)}.
all_three(G, PPN) -->
  {
    debug(kmc_0500, 'Unrecognized compound type for PPN ~w.', [PPN]),
    rdf_assert(PPN, stcnv:status, stcnv:unknown_status, G)
  }.

assert_schema_kmc_0500(G):-
  rdfs_assert_class(stcnv:'StatusValue', G),
  rdfs_assert_label(stcnv:'StatusValue', 'status value', G),
  
  rdfs_assert_subclass(stcnv:'STCNStatusValue', stcnv:'StatusValue', G),
  
  rdf_assert_individual(stcnv:monograph, stcnv:'STCNStatusValue', G),
  
  rdf_assert_individual(stcnv:multipart_work, stcnv:'STCNStatusValue', G),
  rdf_assert_individual(stcnv:magazine, stcnv:'STCNStatusValue', G),
  rdf_assert_individual(stcnv:unknown_status, stcnv:'STCNStatusValue', G),
  rdfs_assert_class(stcnv:'FirstPositionStatusValue', G),
  rdfs_assert_subclass(
    stcnv:'FirstPositionStatusValue',
    stcnv:'StatusValue',
    G
  ),
  rdfs_assert_class(stcnv:'SecondPositionStatusValue', G),
  rdfs_assert_subclass(
    stcnv:'SecondPositionStatusValue',
    stcnv:'StatusValue',
    G
  ),
  forall(
    value_kmc_055_1(Char, Label, Comment),
    (
      atomic_concat('kmc055-1_', Char, Name),
      rdf_global_id(stcnv:Name, FirstPositionStatusValue1),
      rdf_assert_individual(FirstPositionStatusValue1,
        stcnv:'FirstPositionStatusValue', G),
      rdfs_assert_label(FirstPositionStatusValue1, nl, Label, G),
      rdfs_assert_comment(FirstPositionStatusValue1, nl, Comment, G)
    )
  ),
  forall(
    value_kmc_055_2(Char, Label, Comment),
    (
      atomic_concat('kmc055-2_', Char, Name),
      rdf_global_id(stcnv:Name, SecondPositionStatusValue1),
      rdf_assert_individual(SecondPositionStatusValue1,
        stcnv:'SecondPositionStatusValue', G),
      rdfs_assert_label(SecondPositionStatusValue1, nl, Label, G),
      rdfs_assert_comment(SecondPositionStatusValue1, nl, Comment, G)
    )
  ),
  assert_schema_par_103(G),
  rdf_assert_property(stcnv:status, G),
  rdf_assert_literal(stcnv:status, stcnv:kb_name, 'KMC 0500', G),
  rdf_assert(stcnv:status, stcnv:documentation,
    'http://www.kb.nl/kbhtml/stcnhandleiding/0500.html', G).

kmc_0500(G, PPN) -->
  kmc_0500_first(G, PPN, First),
  kmc_0500_second(G, PPN, Second),
  kmc_0500_third(G, PPN, Third),
  {phrase(all_three(G, PPN), [First,Second,Third], _Rest)}.

kmc_0500_first(G, PPN, Code) -->
  [Code],
  {
    char_code(Char, Code),
    value_kmc_055_1(Char, Label, Comment),
    atomic_concat('kmc055-1_', Char, Name),
    rdf_global_id(stcnv:Name, Category),
    rdf_assert(PPN, stcnv:status, Category, G),
    rdfs_assert_label(Category, nl, Label, G),
    rdfs_assert_comment(Category, nl, Comment, G)
  }.

kmc_0500_second(G, PPN, Code) -->
  [Code],
  {
    char_code(Char, Code),
    value_kmc_055_2(Char, Label, Comment),
    atomic_concat('kmc055-2_', Char, Name),
    rdf_global_id(stcn:Name, Category),
    rdf_assert(PPN, stcnv:status, Category, G),
    rdfs_assert_label(Category, nl, Label, G),
    rdfs_assert_comment(Category, nl, Comment, G)
  }.

kmc_0500_third(G, PPN, Code) -->
  [Code],
  {
    char_code(Char, Code),
    value_par_103(Char, Label, Comment),
    atomic_concat('par103_', Char, Name),
    rdf_global_id(stcnv:Name, Category),
    rdf_assert(PPN, stcnv:status, Category, G),
    rdfs_assert_label(Category, nl, Label, G),
    rdfs_assert_comment(Category, nl, Comment, G)
  }.

value_kmc_055_1('A', 'gedrukte tekst',
  '- Gedrukte tekst.\c
   - Gedrukt niet-bewegend beeld in boekvorm uitgegeven.\c
   - Gedrukt cartografisch materiaal in boekvorm uitgegeven.\c
   - Gedrukte brieven in boekvorm uitgegeven.\c
   - Bladmuziek valt niet onder deze categorie.').
value_kmc_055_1('B', 'bewegend beeld',
  'Bewegend beeld (o.a. streaming video).').
value_kmc_055_1('D', 'losse brieven',
  'Losse brieven al dan niet in een band of een map.\c
   Dit is een specifieke categorie handschriften.\c
   Het gaat hierbij dus niet om in boekvorm verschenen brieven.').
value_kmc_055_1('F', handschriften,
  'Handschriften inclusief typoscript (m.u.v. losse brieven).').
value_kmc_055_1('G', geluid, 'Geluid').
value_kmc_055_1('I', 'niet-bewegend beeld',
  'Niet-bewegend beeld in losse items al dan niet in een band of\c
   een map (bijvoorbeeld losse foto\'s, losse prenten en dia\'s).\c
   Het gaat hierbij dus niet om in boekvorm verschenen niet-bewegend\c
   beeld.').
value_kmc_055_1('K', 'cartografisch materiaal',
  'Cartografisch materiaal in losse items al dan niet in een band\c
   of een map (bijvoorbeeld losse plattegronden van steden)\c
   (m.u.v. handschriften).').
value_kmc_055_1('M', bladmuziek, 'Bladmuziek (m.u.v. handschriften).').
value_kmc_055_1('O', 'online resources',
  'Online resources (hoofdzakelijk tekst), zoals online e-books en online\c
   e-journals.').
value_kmc_055_1('R', archiefmateriaal, 'Archiefmateriaal').
value_kmc_055_1('S', software,
  'Software, d.w.z.\c
   - Algemene computerprogramma\'s\c
   - Computergames
   - Multimedia op computerdrager
   - E-books en e-journals op computerdrager\c
     (tekst; geen online resource)).').
value_kmc_055_1('T', thesaurus, 'Thesaurus').
value_kmc_055_1('V', objecten,
  'Objecten. De overige materiaalsoorten gelden als \'primair\'.\c
   Alleen \'objecten\' die niet vallen onder een van de meer specifieke\c
   categorieën (A, B, F e.d.), krijgen een \'V\' op de 1e positie van\c
   kmc 0500 met de betekenis van \'voorwerp/object\'. ').
value_kmc_055_1('X', 'NBD Biblion',
  'Uitsluitend gebruikt door NBD Biblion om records in te voeren t.b.v.\c
   de aanschafinformaties (a.i\'s) voor openbare bibliotheken.').

value_kmc_055_2('a', 'eendelig document',
  'Eendelig document al dan niet in een reeks.').
value_kmc_055_2('b', periodiek,
  '- Periodiek (tijdschrift, jaarboek, krant e.d.)\c
   - Koepeltitel van een reeks.').
value_kmc_055_2('c', koepeltitel,
  'Koepeltitel meerdelige publicatie (mp).').
value_kmc_055_2('e', 'onzelfstandige tussentitel',
  'Onzelfstandige tussentitel binnen meerdelige publicatie (mp).').
value_kmc_055_2('E', 'zelfstandige tussentitel',
  'Zelfstandige tussentitel binnen meerdelige publicatie (mp).').
value_kmc_055_2('f', 'onzelfstandige deeltitel',
  'Onzelfstandige deeltitel binnen meerdelige publicatie (mp).').
value_kmc_055_2('F', 'zelfstandige deeltitel',
  'Zelfstandige deeltitel binnen meerdelige publicatie (mp).').
value_kmc_055_2('n', 'Online Contents',
  'Abstract gerelateerd aan Online Contents artikel.').
value_kmc_055_2('N', 'Online Contents',
  'Abstract gerelateerd aan Online Contents artikel.').
value_kmc_055_2('o', 'Online Contents',
  'Abstract gerelateerd aan Online Contents artikel.').
value_kmc_055_2('r', convoluut,
  'Convoluut (aantal handschriften in één band) of compilatie.').
value_kmc_055_2('s', onderdeelbeschrijving,
  'Onderdeelbeschrijving, artikelbeschrijving.').

statistics_kmc_0500(G, [[A1,V1],[A2,V2],[A3,V3],[A4,V4]]):-
  A1 = 'Monograph publications',
  count_subjects(stcnv:status, stcnv:monograph, G, V1),
  debug(stcn_statistics, '-- ~w: ~w', [A1,V1]),

  A2 = 'Multi-part publications',
  count_subjects(stcnv:status, stcnv:multipart_work, G, V2),
  debug(stcn_statistics, '-- ~w: ~w', [A2,V2]),

  A3 = 'Magazine publications',
  count_subjects(stcnv:status, stcnv:magazine, G, V3),
  debug(stcn_statistics, '-- ~w: ~w', [A3,V3]),

  A4 = 'Publications of another type',
  count_subjects(stcnv:status, stcnv:unknown_status, G, V4),
  debug(stcn_statistics, '-- ~w: ~w', [A4,V4]).

