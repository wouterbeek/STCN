:- module(
  kmc_1200,
  [
    assert_schema_kmc_1200/1, % +Graph:graph
    kmc_1200//2, % +Graph:atom
                 % +PPN:uri
    kmc_1200_picarta//2, % +Graph:atom
                         % +PPN:uri
    statistics_kmc_1200/2 % +Graph:atom
                          % -Rows:list(list)
  ]
).

/** <module> KMC 1200 - TYPOGRAPHIC PROPERTIES

Verplicht veld. Herhaalbaar.

KMC 1200 bevat de typografische kenmerken.

Vorm: één letter

# Parsing problems

## 1

PPN 14162843X seems to use the KMC code 1200 mistakenly.

## 2

'1' (the number between 0 and 2) is assumed to be a typo of 'l'
(the letter between 'k' and 'm'), e.g. PPN 321339320.

## 3

'X' (uppercase x) is assumed to be a type of 'x' (lowercase x),
e.g. PPN 298324997.

## 4

Some PPNs, e.g., PPN 271591978, have the undocumented value '5'.

## 5

Some PPNs, e.g., PPN 301032831, have the undocumented value '9'.

## 6

'w.' (with the dot) is assumed to be a typo of 'w' (without the dot),
e.g. PPN 204513316.

This one is particularly nasty, since it implies that the typographic property
cannot be assumed to be a single character.

## 7

'v' occurs in the STCN documentation, but not in the GGC documentation.

## 8

PPN 14162843X uses this code wrongly. The parser breaks on this.

## 9

PPN 261654624 uses 'b)' which is assumend to be a typo of 'b'.

## 10

'Y' is assumed to be a typo of 'y', e.g. PPN 305305972.

## 11

'C' --> 'c'
PPN 314559655

# Titelpagina

  * x, typografische titelpagina
  * y, geen titelpagina
  * a, illustratie op de titelpagina
       Op te vatten in ruime zin, dus ook een gegraveerd titelblad,
       een frontispice, een auteursportret voor of na de titel,
       een illustratie op de bedrukte omslag, enz., doch niet vignetten
       en dergelijke als typografisch materiaal op te vatten
       boekversieringen.
  * w, gegraveerde titelpagina
       Hieronder wordt verstaan een titelblad dat geheel door middel van
       een gravure of houtsnede is vervaardigd. Een gegraveerde titelpagina
       hoeft niet per definitie een illustratie te bevatten, maar kan louter
       uit gegraveerde tekst bestaan; in het eerste geval krijgt hij zowel
       code =a= als code =w=, in het tweede geval alleen code =w=. Een
       titelpagina die bijna geheel uit een gravure of houtsnede bestaat,
       maar waarvan de titel gevormd is door middel van zetsel, is een
       typografische titelpagina, met illustratie. (N.B. Het verschil
       tussen een gegraveerd titelblad en een frontispice is de aan- resp.
       afwezigheid van titel of impressum. Zowel gegraveerde titelpagina als
       frontispice worden opgenomen in de collatieformule.)
  * z, meerkleurig
       Ook als het gehele werk in een kleur (dus niet zwart) is gedrukt.
  * h, drukkersmerk
       Vignetten van bij voorbeeld de Amsterdamse Academie (de Bijenkorf)
       of van Nil Volentibus Arduum worden niet beschouwd als drukkersmerk,
       maar als illustratie op de titelpagina (a).
  * v, bedrukte omslag
       Een bedrukte omslag kan voorkomen naast een typografische of een
       gegraveerde titelpagina, maar ook in plaats daarvan, zie kmc 4000.
       Gegevens afkomstig van een bedrukte omslag zoals de prijs of een
       boekenlijst, krijgen ook de betreffende code.

 # Lettertype

 NB: Voor Romein, Gotisch, Grieks, Hebreeuws, Arabisch, Armeens
 en Cyrillish geldt dat er meer in dit lettertype gezet moet zijn dan een
 enkel citaat, dus substantieële tekstgedeelten.
   * i, romein
   * j, gotisch
   * k, cursief
   * l, civilité
     Klik hier voor een impressum in civilité.
   * m, Grieks
   * n, Hebreeuws
   * o, Arabisch
   * p, Armeens
   * r, cyrillisch
   * q, muzieknoten
     Ook als er maar één muzieknoot in de hele tekst staat.
   * s, overige

 # Illustraties

   * a, illustratie op de titelpagina
     Op te vatten in ruime zin, dus ook een gegraveerd titelblad, een
     frontispice, een auteursportret voor of na de titel, een illustratie
     op de bedrukte omslag, enz., doch niet vignetten en dergelijke als
     typografisch materiaal op te vatten boekversieringen.
   * b, illustraties buiten collatie
     Als illustraties gelden ook kaarten.
   * c, andere illustraties binnen collatie
     Dit zijn alle illustraties binnen de collatie, met uitzondering van
     degene die code =a= krijgen. Als een boek geheel of gedeeltelijk uit
     gegraveerde bladen bestaat en de collatie luidt: `engraved folia',
     dan vallen deze illustraties binnen de collatie, en wordt dus kenmerk
     =c= toegekend.

 # Boekenlijsten

Waar mogelijk worden van deze lijsten kopieën gemaakt met aangeniet een
kopie van de tp. (zie ook kmc 7134)
   * d, van auteur	(geen kopie maken)
   * e, fondslijst
     De uitgever heeft de boeken gedrukt of houdt het kopijrecht en biedt
     ze in groten getale te koop (bij voorbeeld: `Catalogus van eenige
     Nederduytse boeken die gedrukt of te bekoomen zyn by Boudewyn vander
     Aa').
   * f, assortimentslijst
     Exemplaren van deze boeken zijn in de betreffende boekwinkel te koop
     (bij voorbeeld: `Register van Nederduytse reghts-geleerde boeken,
     dewelke [...] tot Amsterdam, by Hendrik en Dirk Boom, boekverkoopers,
     te bekomen zijn').
   * g, diversen
     Hieronder vallen bij voorbeeld veilingcatalogi, maar ook advertenties.
     Onder advertenties verstaan we aankondigingen van een of enkele boeken
     in een lopende tekst.

 # Diversen

   * 3, lijst van intekenaren
     Lijsten van intekenaren en oproepen tot intekening komen vooral voor
     als onderdeel van voor- of nawerk, maar kunnen ook, als een soort
     prospectus, zelfstandig voorkomen. Zelfstandig uitgekomen advertenties
     voor de inschrijving op een boek (kan ook een reeks of tijdschrift
     zijn), zonder lijst van intekenaars, krijgen een '3'en een 'g' en,
     indien uit de titel niet al duidelijk blijkt dat het om een
     inschrijving gaat, in kmc 4201 de annotatie Proposal for printing by
     subscription. Tevens komt de auteur van het beoogde werk in kmc 3011 en
     wordt ontsloten op Documentary information EN het onderwerp van het
     betreffende boek. Gaat het niet om een intekenlijst maar om een
     advertentie-achtig blaadje, dan 4201 advertisement brochure. Gebruik in
     het Engels niet het woord 'prospectus', dat is meer voor bedrijven of
     scholen die zichzelf aanprijzen. Boekverkopers die als intekenadres op
     zulke advertenties voorkomen, worden in beschrijving en thesaurus
     verder genegeerd, maar er wordt wel een annotatie toegevoegd (zie kmc
     4040).
   * 4, prijsopgave
     Ook bij opmerkingen als 'gratis' of 'bijdrage voor de armen'
   * 8, boekverkoperslijst
     De 8 wordt door het controlescript toegevoegd indien aanwezig in 700X.

@author Wouter Beek
@version 2013/01-2013/04, 2013/06, 2013/09
*/

:- use_module(dcg(dcg_ascii)).
:- use_module(dcg(dcg_content)).
:- use_module(dcg(dcg_generic)).
:- use_module(library(semweb/rdf_db)).
:- use_module(rdf(rdf_build)).
:- use_module(rdf(rdf_statistics)).
:- use_module(rdfs(rdfs_build)).
:- use_module(xml(xml_namespace)).

:- xml_register_namespace(stcn, 'http://stcn.data2semantics.org/resource/').
:- xml_register_namespace(stcnv, 'http://stcn.data2semantics.org/resource/vocab/').



assert_schema_kmc_1200(G):-
  rdfs_assert_property_class(stcnv:'TypographicProperty', G),
  rdfs_assert_label(stcnv:'TypographicProperty', nl,
    'typografische eigenschap', G),
  rdf_assert_property(stcnv:typographic_property, G),
  rdfs_assert_label(stcnv:typographic_property, nl,
    'typografische eigenschap', G),
  rdf_assert_literal(stcnv:typographic_property, stcnv:kb_name, 'KMC 1200',
    G),
  rdf_assert(stcnv:typographic_property, stcnv:documentation,
    'http://www.kb.nl/kbhtml/stcnhandleiding/1200.html', G),
  rdf_assert_literal(stcnv:typographic_property, stcnv:picarta_name, nl,
    'Typografische informatie', G),
  
  rdfs_assert_subproperty(stcnv:book_list, stcnv:typographic_property, G),
  rdfs_assert_label(stcnv:titlepage, nl, 'boeken lijst', G),
  
  rdfs_assert_subproperty(stcnv:font_type, stcnv:typographic_property, G),
  rdfs_assert_label(stcnv:titlepage, nl, lettertype, G),
  
  rdfs_assert_subproperty(stcnv:illustration, stcnv:typographic_property, G),
  rdfs_assert_label(stcnv:titlepage, nl, illustratie, G),
  
  rdfs_assert_subproperty(stcnv:other, stcnv:typographic_property, G),
  rdfs_assert_label(stcnv:titlepage, nl, anders, G),
  
  rdfs_assert_subproperty(stcnv:titlepage, stcnv:typographic_property, G),
  rdfs_assert_label(stcnv:titlepage, nl, titelpagina, G).

book_list(d, author).
book_list(e, fund).
book_list(f, assortment).
book_list(g, diverse).

font_type(i, roman_font_type).
font_type(j, gothic_font_typeh).
font_type(k, italic_font_type).
font_type(l, 'civilité_font_type').
% Assumed to be a typo of 'l', e.g. PPN 321339320.
font_type('1', 'civilité_font_type').
font_type(m, greek_font_type).
font_type(n, hebrew_font_type).
font_type(o, arabic_font_type).
font_type(p, armenian_font_type).
font_type(q, music_note_font_type).
font_type(r, cyrillic_font_type).
font_type(s, other_font_type).

illustration('a',  illustration_on_titlepage).
illustration('b',  illustration_outside_collation).
% This is assumend to be a typo of 'b', e.g. PPN 261654624.
illustration('b)', illustration_outside_collation).
% E.g. PPN 314559655.
illustration('C',  illustration_inside_collation).
illustration('c',  illustration_inside_collation).

other('3', intekenaren_other).
other('4', prize_other).
other('8', bookseller_other).
% Some PPNs, e.g., 271591978, have this value which is undocumented.
other('5', unknown_other).
% Some PPNs, e.g., 301032831, have this value which is undocumented.
other('9', unknown_other).

% This applies to KMC 1200 values as they occur
% in the =Redactiebladen.txt= file.
kmc_1200(G, PPN) -->
  % A KMC 1200 value consist (ideally) of one character,
  % but sometimes it consists of two characters (erroneously).
  ( [X], {atom_codes(Atom, [X])}
  ; [X,Y], {atom_codes(Atom, [X,Y])}
  ),
  {typographic_property(G, PPN, Atom)}.
% PPN 14162843X uses this KMC to register a long title.
% We skip this statement entirely.
kmc_1200(_G, PPN) -->
  {rdf_global_id(stcn:'14162843X', PPN)},
  atom(aeloude).
% Debug.
kmc_1200(_G, _PPN) -->
  dcg_debug.

% This applies to KMC 1200 values as they occur in content that
% was scraped from Picarta.
kmc_1200_picarta(G, PPN) -->
  [Code],
  {code_type(Code, alnum)},
  space,
  opening_bracket,
  dcg_until(
    [end_mode(exclusive),output_format(atom)],
    closing_bracket,
    LabelNL
  ),
  closing_bracket,
  {
    char_code(Char, Code),
    typographic_property(G, PPN, Char),
    rdf_has(PPN, stcnv:typographic_property, TypographicProperty),
    rdfs_assert_label(TypographicProperty, nl, LabelNL, G)
  }.
kmc_1200_picarta(_Graph, _PPN) -->
  dcg_debug.

statistics_kmc_1200(
  G,
  [[A1, V1], [A2, V2], [A3, V3], [A4, V4], [A5, V5], [A6, V6]]
):-
  A1 = 'Number of typographic properties',
  count_subjects(stcnv:typographic_property, _, G, V1),
  debug(stcn_statistics, '~w: ~w', [A1, V1]),

  A2 = 'Number of book lists',
  count_subjects(stcnv:book_list, _, G, V2),
  debug(stcn_statistics, '-- ~w: ~w', [A2, V2]),

  A3 = 'Number of font types',
  count_subjects(stcnv:font_type, _, G, V3),
  debug(stcn_statistics, '-- ~w: ~w', [A3, V3]),

  A4 = 'Number of illustration',
  count_subjects(stcnv:illustration, _, G, V4),
  debug(stcn_statistics, '-- ~w: ~w', [A4, V4]),

  A5 = 'Number of others',
  count_subjects(stcnv:other, _, G, V5),
  debug(stcn_statistics, '-- ~w: ~w', [A5, V5]),

  A6 = 'Number of title pages',
  count_subjects(stcnv:titlepage, _, G, V6),
  debug(stcn_statistics, '-- ~w: ~w.', [A6, V6]).

titlepage('h', printers_mark_titlepage).
% This occus in the STCN documentation but not in the GGC documentation.
titlepage('v', printed_surface_titlepage).
titlepage('w', engraved_titlepage).
% Probably a typo of 'w', e.g. 204513316.
titlepage('w.', engraved_titlepage).
titlepage('x', typographic_titlepage).
% Probably a typo of 'x', e.g. 298324997.
titlepage('X', typographic_titlepage).
titlepage('y', no_titlepage).
% This is assumed to be a typo of 'y', e.g. PPN 305305972.
titlepage('Y', no_titlepage).
titlepage('z', multicolor_titlepage).

typographic_property(G, PPN, Atom):-
  book_list(Atom, Name), !,
  rdf_global_id(stcnv:Name, Object),
  rdf_assert(PPN, stcnv:book_list, Object, G).
typographic_property(G, PPN, Atom):-
  font_type(Atom, Name), !,
  rdf_global_id(stcnv:Name, Object),
  rdf_assert(PPN, stcnv:font_type, Object, G).
typographic_property(G, PPN, Atom):-
  illustration(Atom, Name), !,
  rdf_global_id(stcnv:Name, Object),
  rdf_assert(PPN, stcnv:illustration, Object, G).
typographic_property(G, PPN, Atom):-
  other(Atom, Name), !,
  rdf_global_id(stcnv:Name, Object),
  rdf_assert(PPN, stcnv:other, Object, G).
typographic_property(G, PPN, Atom):-
  titlepage(Atom, Name), !,
  rdf_global_id(stcnv:Name, Object),
  rdf_assert(PPN, stcnv:titlepage, Object, G).

