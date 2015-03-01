:- module(kmc_value_1200, []).

/** <module> Parse KMC Value: 1200 (Typographic properties)

A KMC 1200 value consist (ideally) of one character,
but sometimes it consists of two characters (erroneously).
Duo to typos of this latter kind we need to parse 2-character
values as well.

PPN 14162843X has a very strange value for this KMC.

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

---

@author Wouter Beek
@tbd http://www.kb.nl/kbhtml/stcnhandleiding/1200.html
@tbd http://support.oclc.org/ggc/richtlijnen/php/showPresentation.php?id=12&ln=nl&sec=k-1200
@version 2015/02
*/

:- use_module(library(debug)).
:- use_module(library(semweb/rdf_db), except([rdf_node/1])).

:- use_module(plRdf(api/rdf_build)).
:- use_module(plRdf(api/rdf_read)).
:- use_module(plRdf(api/rdfs_build)).
:- use_module(plRdf(vocabulary/rdf_stat)).

:- use_module(stcn(kmc/kmc_1200)).

:- multifile(kmc:kmc_value//3).





kmc:kmc_value(1200, _, Publication) -->
  atom(aeloude), !,
  '...'(Codes),
  end_of_line, !,
  {
    atom_codes(Atom, Codes),
    debug(
      kmc_1200,
      '[Publication ~w] Unrecognized value for KMC 1200: ~a',
      [Publication,Atom]
    )
  }.
kmc:kmc_value(1200, Graph, Publication) -->
  [C1,C2],
  {
    atom_codes(Atom, [C1,C2]),
    kmc_1200_translate(Atom, Char), !,
    debug(kmc_1200, '[Publication ~w] Typo in value ~w.', [Publication,Atom]),
    kmc_1200(Graph, Publication, Char)
  }.
kmc:kmc_value(1200, Graph, Publication) -->
  [C1],
  {
    char_code(Char1, C1),
    (   kmc_1200_translate(Char1, Char2)
    ->  debug(
          kmc_1200,
          '[PPN ~w] Typo in value ~w -> ~w.',
          [Publication,Char1,Char2]
        )
    ;   Char2 = Char1
    ),
    kmc_1200(Graph, Publication, Char2), !
  }.
kmc:kmc_value(1200, _, Publication) -->
  {debug(kmc_1200, '[PPN ~w] KMC 1200 cannot be parsed.', [Publication])}.

kmc_1200(Graph, Publication, Char1):-
  kmc_1200_table(Category, Char1, _Label, _Comment1),
  kmc_1200_table(Category, Relation1, _Comment2),
  rdf_global_id(stcno:Relation1, Relation2),
  atomic_list_concat(['TypografischKenmerk',Category,Char1], '/', Char2),
  rdf_global_id(stcno:Char2, Char3),
  rdf_assert(Publication, Relation2, Char3, Graph).

%! kmc_1200_translate(+From:atom, -To:atom) is semidet.
% Fix known typocs in KMC 1200 values.

% `1` is assumed to be a typo of `l`. Example: PPN 321339320.
kmc_1200_translate('1', l).
% `b)` is assumend to be a typo of `b`. Example: PPN 261654624.
kmc_1200_translate('b)', b).
% `C` is assumed to be a typo of `c`. Example: PPN 314559655.
kmc_1200_translate('C',  c).
% `w.` is assumed to be a typo of `w`. Example: 204513316.
kmc_1200_translate('w.', w).
% `X` is assumed to be a typo of `x`. Example: 298324997.
kmc_1200_translate('X', x).
% `Y` is assumed to be a typo of `y`. Example: PPN 305305972.
kmc_1200_translate('Y', y).
