:- module(
  kmc_1200,
  [
    kmc_1200_table/3 % ?Category:atom
                     % ?Label:atom
                     % ?Comment:atom
  ]
).

/** <module> Background Knowledge: KMC 1200

Background knowledge for KMC 1200.

@author Wouter Beek
@version 2015/02
*/





%! kmc_1200_table(?Category:atom, ?Label:atom, ?Comment:atom) is nondet.

kmc_1200_table('Boekenlijsten', boekenlijsten,
  'Waar mogelijk worden van deze lijsten kopieën gemaakt\c
   met aangeniet een kopie van de tp. (zie ook kmc 7134).').
kmc_1200_table('Diversen', diversen, '').
kmc_1200_table('Illustraties', illustraties, '').
kmc_1200_table('Lettertype', lettertype,
  'NB: Voor Romein, Gotisch, Grieks, Hebreeuws, Arabisch, Armeens en Cyrillish\c
   geldt dat er meer in dit lettertype gezet moet zijn dan een enkel citaat,\c
   dus substantieële tekstgedeelten.').
kmc_1200_table('Titelpagina', titelpagina, '').

%! kmc_1200_table(?Category:atom, ?Char:char, ?Label:atom, ?Comment:atom) is nondet.

kmc_1200_table('Boekenlijsten', d, 'van auteur', '(geen kopie maken)').
kmc_1200_table('Boekenlijsten', e, fondslijst,
  'De uitgever heeft de boeken gedrukt of\c
   houdt het kopijrecht en biedt ze in groten getale te koop (bij voorbeeld:\c
   `Catalogus van eenige Nederduytse boeken die gedrukt of te bekoomen zyn by\c
   Boudewyn vander Aa\').').
kmc_1200_table('Boekenlijsten', f, assortment,
  'Exemplaren van deze boeken zijn in de betreffende\c
   boekwinkel te koop (bij voorbeeld: `Register van Nederduytse reghts-geleerde\c
   boeken, dewelke [...] tot Amsterdam, by Hendrik en Dirk Boom, boekverkoopers,\c
   te bekomen zijn\').').
kmc_1200_table('Boekenlijsten', g, diverse,
  'Hieronder vallen bij voorbeeld veilingcatalogi,\c
   maar ook advertenties. Onder advertenties verstaan we aankondigingen van\c
   een of enkele boeken in een lopende tekst.').
kmc_1200_table('Diversen', '3', 'lijst van intekenaren',
  'Lijsten van intekenaren en oproepen tot intekening komen vooral voor als\c
   onderdeel van voor- of nawerk, maar kunnen ook, als een soort prospectus,\c
   zelfstandig voorkomen. Zelfstandig uitgekomen advertenties voor de\c
   inschrijving op een boek (kan ook een reeks of tijdschrift zijn), zonder\c
   lijst van intekenaars, krijgen een \'3\' en een \'g\' en, indien uit de\c
   titel niet al duidelijk blijkt dat het om een inschrijving gaat, in\c
   kmc 4201 de annotatie Proposal for printing by subscription. Tevens komt\c
   de auteur van het beoogde werk in kmc 3011 en wordt ontsloten op\c
   Documentary information EN het onderwerp van het betreffende boek. Gaat\c
   het niet om een intekenlijst maar om een advertentie-achtig blaadje,\c
   dan 4201 advertisement brochure. Gebruik in het Engels niet het woord\c
   \'prospectus\', dat is meer voor bedrijven of scholen die zichzelf\c
   aanprijzen.\c
   Boekverkopers die als intekenadres op zulke advertenties voorkomen,\c
   worden in beschrijving en thesaurus verder genegeerd, maar er wordt wel\c
   een annotatie toegevoegd (zie kmc 4040).').
kmc_1200_table('Diversen', '4', prijsopgave,
  'Ook bij opmerkingen als \'gratis\' of \'bijdrage voor de armen\'.').
kmc_1200_table('Diversen', '8', boekverkoperslijst,
  'De 8 wordt door het controlescript toegevoegd indien aanwezig in 700X.').
% Some PPNs, e.g., 271591978, have this value which is undocumented.
kmc_1200_table('Diversen', '5', onbekend, '').
% Some PPNs, e.g., 301032831, have this value which is undocumented.
kmc_1200_table('Diversen', '9', onbekend, '').
kmc_1200_table('Illustraties', a, 'illustratie op de titelpagina',
  'Op te vatten in ruime zin, dus ook een gegraveerd titelblad, een frontispice,\c
   een auteursportret voor of na de titel, een illustratie op de bedrukte omslag,\c
   enz., doch niet vignetten en dergelijke als typografisch materiaal op te\c
   vatten boekversieringen.').
kmc_1200_table('Illustraties', b, 'illustraties buiten collatie',
  'Als illustraties gelden ook kaarten.').
kmc_1200_table('Illustraties', c,  'andere illustraties binnen collatie',
  'Dit zijn alle illustraties binnen de collatie, met uitzondering van degene\c
   die code a krijgen. Als een boek geheel of gedeeltelijk uit gegraveerde\c
   bladen bestaat en de collatie luidt: `engraved folia\', dan vallen deze\c
   illustraties binnen de collatie, en wordt dus kenmerk c toegekend.').
kmc_1200_table('Lettertype', i, romein, '').
kmc_1200_table('Lettertype', j, gotisch, '').
kmc_1200_table('Lettertype', k, cursief, '').
kmc_1200_table('Lettertype', l, 'civilité', '').
kmc_1200_table('Lettertype', m, 'Grieks', '').
kmc_1200_table('Lettertype', n, 'Hebreeuws', '').
kmc_1200_table('Lettertype', o, 'Arabisch', '').
kmc_1200_table('Lettertype', p, 'Armeens', '').
kmc_1200_table('Lettertype', q, muzieknoten,
  'Ook als er maar één muzieknoot in de hele tekst staat.').
kmc_1200_table('Lettertype', r, cyrillisch, '').
kmc_1200_table('Lettertype', s, overige, '').
kmc_1200_table('Titelpagina', a, 'illustratie op de titelpagina',
  'Op te vatten in ruime zin, dus ook een gegraveerd titelblad, een\c
   frontispice, een auteursportret voor of na de titel, een illustratie op\c
   de bedrukte omslag, enz., doch niet vignetten en dergelijke als typografisch\c
   materiaal op te vatten boekversieringen.').
kmc_1200_table('Titelpagina', h, drukkersmerk,
  'Vignetten van bij voorbeeld de Amsterdamse Academie (de Bijenkorf) of van\c
   Nil Volentibus Arduum worden niet beschouwd als drukkersmerk, maar als\c
   illustratie op de titelpagina (a).').
% This occus in the STCN documentation but not in the GGC documentation.
kmc_1200_table('Titelpagina', v, 'bedrukte omslag',
  'Een bedrukte omslag kan voorkomen naast een typografische of een\c
   gegraveerde titelpagina, maar ook in plaats daarvan, zie kmc 4000.\c
   Gegevens afkomstig van een bedrukte omslag zoals de prijs of een\c
   boekenlijst, krijgen ook de betreffende code.').
kmc_1200_table('Titelpagina', w, 'gegraveerde titelpagina',
  'Hieronder wordt verstaan een titelblad dat geheel door middel van een\c
   gravure of houtsnede is vervaardigd. Een gegraveerde titelpagina hoeft\c
   niet per definitie een illustratie te bevatten, maar kan louter uit\c
   gegraveerde tekst bestaan; in het eerste geval krijgt hij zowel code a\c
   als code w, in het tweede geval alleen code w. Een titelpagina die bijna\c
   geheel uit een gravure of houtsnede bestaat, maar waarvan de titel gevormd\c
   is door middel van zetsel, is een typografische titelpagina, met illustratie.\c
   (N.B. Het verschil tussen een gegraveerd titelblad en een frontispice is de\c
   aan- resp. afwezigheid van titel of impressum. Zowel gegraveerde titelpagina\c
   als frontispice worden opgenomen in de collatieformule.)').
kmc_1200_table('Titelpagina', x, 'typografische titelpagina', '').
kmc_1200_table('Titelpagina', y, 'geen titelpagina', '').
kmc_1200_table('Titelpagina', z, meerkleurig,
  'Ook als het gehele werk in een kleur (dus niet zwart) is gedrukt.').
