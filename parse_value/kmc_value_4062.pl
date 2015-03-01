:- module(kmc_value_4062, []).

/** <module> Parse KMC Value: 4062 (Format)

*|KMC 4062|* contains the format, given in the documented format.

The formats are also displayed in the fingerprint kmc_2275.pl (second
position).

# Tables

## Main formats (not all are documented in STCN)

| *Name*          | *Symbol* | *|Number of leaflets|* | *|[Kettinglijnen]|* | *Watermark*                             | *Visual*                     |
| Plano           | 1°       | 1                      | horizontal          | 1/4 [lengteas] halfway [breedteas]      | [[../../www/img/plano.png]]  |
| Folio           | 2°       | 2                      | vertical            | middle                                  | [[../../www/img/folio.png]]  |
| Quarto          | 4°       | 4                      | horizontal          | part: halfway the inner margin          | [[../../www/img/kwarto.png]] |
| Octavo          | 8°       | 8                      | vertical            | part: at the top, in the innner margin  | [[../../www/img/octavo.png]] |
| Duodecimolong   | 12°      | 12                     | vertical            | part: halfway of the top margin         | [[../../www/img/duodl.png]]  |
| Duodecimocommon | 12°      | 12, 8, 6 or 4          | horizontal          | 1/3 outer margin                        | [[../../www/img/duodc.png]]  |
| 16mo            | 16°      | 8                      | horizontal          | part: at the top, in the outer margin   | [[../../www/img/16mo.png]]   |
| 18mo            | 18°      | 18, 12/6, 3x6 or 10/8  | vertical            | middle leaflet                          | [[../../www/img/18mo.png]]   |
| 24mo-long       | 24°      | 3x8 or 2x12            | vertical            | part: haldway the outer margin          | [[../../www/img/24mol.png]]  |
| 24mo            | 24°      | 3x8 or 2x12            | horizontal          | part: at the top, in the inner margin   | [[../../www/img/24mo.png]]   |
| 32mo            | 32°      | 32                     | vertical            | *Undocumented*                          | *Undocumented*               |
| 64mo            | 64°      | 64                     | horizontal          | *Undocumented*                          | *Undocumented*               |
| Other           | Other    | 0                      | does not apply      | *Undocumented*                          | *Undocumented*               |

## Format modifiers, that occur in combination with a main format

These can occur either before or after the main format specifier (except for
the =OtherFormat= main format, which takes no modifiers).

| *Name*       |
| =Agenda=     |
| =Broadsheet= |
| =Oblong=     |

## Undocumented or incorrectly documented formats

| *|Unrecognized value|* | *|Assumed equivalent|* | *Comment*                            |
| =|?|=                  | =UnknownFormat=        |                                      |
| =|°|=                  | =UnknownFormat=        |                                      |
| =|4ç|=                 | =|4°|=                 |                                      |
| =4=, for example       | =|4°|=, for example    | Recognized values without the '°'.   |
| =agenda=               | =Agenda=               | Case-distinction                     |
| =oblong=               | =Oblong=               | Case-distinction                     |
| =|Other°|=             | =Other=                | Recognized values with an extra '°'. |
| =|8 °|=                | =|8°|=                 | E.g. PPN 236634119                   |
| =|8ê|=                 | =|8°|=                 | E.g. PPN 109363094                   |
| =|12 °|=               | =|12°|=                | E.g. PPN 189120959                   |
| =|?ê|=                 | =|Unknown|=            | E.g. PPN 109361423                   |
| =|4ê|=                 | =|4°|=                 | E.g. PPN 108526747                   |
| =|2ê|=                 | =|2°|=                 | E.g. PPN 108495264                   |
| <m> x <n> mm           | =Unknown=              | E.g. PPN 317798766                   |

@tbd Verify this using KMC 2275 redundancy.

# PPN 333614321

Strange string.

# PPN 31858817X

Strange string: '2 volumes'

# PPN 31858817X

'93 x 162 mm'

Swapped with KMC 6040.

# PPN 291582117

Extra space at beginning.

# PPN 115264302

48mo

# Duodecimolong or Duodecimocommon

---

@author Wouter Beek
@version 2015/02
*/

:- use_module(library(dcg/basics)).
:- use_module(library(debug)).
:- use_module(library(semweb/rdf_db), except([rdf_node/1])).

:- use_module(plc(dcg/dcg_ascii)).
:- use_module(plc(dcg/dcg_atom)).
:- use_module(plc(dcg/dcg_content)).

:- use_module(plRdf(api/rdf_build)).
:- use_module(plRdf(api/rdf_read)).
:- use_module(plRdf(api/rdfs_build)).
:- use_module(plRdf(vocabulary/rdf_stat)).

:- use_module(plRdfHtml(rdf_html_triple_table)).

:- use_module(stcn(kmc/kmc_4062)).

:- multifile(kmc:kmc_value//3).





% Documented values.
% The extra format properties, such as 'agenda' and 'oblong', can occur
% either before or after the main format descriptor.
kmc:kmc_value(4062, Graph, Publication) -->
  extra(Graph, Publication),
  main(Graph, Publication),
  extra(Graph, Publication), !.

% What's this?
% PPN 333614321
extra(_, _) -->
  blanks,
  atom('Van WingheLiesvelt translation`LO`').
% What to do with this 'volumes' word?
% PPN 31858817X
extra(_, _) -->
  blanks,
  atom(volumes).
extra(Graph, Publication) -->
  blanks,
  % Case-insensitive parsing is needed, because sometimes
  % 'Agenda' and 'agenda' occur, as well as 'Oblong' and 'oblong'.
  atom(Word),
  {
    Word \== '',
    downcase_atom(Word, LowercaseWord),
    rdf_global_id(stcno:LowercaseWord, Format),
    rdf_assert(Publication, stcno:format, Format, Graph)
  },
  blanks.
extra(_, _) --> [].

main(Graph, Publication) -->
  integer(_),
  blanks,
  "x",
  blanks,
  integer(_Second),
  blanks,
  "mm",
  {rdf_assert(Publication, stcno:format, stcno:unknown_format, Graph)}.
main(Graph, Publication) -->
  blanks,
  integer(Number),
  blanks,
  (   % °
      [176]
  ;   % ê
      [234],
      (question_mark, "")
  ;   % ç
      [231]
  ;   ""
  ),
  {
    translate_format(Format, Number, _NumberOFLeaflets),
    rdf_assert(Publication, stcno:format, Format, Graph)
  }.
% Broadsheet.
% Some PPNs have a format that is not in the documentation for this KMC.
main(Graph, Publication) -->
  atom('Broadsheet'),
  {rdf_assert(Publication, stcno:format, stcno:broadsheet, Graph)}.
% Undocumented and/or malformed formats.
main(Graph, Publication) -->
  {kmc_4062_malformed(FormatAtom, FormatName)},
  atom(FormatAtom),
  {
    rdf_global_id(stcn:FormatName, Format),
    rdf_assert(Publication, stcno:format, Format, Graph)
  }.
