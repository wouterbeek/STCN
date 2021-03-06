:- module(
  kmc_1500,
  [
    recognized_language/2 % ?Code:atom
                          % ?Name:atom
  ]
).

/** <module> KMC 1500

@author Wouter Beek
@version 2015/02
*/





%! recognized_language(?Code:atom, ?Name:atom) is nondet.
% GGC-defined KMC 1500 codes and language names that relate to ISO 639-3
% language codes (and thus to Lexvo URIs).
% Language codes in ISO 639-3.

recognized_language(abk, 'Abchazisch').
recognized_language(ace, 'Acehs, Atjehs').
recognized_language(ada, 'Adangme').
recognized_language(ady, 'Adyghe, Adygei').
recognized_language(aar, 'Afar').
recognized_language(ari, 'Afrihili').
recognized_language(afr, 'Afrikaans').
recognized_language(ain, 'Ainu').
recognized_language(aka, 'Akan').
recognized_language(akk, 'Akkadisch').
recognized_language(ale, 'Aleut').
recognized_language(alu, 'Aluku, Boni').
recognized_language(amh, 'Amharisch').
recognized_language(ang, 'Angelsaksisch').
recognized_language(anp, 'Angika').
recognized_language(ara, 'Arabisch').
recognized_language(arg, 'Aragonees').
recognized_language(ars, 'Aramees').
recognized_language(arp, 'Arapahoe').
recognized_language(arn, 'Araukaans').
recognized_language(arw, 'Arawak').
recognized_language(rup, 'Aromaans').
recognized_language(asm, 'Assameens').
recognized_language(ave, 'Avestisch').
recognized_language(awa, 'Awaars').
recognized_language(aad, 'Awadhi').
recognized_language(aym, 'Aymara').
recognized_language(aze, 'Azerbajdjaans').
recognized_language(ast, 'Bable (Asturisch)').
recognized_language(bli, 'Balinees').
recognized_language(bal, 'Baloetsji').
recognized_language(bam, 'Bambara').
recognized_language(baa, 'Basa').
recognized_language(bak, 'Basjkiers').
recognized_language(bas, 'Baskisch').
recognized_language(bej, 'Beja').
recognized_language(bem, 'Bemba').
recognized_language(ben, 'Bengaals').
recognized_language(bho, 'Bhoipuri').
recognized_language(bik, 'Bikol').
recognized_language(byn, 'Bilin').
recognized_language(bin, 'Bini').
recognized_language(bis, 'Bislama').
recognized_language(bla, 'Blackfoot').
recognized_language(zbl, 'Blissymbolics').
recognized_language(bor, 'Borneotalen, Kalimantantalen').
recognized_language(bos, 'Bosnisch').
recognized_language(bra, 'Braj').
recognized_language(bre, 'Bretons').
recognized_language(bug, 'Buginees').
recognized_language(bul, 'Bulgaars').
recognized_language(bua, 'Buriat').
recognized_language(cad, 'Caddo').
recognized_language(cam, 'Cambodjaans').
recognized_language(car, 'Caribisch').
recognized_language(cat, 'Catalaans').
recognized_language(ceb, 'Cebuano').
recognized_language(chg, 'Chagatai').
recognized_language(cha, 'Chamorro').
recognized_language(chr, 'Cherokee').
recognized_language(chy, 'Cheyenne').
recognized_language(chb, 'Chibcha').
recognized_language(chn, 'Chinook').
recognized_language(chp, 'Chipewyan').
recognized_language(cho, 'Choctaw').
recognized_language(cor, 'Cornisch').
recognized_language(cos, 'Corsicaans').
recognized_language(cre, 'Cree').
recognized_language(hat, 'Creools, Haïtiaans Frans').
recognized_language(dak, 'Dakota').
recognized_language(dar, 'Dargva').
recognized_language(dee, 'Deens').
recognized_language(del, 'Delaware').
recognized_language(din, 'Dinka').
recognized_language(div, 'Divehi').
recognized_language(dgr, 'Dogrib').
recognized_language(dua, 'Duala').
recognized_language(dui, 'Duits').
recognized_language(ndu, 'Duits, Neder').
recognized_language(doh, 'Duits, Oudhoog').
recognized_language(gsw, 'Duits, Zwitsers-').
recognized_language(dyu, 'Dyula').
recognized_language(dzo, 'Dzongkha').
recognized_language(efi, 'Efik').
recognized_language(egy, 'Egyptisch').
recognized_language(eka, 'Ekajuk').
recognized_language(eng, 'Engels').
recognized_language(enm, 'Engels, Middel').
recognized_language(myv, 'Erzja').
recognized_language(esk, 'Eskimotaal').
recognized_language(est, 'Estisch').
recognized_language(eth, 'Ethiopisch').
recognized_language(ewe, 'Ewe').
recognized_language(ewo, 'Ewondo').
recognized_language(far, 'Faeroer').
recognized_language(fan, 'Fang').
recognized_language(fat, 'Fanti').
recognized_language(phn, 'Fenicisch').
recognized_language(fij, 'Fidji').
recognized_language(fil, 'Filipino').
recognized_language(fin, 'Fins').
recognized_language(fon, 'Fon').
recognized_language(fra, 'Frans').
recognized_language(frm, 'Frans, Middel').
recognized_language(fro, 'Frans, Oud').
recognized_language(fur, 'Friulaans').
recognized_language(ful, 'Fula').
recognized_language(gaa, 'Ga').
recognized_language(gae, 'Gaelisch').
recognized_language(gag, 'Galicisch').
recognized_language(gal, 'Galla').
recognized_language(gay, 'Gayo').
recognized_language(gba, 'Gbaya').
recognized_language(gil, 'Gilbertees').
recognized_language(gon, 'Gondi').
recognized_language(gor, 'Gorontalo').
recognized_language(got, 'Gotisch').
recognized_language(grb, 'Grebo').
recognized_language(grn, 'Grieks, modern').
recognized_language(gua, 'Guarani').
recognized_language(guj, 'Gujarati').
recognized_language(gwi, 'Gwich\'in').
recognized_language(hai, 'Haida').
recognized_language(hal, 'Halmaheratalen').
recognized_language(hau, 'Hausa').
recognized_language(haw, 'Hawaïïtaal').
recognized_language(heb, 'Hebreews').
recognized_language(her, 'Herero').
recognized_language(hil, 'Hiligaynon').
recognized_language(hin, 'Hindi').
recognized_language(hmo, 'Hiri Motu').
recognized_language(hmn, 'Hmong').
recognized_language(hup, 'Hupa').
recognized_language(iba, 'Iban').
recognized_language(ibo, 'Ibo').
recognized_language(ido, 'Ido').
recognized_language(mga, 'Iers, Middel (ca. 1100-1500)').
recognized_language(sga, 'Iers, Oud (tot 1100)').
recognized_language(ijs, 'IJslands').
recognized_language(ilo, 'Ilocano').
recognized_language(smn, 'Inari-Samisch').
recognized_language(ica, 'Indianentalen (Centraal-Amerika)').
recognized_language(ina, 'Indianentalen (Noord-Amerika)').
recognized_language(ind, 'Indonesisch').
recognized_language(inh, 'Ingoesjetisch (Ingoesj)').
recognized_language(int, 'Interlingua').
recognized_language(ile, 'Interlingue').
recognized_language(iku, 'Inuktitut').
recognized_language(ipk, 'Inupiaq').
recognized_language(ita, 'Italiaans').
recognized_language(jao, 'Jao').
recognized_language(jav, 'Javaans').
recognized_language(jid, 'Jiddisch').
recognized_language(jor, 'Joruba').
recognized_language(jrb, 'Judeo-Arabisch').
recognized_language(jpr, 'Judeo-Perzisch').
recognized_language(jus, 'Judeo-Spaans').
recognized_language(kbd, 'Kabardisch').
recognized_language(kac, 'Kachin').
recognized_language(kal, 'Kalatdlissut').
recognized_language(xal, 'Kalmuks').
recognized_language(kam, 'Kamba').
recognized_language(kan, 'Kanarees').
recognized_language(kau, 'Kanuri').
recognized_language(kaa, 'Karakalpaks').
recognized_language(krc, 'Karatsjai-Balkarisch').
recognized_language(krl, 'Karelisch').
recognized_language(kas, 'Kasjmiri').
recognized_language(csb, 'Kasjoebisch').
recognized_language(kak, 'Kaukasisch').
recognized_language(kaw, 'Kawi').
recognized_language(kaz, 'Kazaks').
recognized_language(kel, 'Keltische talen (overige)').
recognized_language(slk, 'Kerkslavisch').
recognized_language(kha, 'Khasi').
recognized_language(kho, 'Khotanees').
recognized_language(kik, 'Kikuyu').
recognized_language(kmb, 'Kimbundu').
recognized_language(kin, 'Kinyarwanda').
recognized_language(kir, 'Kirgizisch').
recognized_language(ksu, 'Kleine Sunda Eilanden m.u.v. Balinees').
recognized_language(tlh, 'Klingon').
recognized_language(kur, 'Koerdisch').
recognized_language(koe, 'Koesjitische talen (overige)').
recognized_language(kom, 'Komi').
recognized_language(kon, 'Kongo').
recognized_language(kok, 'Konkani').
recognized_language(kop, 'Koptisch').
recognized_language(kor, 'Koreaans').
recognized_language(kpe, 'Kpelle').
recognized_language(crh, 'Krim-Tataars').
recognized_language(kua, 'Kuanyama').
recognized_language(kum, 'Kumyk').
recognized_language(kru, 'Kurukh').
recognized_language(kus, 'Kusaie').
recognized_language(kut, 'Kutenai').
recognized_language(kwi, 'Kwinti').
recognized_language(lah, 'Lahnda').
recognized_language(lam, 'Lamba').
recognized_language(lan, 'Langue d\'Oc').
recognized_language(lao, 'Laotiaans').
recognized_language(lap, 'Laps').
recognized_language(lat, 'Latijn').
recognized_language(let, 'Lets').
recognized_language(lez, 'Lezgian').
recognized_language(lim, 'Limburgs').
recognized_language(lin, 'Lingala').
recognized_language(lit, 'Litouws').
recognized_language(jbo, 'Lojban').
recognized_language(lol, 'Lolo').
recognized_language(loz, 'Lozi').
recognized_language(lub, 'Luba').
recognized_language(lua, 'Luba-Luala').
recognized_language(lug, 'Luganda').
recognized_language(lui, 'Luisend').
recognized_language(smj, 'Lule-Samisch').
recognized_language(lun, 'Lunda').
recognized_language(luo, 'Luo').
recognized_language(lus, 'Lushai').
recognized_language(ltz, 'Luxemburgs').
recognized_language(mad, 'Malagasi (gesproken op Madagascar)').
recognized_language(mdu, 'Madurees').
recognized_language(mag, 'Magahi').
recognized_language(mai, 'Maithili').
recognized_language(mak, 'Makasar').
recognized_language(mal, 'Maleis').
recognized_language(mlk, 'Malinke').
recognized_language(mlt, 'Maltees').
recognized_language(mnc, 'Manchu').
recognized_language(mdr, 'Mandar').
recognized_language(mni, 'Manipuri').
recognized_language(man, 'Manx').
recognized_language(mar, 'Marathi').
recognized_language(chm, 'Mari').
recognized_language(mah, 'Marshall').
recognized_language(mwr, 'Marwari').
recognized_language(mas, 'Massai').
recognized_language(men, 'Mende').
recognized_language(mic, 'Micmac').
recognized_language(min, 'Minangkabau').
recognized_language(mwl, 'Mirandees').
recognized_language(moh, 'Mohawk').
recognized_language(mdf, 'Moksha').
recognized_language(muk, 'Molukse talen').
recognized_language(mon, 'Mongools').
recognized_language(mos, 'Mossi').
recognized_language(mus, 'Muskogee').
recognized_language(nqo, 'N\'Ko').
recognized_language(nap, 'Napolitaans (Italiaans)').
recognized_language(nau, 'Nauru').
recognized_language(nav, 'Navaho').
recognized_language(nde, 'Ndebele').
recognized_language(nbl, 'Ndebele (Zuid-Afrika)').
recognized_language(ndo, 'Ndonga').
recognized_language(dsb, 'Nedersorbisch').
recognized_language(mnd, 'Nederlands, Middel').
recognized_language(nep, 'Nepalees').
recognized_language(new, 'Newari').
recognized_language(nwc, 'Newari, Oud').
recognized_language(nia, 'Nias').
recognized_language(niu, 'Niuean').
recognized_language(nog, 'Nogais').
recognized_language(frr, 'Noord-Fries').
recognized_language(nso, 'Noord-Sotho').
recognized_language(nob, 'Noors, Bokmål').
recognized_language(nno, 'Noors, Nynorsk').
recognized_language(non, 'Noors, Oud').
recognized_language(nym, 'Nyamwesi').
recognized_language(nya, 'Nyanja, Chewa').
recognized_language(nyn, 'Nyankole').
recognized_language(nyo, 'Nyoro').
recognized_language(nzi, 'Nzima').
recognized_language(oji, 'Ojibwa').
recognized_language(frs, 'Oost-Fries').
recognized_language(hsb, 'Oppersorbisch').
recognized_language(ori, 'Oriya').
recognized_language(osa, 'Osage').
recognized_language(ota, 'Osmaans-Turks').
recognized_language(oss, 'Ossetisch').
recognized_language(pac, 'Pacifictalen').
recognized_language(pah, 'Pahari').
recognized_language(pal, 'Pahlavi').
recognized_language(pau, 'Palauan').
recognized_language(pli, 'Pali').
recognized_language(pam, 'Pampanga').
recognized_language(pag, 'Pangasinan').
recognized_language(pan, 'Panjabi').
recognized_language(pap, 'Papiamento').
recognized_language(ppa, 'Papoea').
recognized_language(pas, 'Pashto').
recognized_language(peo, 'Perzisch, Oud').
recognized_language(pon, 'Ponape').
recognized_language(poo, 'Pools').
recognized_language(por, 'Portugees').
recognized_language(pro, 'Provencaals').
recognized_language(que, 'Quechua').
recognized_language(raj, 'Rajasthani').
recognized_language(rap, 'Rapanui').
recognized_language(rar, 'Rarotongan').
recognized_language(roe, 'Roemeens').
recognized_language(run, 'Rundi').
recognized_language(rus, 'Russisch').
recognized_language(sam, 'Samaritaans').
recognized_language(sme, 'Sami, noord').
recognized_language(smn, 'Samisch, Inari-').
recognized_language(smj, 'Samisch, Lule-').
recognized_language(sms, 'Samisch, Skolt-').
recognized_language(sma, 'Samisch, Zuid-').
recognized_language(sao, 'Samoaans').
recognized_language(sad, 'Sandawe').
recognized_language(sag, 'Sango').
recognized_language(san, 'Sanskrit').
recognized_language(sat, 'Santali').
recognized_language(sac, 'Saramaccaans').
recognized_language(srd, 'Sardisch').
recognized_language(sar, 'Sarnami Hindustani').
recognized_language(sas, 'Sasak').
recognized_language(sco, 'Schots (Lallans)').
recognized_language(sel, 'Selkup').
recognized_language(srr, 'Serer').
recognized_language(ser, 'Servisch').
recognized_language(shn, 'Shan').
recognized_language(sho, 'Shona').
recognized_language(iii, 'Sichuan Yi').
recognized_language(sid, 'Sidamo').
recognized_language(snd, 'Sindhi').
recognized_language(snh, 'Singalees').
recognized_language(sms, 'Skolt-Samisch').
recognized_language(den, 'Slave').
recognized_language(slw, 'Sloveens').
recognized_language(soe, 'Soemerisch').
recognized_language(sog, 'Sogdisch').
recognized_language(som, 'Somalisch').
recognized_language(snk, 'Soninke').
recognized_language(sor, 'Sorbisch').
recognized_language(spa, 'Spaans').
recognized_language(sra, 'Sranan').
recognized_language(suk, 'Sukuma').
recognized_language(sun, 'Sundanees').
recognized_language(suj, 'Surinaams-Javaans').
recognized_language(sne, 'Surinaams-Nederlands').
recognized_language(sus, 'Susu').
recognized_language(swa, 'Swahili').
recognized_language(syr, 'Syrisch').
recognized_language(syc, 'Syrisch, Oud').
recognized_language(tad, 'Tadjik').
recognized_language(tag, 'Tagalog').
recognized_language(tah, 'Tahitisch').
recognized_language(tmh, 'Tamashek').
recognized_language(tam, 'Tamil').
recognized_language(tat, 'Tataars').
recognized_language(tel, 'Telugu').
recognized_language(tem, 'Temne').
recognized_language(ter, 'Tereno').
recognized_language(tet, 'Tetum').
recognized_language(tha, 'Thai').
recognized_language(tig, 'Tigre').
recognized_language(tir, 'Tigrina').
recognized_language(tiv, 'Tiv').
recognized_language(tli, 'Tlingit').
recognized_language(tpi, 'Tok Pisin').
recognized_language(tkl, 'Tokelau').
recognized_language(ton, 'Tonga').
recognized_language(tog, 'Tonga (Nyassa)').
recognized_language(tri, 'Trio').
recognized_language(tru, 'Truk').
recognized_language(tsi, 'Tsimshian').
recognized_language(tse, 'Tsjechisch').
recognized_language(che, 'Tsjetsjeens').
recognized_language(tsj, 'Tsjoevasjisch').
recognized_language(tso, 'Tsonga').
recognized_language(tsw, 'Tswana').
recognized_language(tum, 'Tumbuka').
recognized_language(tuk, 'Turkmeens').
recognized_language(tur, 'Turks').
recognized_language(tvl, 'Tuvalu').
recognized_language(tyv, 'Tuviniaans').
recognized_language(twi, 'Twi').
recognized_language(udm, 'Udmurts').
recognized_language(uga, 'Ugaritisch').
recognized_language(umb, 'Umbundu').
recognized_language(urd, 'Urdu').
recognized_language(vai, 'Vai').
recognized_language(ven, 'Venda').
recognized_language(vie, 'Vietnamees').
recognized_language(vol, 'Volapük').
recognized_language(wln, 'Waals').
recognized_language(wal, 'Walamo').
recognized_language(wap, 'Waphisana').
recognized_language(war, 'Waray').
recognized_language(was, 'Washo').
recognized_language(way, 'Wayana').
recognized_language(wit, 'Wit-Russisch').
recognized_language(wol, 'Wolof').
recognized_language(xho, 'Xhosa').
recognized_language(sah, 'Yakut').
recognized_language(yar, 'Yar').
recognized_language(zap, 'Zapotec').
recognized_language(zza, 'Zazaki').
recognized_language(zen, 'Zenega').
recognized_language(zha, 'Zhuang').
recognized_language(alt, 'Zuid-Altajs').
recognized_language(sma, 'Zuid-Samisch').
recognized_language(sso, 'Zuid-Sotho').
recognized_language(zul, 'Zulu').
recognized_language(zun, 'Zuni').
recognized_language(gsw, 'Zwitsers-Duits').
recognized_language(onb, 'Onbepaald/onbekend').

%! same_language(?Language1:atom, ?Language1:atom) is nondet.
% Language names that are considered to denotate the same language
% according to OCLC standards.
%
% @see Taken from
%      http://support.oclc.org/ggc/richtlijnen/?id=12&ln=nl&sec=k-1500

same_language('Afgaans', 'Pashto').
same_language('Annamees', 'Vietnamees').
same_language('Atjehs', 'Acehs').
same_language('Belorussisch', 'Wit-Russisch').
same_language('Boni', 'Aluku').
same_language('Chechen', 'Tsjetsjeens').
same_language('Chewa', 'Nyanja').
same_language('Foenicisch', 'Fenicisch').
same_language('Iriantalen', 'Papoea').
same_language('Kalimantantalen', 'Borneotalen').
same_language('Ladino', 'Judeo-Spaans').
same_language('Mandingo', 'Malinke').
same_language('Mbundu', 'Umbundu').
same_language('Middelengels', 'Engels, Middel').
same_language('Middelfrans', 'Frans, Middel').
same_language('Middelhoogduits', 'Duits, Middelhoog').
same_language('Middel-iers', 'Iers, Middel-').
same_language('Middelnederlands', 'Nederlands, Middel').
same_language('Nederduits', 'Duits, Neder').
same_language('Nieuwgrieks', 'Grieks, modern').
same_language('Nusa Tenggara', 'Kleine Sunda Eilanden').
same_language('Occidental', 'Interlingua').
same_language('Occitaans', 'Langue d\'Oc').
same_language('Oudfrans', 'Frans, Oud').
same_language('Oudhoogduits', 'Duits, Oudhoog').
same_language('Oudiers', 'Iers, Oud').
same_language('Oudnoors', 'Noors, Oud').
same_language('Oudperzisch', 'Perzisch, Oud').
same_language('Oudsyrisch', 'Syrisch, Oud').
same_language('Phoenicisch', 'Fenicisch').
same_language('Pushto', 'Pashto').
same_language('Romani', 'Zigeunertalen').
same_language('Samisch', 'Laps').
same_language('Sulawesitalen', 'Celebestalen').
same_language('Turks, Ottomaans-', 'Ottomaans-Turks').
same_language('Unuit', 'Eskimotaal').
same_language('Vlaams', 'Nederlands').
same_language('Wendisch', 'Sorbisch').
same_language('Yoruba', 'Joruba').

%! unrecognized_language(?Code:atom, ?Name:atom) is nondet.
% Taken from http://support.oclc.org/ggc/richtlijnen/?id=12&ln=nl&sec=k-1500
% Per 2013/01 this site contains a table with 507 unique language codes,
% of which 98 do not occur in ISO 639-3.
% Language codes not in ISO 639-3.
%
% Language codes that have been translated or recognized by my code are
% not included in this list.
%
% Language codes that are actually language family codes are not included
% in this list either.

unrecognized_language(aco, 'Acoli').
unrecognized_language(afa, 'Afro-aziatische talen (overige)').
unrecognized_language(alb, 'Albanees').
unrecognized_language(alg, 'Algonkium').
unrecognized_language(ajm, 'Aljamia').
unrecognized_language(apa, 'Apache').
unrecognized_language(ath, 'Athapaskische talen').
unrecognized_language(aus, 'Australische talen').
unrecognized_language(bat, 'Baltische talen (overige)').
unrecognized_language(bai, 'Bamileketalen').
unrecognized_language(bad, 'Banda').
unrecognized_language(bnt, 'Bantoetalen (overige)').
unrecognized_language(btk, 'Bataktalen').
unrecognized_language(ber, 'Berbertalen').
unrecognized_language(bih, 'Bihari').
unrecognized_language(bur, 'Burmees').
unrecognized_language(cel, 'Celebestalen, Sulawesitalen').
unrecognized_language(cmc, 'Chamictalen').
unrecognized_language(cpf, 'Creools en Pidgin Frans (overige)').
unrecognized_language(cpp, 'Creools en Pidgin Portugees (overige)').
unrecognized_language(cru, 'Cru').
unrecognized_language(day, 'Dayak').
unrecognized_language(dog, 'Dogri').
unrecognized_language(dra, 'Dravidische talen (overige)').
unrecognized_language(ela, 'Elamitisch').
unrecognized_language(esp, 'Esperanto').
unrecognized_language(phi, 'Filippijnse talen (overige)').
unrecognized_language(fio, 'Fins-Oegrische talen').
unrecognized_language(sgn, 'Gebarentaal').
unrecognized_language(geo, 'Georgisch').
unrecognized_language(ger, 'Germaanse talen (overige)').
unrecognized_language(him, 'Himachali').
unrecognized_language(ier, 'Iers').
unrecognized_language(ijo, 'Ijo').
unrecognized_language(iza, 'Indianentalen (Zuid-Amerika)').
unrecognized_language(inc, 'Indische talen (overige)').
unrecognized_language(ine, 'Indo-Europese talen (overige)').
unrecognized_language(ira, 'Iraans (overige)').
unrecognized_language(kar, 'Karen').
unrecognized_language(khi, 'Khoisan talen (overige)').
unrecognized_language(kro, 'Kroatisch').
unrecognized_language(art, 'Kunstmatige talen (overige)').
unrecognized_language(mac, 'Macedonisch').
unrecognized_language(mly, 'Malayalam').
unrecognized_language(map, 'Maleis-Polynesische talen (overige)').
unrecognized_language(mno, 'Manobo').
unrecognized_language(mao, 'Maori').
unrecognized_language(mol, 'Moldavisch').
unrecognized_language(mkh, 'Mon-Khmertalen (m.u.v. Cambodjaans)').
unrecognized_language(mun, 'Munda talen').
unrecognized_language(myn, 'Mayan').
unrecognized_language(nah, 'Nahuatl').
unrecognized_language(nic, 'Niger-Kongo talen (overige)').
unrecognized_language(noo, 'Noors').
unrecognized_language(nub, 'Nuba').
unrecognized_language(oei, 'Oeigoers').
unrecognized_language(oek, 'Oekraïens').
unrecognized_language(oes, 'Oesbeeks').
unrecognized_language(oto, 'Otomi').
unrecognized_language(rho, 'Rhaeto-Romaans').
unrecognized_language(roa, 'Romaanse talen (overige)').
unrecognized_language(sal, 'Salishan talen').
unrecognized_language(sem, 'Semitische talen (overige)').
unrecognized_language(scc, 'Servo-Kroatisch (Cyrillisch)').
unrecognized_language(scr, 'Servo-Kroatisch (Latijns)').
unrecognized_language(sit, 'Sino-Tibetaanse talen (overige)').
unrecognized_language(sio, 'Siouan talen').
unrecognized_language(sla, 'Slavische talen (overige)').
unrecognized_language(slo, 'Slovaaks').
unrecognized_language(son, 'Songhai').
unrecognized_language(ssa, 'Sub-Sahara Afrikaanse talen (overige)').
unrecognized_language(sum, 'Sumatraanse talen m.u.v. Acehs, Bataks, Minangkabau').
unrecognized_language(swz, 'Swazi').
unrecognized_language(tai, 'Tai talen (overige)').
unrecognized_language(tib, 'Tibetaans').
unrecognized_language(tup, 'Tupi talen').
unrecognized_language(tut, 'Turks-Tataarse talen (overige)').
unrecognized_language(wak, 'Wakashan talen').
unrecognized_language(wel, 'Welsh').
unrecognized_language(wot, 'Wotisch').
unrecognized_language(ypk, 'Yupik').
unrecognized_language(znd, 'Zande').
unrecognized_language(zig, 'Zigeunertalen').
unrecognized_language(zxx, 'Geen taalkundige inhoud').

