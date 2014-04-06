:- module(
  'iso639-5',
  [
    'assert_iso639-5_schema'/1, % +Graph:atom
    'iso639-5'//2 % ?Name:atom
                  % ?URI:atom
  ]
).

/** <module> ISO 639-5

The ISO 639-5 standard for language families and groupos.

The codes are mapped to Lexvo Semantic Web URIs.

@author Wouter Beek
@see http://www.loc.gov/standards/iso639-5/
@version 2013/01, 2013/06-2013/07, 2013/09
*/

:- use_module(library(semweb/rdf_db)).
:- use_module(rdf(rdf_build)).
:- use_module(rdfs(rdfs_build)).
:- use_module(rdfs(rdfs_label_ext)).
:- use_module(xml(xml_namespace)).

:- xml_register_namespace('iso639-5', 'http://lexvo.org/id/iso639-5/').

:- rdf_meta('iso639-5'(?,r,?,?)).



'assert_iso639-5_schema'(G):-
  rdfs_assert_class('iso639-5':'LanguageFamily', G),
  rdfs_assert_label('iso639-5':'LanguageFamily', en,
    'ISO 639-5 language family', G),
  rdfs_assert_seeAlso('iso639-5':'LanguageFamily',
    'http://www.loc.gov/standards/iso639-5/', G),
  forall(
    'iso639-5'(Name, Lang1, _, _),
    (
      rdf_global_id(Lang1, Lang2),
      rdf_assert_individual(Lang2, 'iso639-5':'LanguageFamily', G),
      rdfs_assert_label(Lang2, en, Name, G)
    )
  ).

'iso639-5'('Afro-Asiatic', 'iso639-5':afa) --> "afa".
'iso639-5'('Algonquian', 'iso639-5':alg) --> "alg".
'iso639-5'('Atlantic-Congo', 'iso639-5':alv) --> "alv".
'iso639-5'('Apache', 'iso639-5':apa) --> "apa".
'iso639-5'('Alacalufan', 'iso639-5':aqa) --> "aqa".
'iso639-5'('Algic', 'iso639-5':aql) --> "aql".
'iso639-5'('Artificial', 'iso639-5':art) --> "art".
'iso639-5'('Athapascan', 'iso639-5':ath) --> "ath".
'iso639-5'('Arauan', 'iso639-5':auf) --> "auf".
'iso639-5'('Australian', 'iso639-5':aus) --> "aus".
'iso639-5'('Arawakan', 'iso639-5':awd) --> "awd".
'iso639-5'('Uto-Aztecan', 'iso639-5':azc) --> "azc".
'iso639-5'('Banda', 'iso639-5':bad) --> "bad".
'iso639-5'('Bamileke', 'iso639-5':bai) --> "bai".
'iso639-5'('Baltic', 'iso639-5':bat) --> "bat".
'iso639-5'('Berber', 'iso639-5':ber) --> "ber".
'iso639-5'('Bantu', 'iso639-5':bnt) --> "bnt".
'iso639-5'('Batak', 'iso639-5':btk) --> "btk".
'iso639-5'('Central American Indian', 'iso639-5':cai) --> "cai".
'iso639-5'('Caucasian', 'iso639-5':cau) --> "cau".
'iso639-5'('Chibchan', 'iso639-5':cba) --> "cba".
'iso639-5'('North Caucasian', 'iso639-5':ccn) --> "ccn".
'iso639-5'('South Caucasian', 'iso639-5':ccs) --> "ccs".
'iso639-5'('Chadic', 'iso639-5':cdc) --> "cdc".
'iso639-5'('Caddoan', 'iso639-5':cdd) --> "cdd".
'iso639-5'('Celtic', 'iso639-5':cel) --> "cel".
'iso639-5'('Chamic', 'iso639-5':cmc) --> "cmc".
'iso639-5'('Creoles and pidgins, English‑based', 'iso639-5':cpe) --> "cpe".
'iso639-5'('Creoles and pidgins, French‑based', 'iso639-5':cpf) --> "cpf".
'iso639-5'('Creoles and pidgins, Portuguese-based', 'iso639-5':cpp) --> "cpp".
'iso639-5'('Creoles and pidgins', 'iso639-5':crp) --> "crp".
'iso639-5'('Central Sudanic', 'iso639-5':csu) --> "csu".
'iso639-5'('Cushitic', 'iso639-5':cus) --> "cus".
'iso639-5'('Land Dayak', 'iso639-5':day) --> "day".
'iso639-5'('Mande', 'iso639-5':dmn) --> "dmn".
'iso639-5'('Dravidian', 'iso639-5':dra) --> "dra".
'iso639-5'('Egyptian', 'iso639-5':egx) --> "egx".
'iso639-5'('Eskimo-Aleut', 'iso639-5':esx) --> "esx".
'iso639-5'('Basque', 'iso639-5':euq) --> "euq".
'iso639-5'('Finno-Ugrian', 'iso639-5':fiu) --> "fiu".
'iso639-5'('Formosan', 'iso639-5':fox) --> "fox".
'iso639-5'('Germanic', 'iso639-5':gem) --> "gem".
'iso639-5'('East Germanic', 'iso639-5':gme) --> "gme".
'iso639-5'('North Germanic', 'iso639-5':gmq) --> "gmq".
'iso639-5'('West Germanic', 'iso639-5':gmw) --> "gmw".
'iso639-5'('Greek', 'iso639-5':grk) --> "grk".
'iso639-5'('Hmong-Mien', 'iso639-5':hmx) --> "hmx".
'iso639-5'('Hokan languages', 'iso639-5':hok) --> "hok".
'iso639-5'('Armenian', 'iso639-5':hyx) --> "hyx".
'iso639-5'('Indo-Iranian', 'iso639-5':iir) --> "iir".
'iso639-5'('Ijo', 'iso639-5':ijo) --> "ijo".
'iso639-5'('Indic', 'iso639-5':inc) --> "inc".
'iso639-5'('Indo-European', 'iso639-5':ine) --> "ine".
'iso639-5'('Iranian', 'iso639-5':ira) --> "ira".
'iso639-5'('Iroquoian', 'iso639-5':iro) --> "iro".
'iso639-5'('Italic', 'iso639-5':itc) --> "itc".
'iso639-5'('Japanese', 'iso639-5':jpx) --> "jpx".
'iso639-5'('Karen', 'iso639-5':kar) --> "kar".
'iso639-5'('Kordofanian', 'iso639-5':kdo) --> "kdo".
'iso639-5'('Khoisan', 'iso639-5':khi) --> "khi".
'iso639-5'('Kru', 'iso639-5':kro) --> "kro".
'iso639-5'('Austronesian', 'iso639-5':map) --> "map".
'iso639-5'('Mon-Khmer', 'iso639-5':mkh) --> "mkh".
'iso639-5'('Manobo', 'iso639-5':mno) --> "mno".
'iso639-5'('Munda', 'iso639-5':mun) --> "mun".
'iso639-5'('Mayan', 'iso639-5':myn) --> "myn".
'iso639-5'('Nahuatl', 'iso639-5':nah) --> "nah".
'iso639-5'('North American Indian', 'iso639-5':nai) --> "nai".
'iso639-5'('Trans-New Guinea', 'iso639-5':ngf) --> "ngf".
'iso639-5'('Niger-Kordofanian', 'iso639-5':nic) --> "nic".
'iso639-5'('Nubian', 'iso639-5':nub) --> "nub".
'iso639-5'('Oto-Manguean', 'iso639-5':omq) --> "omq".
'iso639-5'('Omotic', 'iso639-5':omv) --> "omv".
'iso639-5'('Otomian', 'iso639-5':oto) --> "oto".
'iso639-5'('Papuan', 'iso639-5':paa) --> "paa".
'iso639-5'('Philippine', 'iso639-5':phi) --> "phi".
'iso639-5'('Central Malayo-Polynesian', 'iso639-5':plf) --> "plf".
'iso639-5'('Malayo-Polynesian', 'iso639-5':poz) --> "poz".
'iso639-5'('Eastern Malayo-Polynesian', 'iso639-5':pqe) --> "pqe".
'iso639-5'('Western Malayo-Polynesian', 'iso639-5':pqw) --> "pqw".
'iso639-5'('Prakrit', 'iso639-5':pra) --> "pra".
'iso639-5'('Quechuan', 'iso639-5':qwe) --> "qwe".
'iso639-5'('Romance', 'iso639-5':roa) --> "roa".
'iso639-5'('South American Indian', 'iso639-5':sai) --> "sai".
'iso639-5'('Salishan', 'iso639-5':sal) --> "sal".
'iso639-5'('Eastern Sudanic', 'iso639-5':sdv) --> "sdv".
'iso639-5'('Semitic', 'iso639-5':sem) --> "sem".
'iso639-5'('sign', 'iso639-5':sgn) --> "sgn".
'iso639-5'('Siouan', 'iso639-5':sio) --> "sio".
'iso639-5'('Sino-Tibetan', 'iso639-5':sit) --> "sit".
'iso639-5'('Slavic', 'iso639-5':sla) --> "sla".
'iso639-5'('Sami', 'iso639-5':smi) --> "smi".
'iso639-5'('Songhai', 'iso639-5':son) --> "son".
'iso639-5'('Albanian', 'iso639-5':sqj) --> "sqj".
'iso639-5'('Nilo-Saharan', 'iso639-5':ssa) --> "ssa".
'iso639-5'('Samoyedic', 'iso639-5':syd) --> "syd".
'iso639-5'('Tai', 'iso639-5':tai) --> "tai".
'iso639-5'('Tibeto-Burman', 'iso639-5':tbq) --> "tbq".
'iso639-5'('Turkic', 'iso639-5':trk) --> "trk".
'iso639-5'('Tupi', 'iso639-5':tup) --> "tup".
'iso639-5'('Altaic', 'iso639-5':tut) --> "tut".
'iso639-5'('Tungus', 'iso639-5':tuw) --> "tuw".
'iso639-5'('Uralic', 'iso639-5':urj) --> "urj".
'iso639-5'('Wakashan', 'iso639-5':wak) --> "wak".
'iso639-5'('Sorbian', 'iso639-5':wen) --> "wen".
'iso639-5'('Mongolian', 'iso639-5':xgn) --> "xgn".
'iso639-5'('Na-Dene', 'iso639-5':xnd) --> "xnd".
'iso639-5'('Yupik', 'iso639-5':ypk) --> "ypk".
'iso639-5'('Chinese', 'iso639-5':zhx) --> "zhx".
'iso639-5'('East Slavic', 'iso639-5':zle) --> "zle".
'iso639-5'('South Slavic', 'iso639-5':zls) --> "zls".
'iso639-5'('West Slavic', 'iso639-5':zlw) --> "zlw".
'iso639-5'('Zande', 'iso639-5':znd) --> "znd".

