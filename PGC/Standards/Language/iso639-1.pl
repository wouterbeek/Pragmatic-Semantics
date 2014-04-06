:- module(
  'iso639-1',
  [
    'iso639-1'//1 % ?URI:atom
  ]
).

/** <module> ISO 639-1

The ISO 639-1 standard for language codes with Lexvo Semantic Web URIs.

# Table of supported languages

!!'iso639-1'!!language!!2!!

# RE

The following RE notation was used to replace this representation to
the DCG representation:
  * From: `'iso639-1'\(([a-z]+), ([^)]+)\).`
  * To:   `'iso639-1'\(\2\) --> "\1".`

@author Wouter Beek
@see http://www.sil.org/iso639-1/
@version 2013/01, 2013/06-2013/07
*/

:- use_module(library(semweb/rdf_db)). % For rdf_meta/1.
:- use_module(xml(xml_namespace)).

:- xml_register_namespace('iso639-3', 'http://lexvo.org/id/iso639-3/').

:- rdf_meta('iso639-1'(r,?,?)).



%! 'iso639-1'(?Lexvo:uri)//

'iso639-1'('iso639-3':aar) --> "aa".
'iso639-1'('iso639-3':abk) --> "ab".
'iso639-1'('iso639-3':ave) --> "ae".
'iso639-1'('iso639-3':afr) --> "af".
'iso639-1'('iso639-3':aka) --> "ak".
'iso639-1'('iso639-3':amh) --> "am".
'iso639-1'('iso639-3':arg) --> "an".
'iso639-1'('iso639-3':ara) --> "ar".
'iso639-1'('iso639-3':asm) --> "as".
'iso639-1'('iso639-3':ava) --> "av".
'iso639-1'('iso639-3':aym) --> "ay".
'iso639-1'('iso639-3':aze) --> "az".
'iso639-1'('iso639-3':bak) --> "ba".
'iso639-1'('iso639-3':bel) --> "be".
'iso639-1'('iso639-3':bul) --> "bg".
'iso639-1'('iso639-3':bis) --> "bi".
'iso639-1'('iso639-3':bam) --> "bm".
'iso639-1'('iso639-3':ben) --> "bn".
'iso639-1'('iso639-3':bod) --> "bo".
'iso639-1'('iso639-3':bre) --> "br".
'iso639-1'('iso639-3':bos) --> "bs".
'iso639-1'('iso639-3':cat) --> "ca".
'iso639-1'('iso639-3':che) --> "ce".
'iso639-1'('iso639-3':cha) --> "ch".
'iso639-1'('iso639-3':cos) --> "co".
'iso639-1'('iso639-3':cre) --> "cr".
'iso639-1'('iso639-3':ces) --> "cs".
'iso639-1'('iso639-3':chu) --> "cu".
'iso639-1'('iso639-3':chv) --> "cv".
'iso639-1'('iso639-3':cym) --> "cy".
'iso639-1'('iso639-3':dan) --> "da".
'iso639-1'('iso639-3':deu) --> "de".
'iso639-1'('iso639-3':div) --> "dv".
'iso639-1'('iso639-3':dzo) --> "dz".
'iso639-1'('iso639-3':ewe) --> "ee".
'iso639-1'('iso639-3':ell) --> "el".
'iso639-1'('iso639-3':eng) --> "en".
'iso639-1'('iso639-3':epo) --> "eo".
'iso639-1'('iso639-3':spa) --> "es".
'iso639-1'('iso639-3':est) --> "et".
'iso639-1'('iso639-3':eus) --> "eu".
'iso639-1'('iso639-3':fas) --> "fa".
'iso639-1'('iso639-3':ful) --> "ff".
'iso639-1'('iso639-3':fin) --> "fi".
'iso639-1'('iso639-3':fij) --> "fj".
'iso639-1'('iso639-3':fao) --> "fo".
'iso639-1'('iso639-3':fra) --> "fr".
'iso639-1'('iso639-3':fry) --> "fy".
'iso639-1'('iso639-3':gle) --> "ga".
'iso639-1'('iso639-3':gla) --> "gd".
'iso639-1'('iso639-3':glg) --> "gl".
'iso639-1'('iso639-3':grn) --> "gn".
'iso639-1'('iso639-3':guj) --> "gu".
'iso639-1'('iso639-3':glv) --> "gv".
'iso639-1'('iso639-3':hau) --> "ha".
'iso639-1'('iso639-3':heb) --> "he".
'iso639-1'('iso639-3':hin) --> "hi".
'iso639-1'('iso639-3':hmo) --> "ho".
'iso639-1'('iso639-3':hrv) --> "hr".
'iso639-1'('iso639-3':hat) --> "ht".
'iso639-1'('iso639-3':hun) --> "hu".
'iso639-1'('iso639-3':hye) --> "hy".
'iso639-1'('iso639-3':her) --> "hz".
'iso639-1'('iso639-3':ina) --> "ia".
'iso639-1'('iso639-3':ind) --> "id".
'iso639-1'('iso639-3':ile) --> "ie".
'iso639-1'('iso639-3':ibo) --> "ig".
'iso639-1'('iso639-3':iii) --> "ii".
'iso639-1'('iso639-3':ipk) --> "ik".
'iso639-1'('iso639-3':ido) --> "io".
'iso639-1'('iso639-3':isl) --> "is".
'iso639-1'('iso639-3':ita) --> "it".
'iso639-1'('iso639-3':iku) --> "iu".
'iso639-1'('iso639-3':jpn) --> "ja".
'iso639-1'('iso639-3':jav) --> "jv".
'iso639-1'('iso639-3':kat) --> "ka".
'iso639-1'('iso639-3':kon) --> "kg".
'iso639-1'('iso639-3':kik) --> "ki".
'iso639-1'('iso639-3':kua) --> "kj".
'iso639-1'('iso639-3':kaz) --> "kk".
'iso639-1'('iso639-3':kal) --> "kl".
'iso639-1'('iso639-3':khm) --> "km".
'iso639-1'('iso639-3':kan) --> "kn".
'iso639-1'('iso639-3':kor) --> "ko".
'iso639-1'('iso639-3':kau) --> "kr".
'iso639-1'('iso639-3':kas) --> "ks".
'iso639-1'('iso639-3':kur) --> "ku".
'iso639-1'('iso639-3':kom) --> "kv".
'iso639-1'('iso639-3':cor) --> "kw".
'iso639-1'('iso639-3':kir) --> "ky".
'iso639-1'('iso639-3':lat) --> "la".
'iso639-1'('iso639-3':ltz) --> "lb".
'iso639-1'('iso639-3':lug) --> "lg".
'iso639-1'('iso639-3':lim) --> "li".
'iso639-1'('iso639-3':lin) --> "ln".
'iso639-1'('iso639-3':lao) --> "lo".
'iso639-1'('iso639-3':lit) --> "lt".
'iso639-1'('iso639-3':lub) --> "lu".
'iso639-1'('iso639-3':lav) --> "lv".
'iso639-1'('iso639-3':mlg) --> "mg".
'iso639-1'('iso639-3':mah) --> "mh".
'iso639-1'('iso639-3':mri) --> "mi".
'iso639-1'('iso639-3':mkd) --> "mk".
'iso639-1'('iso639-3':mal) --> "ml".
'iso639-1'('iso639-3':mon) --> "mn".
'iso639-1'('iso639-3':mar) --> "mr".
'iso639-1'('iso639-3':msa) --> "ms".
'iso639-1'('iso639-3':mlt) --> "mt".
'iso639-1'('iso639-3':mya) --> "my".
'iso639-1'('iso639-3':nau) --> "na".
'iso639-1'('iso639-3':nob) --> "nb".
'iso639-1'('iso639-3':nde) --> "nd".
'iso639-1'('iso639-3':nep) --> "ne".
'iso639-1'('iso639-3':ndo) --> "ng".
'iso639-1'('iso639-3':nld) --> "nl".
'iso639-1'('iso639-3':nno) --> "nn".
'iso639-1'('iso639-3':nor) --> "no".
'iso639-1'('iso639-3':nbl) --> "nr".
'iso639-1'('iso639-3':nav) --> "nv".
'iso639-1'('iso639-3':nya) --> "ny".
'iso639-1'('iso639-3':oci) --> "oc".
'iso639-1'('iso639-3':oji) --> "oj".
'iso639-1'('iso639-3':orm) --> "om".
'iso639-1'('iso639-3':ori) --> "or".
'iso639-1'('iso639-3':oss) --> "os".
'iso639-1'('iso639-3':pan) --> "pa".
'iso639-1'('iso639-3':pli) --> "pi".
'iso639-1'('iso639-3':pol) --> "pl".
'iso639-1'('iso639-3':pus) --> "ps".
'iso639-1'('iso639-3':por) --> "pt".
'iso639-1'('iso639-3':que) --> "qu".
'iso639-1'('iso639-3':roh) --> "rm".
'iso639-1'('iso639-3':run) --> "rn".
'iso639-1'('iso639-3':ron) --> "ro".
'iso639-1'('iso639-3':rus) --> "ru".
'iso639-1'('iso639-3':kin) --> "rw".
'iso639-1'('iso639-3':san) --> "sa".
'iso639-1'('iso639-3':srd) --> "sc".
'iso639-1'('iso639-3':snd) --> "sd".
'iso639-1'('iso639-3':sme) --> "se".
'iso639-1'('iso639-3':sag) --> "sg".
'iso639-1'('iso639-3':hbs) --> "sh".
'iso639-1'('iso639-3':sin) --> "si".
'iso639-1'('iso639-3':slk) --> "sk".
'iso639-1'('iso639-3':slv) --> "sl".
'iso639-1'('iso639-3':smo) --> "sm".
'iso639-1'('iso639-3':sna) --> "sn".
'iso639-1'('iso639-3':som) --> "so".
'iso639-1'('iso639-3':sqi) --> "sq".
'iso639-1'('iso639-3':srp) --> "sr".
'iso639-1'('iso639-3':ssw) --> "ss".
'iso639-1'('iso639-3':sot) --> "st".
'iso639-1'('iso639-3':sun) --> "su".
'iso639-1'('iso639-3':swe) --> "sv".
'iso639-1'('iso639-3':swa) --> "sw".
'iso639-1'('iso639-3':tam) --> "ta".
'iso639-1'('iso639-3':tel) --> "te".
'iso639-1'('iso639-3':tgk) --> "tg".
'iso639-1'('iso639-3':tha) --> "th".
'iso639-1'('iso639-3':tir) --> "ti".
'iso639-1'('iso639-3':tuk) --> "tk".
'iso639-1'('iso639-3':tgl) --> "tl".
'iso639-1'('iso639-3':tsn) --> "tn".
'iso639-1'('iso639-3':ton) --> "to".
'iso639-1'('iso639-3':tur) --> "tr".
'iso639-1'('iso639-3':tso) --> "ts".
'iso639-1'('iso639-3':tat) --> "tt".
'iso639-1'('iso639-3':twi) --> "tw".
'iso639-1'('iso639-3':tah) --> "ty".
'iso639-1'('iso639-3':uig) --> "ug".
'iso639-1'('iso639-3':ukr) --> "uk".
'iso639-1'('iso639-3':urd) --> "ur".
'iso639-1'('iso639-3':uzb) --> "uz".
'iso639-1'('iso639-3':ven) --> "ve".
'iso639-1'('iso639-3':vie) --> "vi".
'iso639-1'('iso639-3':vol) --> "vo".
'iso639-1'('iso639-3':wln) --> "wa".
'iso639-1'('iso639-3':wol) --> "wo".
'iso639-1'('iso639-3':xho) --> "xh".
'iso639-1'('iso639-3':yid) --> "yi".
'iso639-1'('iso639-3':yor) --> "yo".
'iso639-1'('iso639-3':zha) --> "za".
'iso639-1'('iso639-3':zho) --> "zh".
'iso639-1'('iso639-3':zul) --> "zu".
