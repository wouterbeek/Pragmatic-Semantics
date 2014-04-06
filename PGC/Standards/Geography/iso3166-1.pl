:- module(
  'iso3166-1',
  [
    'assert_iso3166-1_schema'/1, % 
    'iso3166-1'//2 % ?Name:atom
                   % ?Country:iri
  ]
).

/** <module> ISO 3166-1

Suopport for the ISO 3166-1 country code standard.

@author Wouter Beek
@version 2013/01, 2013/06
*/

:- use_module(library(semweb/rdf_db)). % rdf_meta/1
:- use_module(rdf(rdf_build)).
:- use_module(rdfs(rdfs_build)).
:- use_module(rdfs(rdfs_label_ext)).
:- use_module(xml(xml_namespace)).

:- xml_register_namespace('iso3166-1', 'http://lexvo.org/id/iso3166/').

:- rdf_meta('iso3166-1'(?,?,r)).



'assert_iso3166-1_schema'(G):-
  rdfs_assert_class('iso3166-1':'Country', G),
  rdfs_assert_label('iso3166-1':'Country', en, 'ISO 3166-1 country', G),
  forall(
    'iso3166-1'(Name, Country1, _, _),
    (
      rdf_global_id(Country1, Country2),
      rdf_assert_individual(Country2, 'iso3166-1':'Country', G),
      rdfs_assert_label(Country2, en, Name, G)
    )
  ).

'iso3166-1'('Andorra', 'iso3166-1':'AD') --> "ad".
'iso3166-1'('United arab emirates', 'iso3166-1':'AE') --> "ae".
'iso3166-1'('Afghanistan', 'iso3166-1':'AF') --> "af".
'iso3166-1'('Antigua and barbuda', 'iso3166-1':'AG') --> "ag".
'iso3166-1'('Anguilla', 'iso3166-1':'AI') --> "ai".
'iso3166-1'('Albania', 'iso3166-1':'AL') --> "al".
'iso3166-1'('Armenia', 'iso3166-1':'AM') --> "am".
'iso3166-1'('Angola', 'iso3166-1':'AO') --> "ao".
'iso3166-1'('Antarctica', 'iso3166-1':'AQ') --> "aq".
'iso3166-1'('Argentina', 'iso3166-1':'AR') --> "ar".
'iso3166-1'('American samoa', 'iso3166-1':'AS') --> "as".
'iso3166-1'('Austria', 'iso3166-1':'AT') --> "at".
'iso3166-1'('Australia', 'iso3166-1':'AU') --> "au".
'iso3166-1'('Aruba', 'iso3166-1':'AW') --> "aw".
'iso3166-1'('Åland islands', 'iso3166-1':'AX') --> "ax".
'iso3166-1'('Azerbaijan', 'iso3166-1':'AZ') --> "az".
'iso3166-1'('Bosnia and herzegovina', 'iso3166-1':'BA') --> "ba".
'iso3166-1'('Barbados', 'iso3166-1':'BB') --> "bb".
'iso3166-1'('Bangladesh', 'iso3166-1':'BD') --> "bd".
'iso3166-1'('Belgium', 'iso3166-1':'BE') --> "be".
'iso3166-1'('Burkina faso', 'iso3166-1':'BF') --> "bf".
'iso3166-1'('Bulgaria', 'iso3166-1':'BG') --> "bg".
'iso3166-1'('Bahrain', 'iso3166-1':'BH') --> "bh".
'iso3166-1'('Burundi', 'iso3166-1':'BI') --> "bi".
'iso3166-1'('Benin', 'iso3166-1':'BJ') --> "bj".
'iso3166-1'('Saint barthélemy', 'iso3166-1':'BL') --> "bl".
'iso3166-1'('Bermuda', 'iso3166-1':'BM') --> "bm".
'iso3166-1'('Brunei darussalam', 'iso3166-1':'BN') --> "bn".
'iso3166-1'('Bolivia, plurinational state of', 'iso3166-1':'BO') --> "bo".
'iso3166-1'('Bonaire, sint eustatius and saba', 'iso3166-1':'BQ') --> "bq".
'iso3166-1'('Brazil', 'iso3166-1':'BR') --> "br".
'iso3166-1'('Bahamas', 'iso3166-1':'BS') --> "bs".
'iso3166-1'('Bhutan', 'iso3166-1':'BT') --> "bt".
'iso3166-1'('Bouvet island', 'iso3166-1':'BV') --> "bv".
'iso3166-1'('Botswana', 'iso3166-1':'BW') --> "bw".
'iso3166-1'('Belarus', 'iso3166-1':'BY') --> "by".
'iso3166-1'('Belize', 'iso3166-1':'BZ') --> "bz".
'iso3166-1'('Canada', 'iso3166-1':'CA') --> "ca".
'iso3166-1'('Cocos (keeling) islands', 'iso3166-1':'CC') --> "cc".
'iso3166-1'('Congo, the democratic republic of the', 'iso3166-1':'CD') --> "cd".
'iso3166-1'('Central african republic', 'iso3166-1':'CF') --> "cf".
'iso3166-1'('Congo', 'iso3166-1':'CG') --> "cg".
'iso3166-1'('Switzerland', 'iso3166-1':'CH') --> "ch".
'iso3166-1'('Côte d\'ivoire', 'iso3166-1':'CI') --> "ci".
'iso3166-1'('Cook islands', 'iso3166-1':'CK') --> "ck".
'iso3166-1'('Chile', 'iso3166-1':'CL') --> "cl".
'iso3166-1'('Cameroon', 'iso3166-1':'CM') --> "cm".
'iso3166-1'('China', 'iso3166-1':'CN') --> "cn".
'iso3166-1'('Colombia', 'iso3166-1':'CO') --> "co".
'iso3166-1'('Costa rica', 'iso3166-1':'CR') --> "cr".
'iso3166-1'('Cuba', 'iso3166-1':'CU') --> "cu".
'iso3166-1'('Cape verde', 'iso3166-1':'CV') --> "cv".
'iso3166-1'('Curaçao', 'iso3166-1':'CW') --> "cw".
'iso3166-1'('Christmas island', 'iso3166-1':'CX') --> "cx".
'iso3166-1'('Cyprus', 'iso3166-1':'CY') --> "cy".
'iso3166-1'('Czech republic', 'iso3166-1':'CZ') --> "cz".
'iso3166-1'('Germany', 'iso3166-1':'DE') --> "de".
'iso3166-1'('Djibouti', 'iso3166-1':'DJ') --> "dj".
'iso3166-1'('Denmark', 'iso3166-1':'DK') --> "dk".
'iso3166-1'('Dominica', 'iso3166-1':'DM') --> "dm".
'iso3166-1'('Dominican republic', 'iso3166-1':'DO') --> "do".
'iso3166-1'('Algeria', 'iso3166-1':'DZ') --> "dz".
'iso3166-1'('Ecuador', 'iso3166-1':'EC') --> "ec".
'iso3166-1'('Estonia', 'iso3166-1':'EE') --> "ee".
'iso3166-1'('Egypt', 'iso3166-1':'EG') --> "eg".
'iso3166-1'('Western sahara', 'iso3166-1':'EH') --> "eh".
'iso3166-1'('Eritrea', 'iso3166-1':'ER') --> "er".
'iso3166-1'('Spain', 'iso3166-1':'ES') --> "es".
'iso3166-1'('Ethiopia', 'iso3166-1':'ET') --> "et".
'iso3166-1'('Finland', 'iso3166-1':'FI') --> "fi".
'iso3166-1'('Fiji', 'iso3166-1':'FJ') --> "fj".
'iso3166-1'('Falkland islands (malvinas)', 'iso3166-1':'FK') --> "fk".
'iso3166-1'('Micronesia, federated states of', 'iso3166-1':'FM') --> "fm".
'iso3166-1'('Faroe islands', 'iso3166-1':'FO') --> "fo".
'iso3166-1'('France', 'iso3166-1':'FR') --> "fr".
'iso3166-1'('Gabon', 'iso3166-1':'GA') --> "ga".
'iso3166-1'('United kingdom', 'iso3166-1':'GB') --> "gb".
'iso3166-1'('Grenada', 'iso3166-1':'GD') --> "gd".
'iso3166-1'('Georgia', 'iso3166-1':'GE') --> "ge".
'iso3166-1'('French guiana', 'iso3166-1':'GF') --> "gf".
'iso3166-1'('Guernsey', 'iso3166-1':'GG') --> "gg".
'iso3166-1'('Ghana', 'iso3166-1':'GH') --> "gh".
'iso3166-1'('Gibraltar', 'iso3166-1':'GI') --> "gi".
'iso3166-1'('Greenland', 'iso3166-1':'GL') --> "gl".
'iso3166-1'('Gambia', 'iso3166-1':'GM') --> "gm".
'iso3166-1'('Guinea', 'iso3166-1':'GN') --> "gn".
'iso3166-1'('Guadeloupe', 'iso3166-1':'GP') --> "gp".
'iso3166-1'('Equatorial guinea', 'iso3166-1':'GQ') --> "gq".
'iso3166-1'('Greece', 'iso3166-1':'GR') --> "gr".
'iso3166-1'('South georgia and the south sandwich islands', 'iso3166-1':'GS') --> "gs".
'iso3166-1'('Guatemala', 'iso3166-1':'GT') --> "gt".
'iso3166-1'('Guam', 'iso3166-1':'GU') --> "gu".
'iso3166-1'('Guinea-bissau', 'iso3166-1':'GW') --> "gw".
'iso3166-1'('Guyana', 'iso3166-1':'GY') --> "gy".
'iso3166-1'('Hong kong', 'iso3166-1':'HK') --> "hk".
'iso3166-1'('Heard island and mcdonald islands', 'iso3166-1':'HM') --> "hm".
'iso3166-1'('Honduras', 'iso3166-1':'HN') --> "hn".
'iso3166-1'('Croatia', 'iso3166-1':'HR') --> "hr".
'iso3166-1'('Haiti', 'iso3166-1':'HT') --> "ht".
'iso3166-1'('Hungary', 'iso3166-1':'HU') --> "hu".
'iso3166-1'('Indonesia', 'iso3166-1':'ID') --> "id".
'iso3166-1'('Ireland', 'iso3166-1':'IE') --> "ie".
'iso3166-1'('Israel', 'iso3166-1':'IL') --> "il".
'iso3166-1'('Isle of man', 'iso3166-1':'IM') --> "im".
'iso3166-1'('India', 'iso3166-1':'IN') --> "in".
'iso3166-1'('British indian ocean territory', 'iso3166-1':'IO') --> "io".
'iso3166-1'('Iraq', 'iso3166-1':'IQ') --> "iq".
'iso3166-1'('Iran, islamic republic of', 'iso3166-1':'IR') --> "ir".
'iso3166-1'('Iceland', 'iso3166-1':'IS') --> "is".
'iso3166-1'('Italy', 'iso3166-1':'IT') --> "it".
'iso3166-1'('Jersey', 'iso3166-1':'JE') --> "je".
'iso3166-1'('Jamaica', 'iso3166-1':'JM') --> "jm".
'iso3166-1'('Jordan', 'iso3166-1':'JO') --> "jo".
'iso3166-1'('Japan', 'iso3166-1':'JP') --> "jp".
'iso3166-1'('Kenya', 'iso3166-1':'KE') --> "ke".
'iso3166-1'('Kyrgyzstan', 'iso3166-1':'KG') --> "kg".
'iso3166-1'('Cambodia', 'iso3166-1':'KH') --> "kh".
'iso3166-1'('Kiribati', 'iso3166-1':'KI') --> "ki".
'iso3166-1'('Comoros', 'iso3166-1':'KM') --> "km".
'iso3166-1'('Saint kitts and nevis', 'iso3166-1':'KN') --> "kn".
'iso3166-1'('Korea, democratic people\'s republic of', 'iso3166-1':'KP') --> "kp".
'iso3166-1'('Korea, republic of', 'iso3166-1':'KR') --> "kr".
'iso3166-1'('Kuwait', 'iso3166-1':'KW') --> "kw".
'iso3166-1'('Cayman islands', 'iso3166-1':'KY') --> "ky".
'iso3166-1'('Kazakhstan', 'iso3166-1':'KZ') --> "kz".
'iso3166-1'('Lao people\'s democratic republic', 'iso3166-1':'LA') --> "la".
'iso3166-1'('Lebanon', 'iso3166-1':'LB') --> "lb".
'iso3166-1'('Saint lucia', 'iso3166-1':'LC') --> "lc".
'iso3166-1'('Liechtenstein', 'iso3166-1':'LI') --> "li".
'iso3166-1'('Sri lanka', 'iso3166-1':'LK') --> "lk".
'iso3166-1'('Liberia', 'iso3166-1':'LR') --> "lr".
'iso3166-1'('Lesotho', 'iso3166-1':'LS') --> "ls".
'iso3166-1'('Lithuania', 'iso3166-1':'LT') --> "lt".
'iso3166-1'('Luxembourg', 'iso3166-1':'LU') --> "lu".
'iso3166-1'('Latvia', 'iso3166-1':'LV') --> "lv".
'iso3166-1'('Libya', 'iso3166-1':'LY') --> "ly".
'iso3166-1'('Morocco', 'iso3166-1':'MA') --> "ma".
'iso3166-1'('Monaco', 'iso3166-1':'MC') --> "mc".
'iso3166-1'('Moldova, republic of', 'iso3166-1':'MD') --> "md".
'iso3166-1'('Montenegro', 'iso3166-1':'ME') --> "me".
'iso3166-1'('Saint martin (french part)', 'iso3166-1':'MF') --> "mf".
'iso3166-1'('Madagascar', 'iso3166-1':'MG') --> "mg".
'iso3166-1'('Marshall islands', 'iso3166-1':'MH') --> "mh".
'iso3166-1'('Macedonia, the former yugoslav republic of', 'iso3166-1':'MK') --> "mk".
'iso3166-1'('Mali', 'iso3166-1':'ML') --> "ml".
'iso3166-1'('Myanmar', 'iso3166-1':'MM') --> "mm".
'iso3166-1'('Mongolia', 'iso3166-1':'MN') --> "mn".
'iso3166-1'('Macao', 'iso3166-1':'MO') --> "mo".
'iso3166-1'('Northern mariana islands', 'iso3166-1':'MP') --> "mp".
'iso3166-1'('Martinique', 'iso3166-1':'MQ') --> "mq".
'iso3166-1'('Mauritania', 'iso3166-1':'MR') --> "mr".
'iso3166-1'('Montserrat', 'iso3166-1':'MS') --> "ms".
'iso3166-1'('Malta', 'iso3166-1':'MT') --> "mt".
'iso3166-1'('Mauritius', 'iso3166-1':'MU') --> "mu".
'iso3166-1'('Maldives', 'iso3166-1':'MV') --> "mv".
'iso3166-1'('Malawi', 'iso3166-1':'MW') --> "mw".
'iso3166-1'('Mexico', 'iso3166-1':'MX') --> "mx".
'iso3166-1'('Malaysia', 'iso3166-1':'MY') --> "my".
'iso3166-1'('Mozambique', 'iso3166-1':'MZ') --> "mz".
'iso3166-1'('Namibia', 'iso3166-1':'NA') --> "na".
'iso3166-1'('New caledonia', 'iso3166-1':'NC') --> "nc".
'iso3166-1'('Niger', 'iso3166-1':'NE') --> "ne".
'iso3166-1'('Norfolk island', 'iso3166-1':'NF') --> "nf".
'iso3166-1'('Nigeria', 'iso3166-1':'NG') --> "ng".
'iso3166-1'('Nicaragua', 'iso3166-1':'NI') --> "ni".
'iso3166-1'('Netherlands', 'iso3166-1':'NL') --> "nl".
'iso3166-1'('Norway', 'iso3166-1':'NO') --> "no".
'iso3166-1'('Nepal', 'iso3166-1':'NP') --> "np".
'iso3166-1'('Nauru', 'iso3166-1':'NR') --> "nr".
'iso3166-1'('Niue', 'iso3166-1':'NU') --> "nu".
'iso3166-1'('New zealand', 'iso3166-1':'NZ') --> "nz".
'iso3166-1'('Oman', 'iso3166-1':'OM') --> "om".
'iso3166-1'('Panama', 'iso3166-1':'PA') --> "pa".
'iso3166-1'('Peru', 'iso3166-1':'PE') --> "pe".
'iso3166-1'('French polynesia', 'iso3166-1':'PF') --> "pf".
'iso3166-1'('Papua new guinea', 'iso3166-1':'PG') --> "pg".
'iso3166-1'('Philippines', 'iso3166-1':'PH') --> "ph".
'iso3166-1'('Pakistan', 'iso3166-1':'PK') --> "pk".
'iso3166-1'('Poland', 'iso3166-1':'PL') --> "pl".
'iso3166-1'('Saint pierre and miquelon', 'iso3166-1':'PM') --> "pm".
'iso3166-1'('Pitcairn', 'iso3166-1':'PN') --> "pn".
'iso3166-1'('Puerto rico', 'iso3166-1':'PR') --> "pr".
'iso3166-1'('Palestinian territory, occupied', 'iso3166-1':'PS') --> "ps".
'iso3166-1'('Portugal', 'iso3166-1':'PT') --> "pt".
'iso3166-1'('Palau', 'iso3166-1':'PW') --> "pw".
'iso3166-1'('Paraguay', 'iso3166-1':'PY') --> "py".
'iso3166-1'('Qatar', 'iso3166-1':'QA') --> "qa".
'iso3166-1'('Réunion', 'iso3166-1':'RE') --> "re".
'iso3166-1'('Romania', 'iso3166-1':'RO') --> "ro".
'iso3166-1'('Serbia', 'iso3166-1':'RS') --> "rs".
'iso3166-1'('Russian federation', 'iso3166-1':'RU') --> "ru".
'iso3166-1'('Rwanda', 'iso3166-1':'RW') --> "rw".
'iso3166-1'('Saudi arabia', 'iso3166-1':'SA') --> "sa".
'iso3166-1'('Solomon islands', 'iso3166-1':'SB') --> "sb".
'iso3166-1'('Seychelles', 'iso3166-1':'SC') --> "sc".
'iso3166-1'('Sudan', 'iso3166-1':'SD') --> "sd".
'iso3166-1'('Sweden', 'iso3166-1':'SE') --> "se".
'iso3166-1'('Singapore', 'iso3166-1':'SG') --> "sg".
'iso3166-1'('Saint helena, ascension and tristan da cunha', 'iso3166-1':'SH') --> "sh".
'iso3166-1'('Slovenia', 'iso3166-1':'SI') --> "si".
'iso3166-1'('Svalbard and jan mayen', 'iso3166-1':'SJ') --> "sj".
'iso3166-1'('Slovakia', 'iso3166-1':'SK') --> "sk".
'iso3166-1'('Sierra leone', 'iso3166-1':'SL') --> "sl".
'iso3166-1'('San marino', 'iso3166-1':'SM') --> "sm".
'iso3166-1'('Senegal', 'iso3166-1':'SN') --> "sn".
'iso3166-1'('Somalia', 'iso3166-1':'SO') --> "so".
'iso3166-1'('Suriname', 'iso3166-1':'SR') --> "sr".
'iso3166-1'('South sudan', 'iso3166-1':'SS') --> "ss".
'iso3166-1'('Sao tome and principe', 'iso3166-1':'ST') --> "st".
'iso3166-1'('El salvador', 'iso3166-1':'SV') --> "sv".
'iso3166-1'('Sint maarten (dutch part)', 'iso3166-1':'SX') --> "sx".
'iso3166-1'('Syrian arab republic', 'iso3166-1':'SY') --> "sy".
'iso3166-1'('Swaziland', 'iso3166-1':'SZ') --> "sz".
'iso3166-1'('Turks and caicos islands', 'iso3166-1':'TC') --> "tc".
'iso3166-1'('Chad', 'iso3166-1':'TD') --> "td".
'iso3166-1'('French southern territories', 'iso3166-1':'TF') --> "tf".
'iso3166-1'('Togo', 'iso3166-1':'TG') --> "tg".
'iso3166-1'('Thailand', 'iso3166-1':'TH') --> "th".
'iso3166-1'('Tajikistan', 'iso3166-1':'TJ') --> "tj".
'iso3166-1'('Tokelau', 'iso3166-1':'TK') --> "tk".
'iso3166-1'('Timor-leste', 'iso3166-1':'TL') --> "tl".
'iso3166-1'('Turkmenistan', 'iso3166-1':'TM') --> "tm".
'iso3166-1'('Tunisia', 'iso3166-1':'TN') --> "tn".
'iso3166-1'('Tonga', 'iso3166-1':'TO') --> "to".
'iso3166-1'('Turkey', 'iso3166-1':'TR') --> "tr".
'iso3166-1'('Trinidad and tobago', 'iso3166-1':'TT') --> "tt".
'iso3166-1'('Tuvalu', 'iso3166-1':'TV') --> "tv".
'iso3166-1'('Taiwan, province of china', 'iso3166-1':'TW') --> "tw".
'iso3166-1'('Tanzania, united republic of', 'iso3166-1':'TZ') --> "tz".
'iso3166-1'('Ukraine', 'iso3166-1':'UA') --> "ua".
'iso3166-1'('Uganda', 'iso3166-1':'UG') --> "ug".
'iso3166-1'('United states minor outlying islands', 'iso3166-1':'UM') --> "um".
'iso3166-1'('United states', 'iso3166-1':'US') --> "us".
'iso3166-1'('Uruguay', 'iso3166-1':'UY') --> "uy".
'iso3166-1'('Uzbekistan', 'iso3166-1':'UZ') --> "uz".
'iso3166-1'('Holy see (vatican city state)', 'iso3166-1':'VA') --> "va".
'iso3166-1'('Saint vincent and the grenadines', 'iso3166-1':'VC') --> "vc".
'iso3166-1'('Venezuela, bolivarian republic of', 'iso3166-1':'VE') --> "ve".
'iso3166-1'('Virgin islands, british', 'iso3166-1':'VG') --> "vg".
'iso3166-1'('Virgin islands, u.s.', 'iso3166-1':'VI') --> "vi".
'iso3166-1'('Viet nam', 'iso3166-1':'VN') --> "vn".
'iso3166-1'('Vanuatu', 'iso3166-1':'VU') --> "vu".
'iso3166-1'('Wallis and futuna', 'iso3166-1':'WF') --> "wf".
'iso3166-1'('Samoa', 'iso3166-1':'WS') --> "ws".
'iso3166-1'('Yemen', 'iso3166-1':'YE') --> "ye".
'iso3166-1'('Mayotte', 'iso3166-1':'YT') --> "yt".
'iso3166-1'('South africa', 'iso3166-1':'ZA') --> "za".
'iso3166-1'('Zambia', 'iso3166-1':'ZM') --> "zm".
'iso3166-1'('Zimbabwe', 'iso3166-1':'ZW') --> "zw".
