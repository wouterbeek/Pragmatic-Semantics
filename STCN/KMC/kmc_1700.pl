:- module(
  kmc_1700,
  [
    assert_schema_kmc_1700/1, % +Graph:graph
    kmc_1700//2, % +Graph:atom
                 % +PPN:uri
    statistics_kmc_1700/2 % +Graph:atom
                          % -Rows:list(list)
  ]
).

/** <module> KMC 1700 - Country

Required field. Cannot be repeated.

KMC 1700 contains the country code.

# Tables for country codes

## Country code overlap between STCN and ISO

!!kmc_1700!!recognized_country!!2!!

## Identical countries in STCN

!!kmc_1700!!same_country!!2!!

## Unrecognized country codes (i.e., in STCN but not in ISO)

!!kmc_1700!!unrecognized_country!!2!!

Some of these countries no longer exist, e.g. Czechoslovakia (code =cs=).
TODO: See whether these countries can be related to their current equivalents,
using the city of the publisher as well (to discriminate between multiple
country options).

@author Wouter Beek
@tbd Relocate the recognized, unrecognized, and same country codes/names
     to an OCLC-specific module.
@tbd Cover the unrecognized country codes by including support for
     ISO 3166-3, i.e. the list of codes for past countries.
@see http://www.kb.nl/kbhtml/stcnhandleiding/1700.html
@see http://support.oclc.org/ggc/richtlijnen/?id=12&ln=nl&sec=k-1700
@version 2013/01-2013/06, 2013/09, 2014/03
*/

:- use_module(dcg(dcg_ascii)).
:- use_module(dcg(dcg_content)).
:- use_module(geo('iso3166-1')).
:- use_module(library(debug)).
:- use_module(library(semweb/rdf_db)).
:- use_module(rdf(rdf_build)).
:- use_module(rdf(rdf_stat)).
:- use_module(rdf_term(rdf_string)).
:- use_module(rdfs(rdfs_build)).
:- use_module(rdfs(rdfs_label_ext)).
:- use_module(xml(xml_namespace)).

:- discontiguous(recognized_country/2).
:- discontiguous(recognized_country/3).
:- discontiguous(unrecognized_country/2).
:- discontiguous(unrecognized_country/3).

:- xml_register_namespace(stcn, 'http://stcn.data2semantics.org/resource/').
:- xml_register_namespace(stcnv, 'http://stcn.data2semantics.org/vocab/').



assert_schema_kmc_1700(G):-
  rdf_assert_property(stcnv:landcode, G),
  rdfs_assert_label(stcnv:landcode, nl, 'land van uitgave', G),
  rdf_assert_string(stcnv:landcode, stcnv:kb_name, 'KMC 1700', G),
  rdfs_assert_seeAlso(stcnv:landcode,
      'http://www.kb.nl/kbhtml/stcnhandleiding/1700.html', G),
  rdfs_assert_seeAlso(stcnv:landcode,
      'http://support.oclc.org/ggc/richtlijnen/?id=12&ln=nl&sec=k-1700', G),
  rdfs_assert_domain(stcnv:landcode, stcnv:'Publication', G),
  rdfs_assert_range(stcnv:landcode, 'iso3166-1':'Country', G),

  rdfs_assert_subproperty(stcnv:displayed_country, stcnv:landcode, G),
  rdfs_assert_label(stcnv:displayed_country, 'weergegeven land van uitgave', nl, G),
  rdfs_assert_comment(stcnv:displayed_country,
      'In geval van een gefingeerd of onjuist impressum wordt in /2 de\c
       landcode opgenomen die hoort bij het juiste impressum zoals dat in\c
       een annotatie is verantwoord.', nl, G),

  rdfs_assert_subproperty(stcnv:actual_country, stcnv:landcode, G),
  rdfs_assert_label(stcnv:actual_country, 'daadwerkelijk land van uitgave',
      nl, G),

  rdfs_assert_class(stcnv:'Country', G),
  rdfs_assert_label(stcnv:'Country', 'OCLC country code not supported by ISO',
      en, G),

  forall(
     unrecognized_country(Abbr1, Name),
     (
        atomic_list_concat(['Country',Abbr1], '/', Abbr2),
        rdf_global_id(stcnv:Abbr2, Abbr3),
        rdf_assert_individual(Abbr3, stcnv:'Country', G),
        rdfs_assert_label(Abbr3, Name, en, G)
     )
  ),

  'assert_iso3166-1_schema'(G),
  % Add Dutch labels for countries that occur in the OCLC.
  forall(
    (
      recognized_country(Atom, NL_Name),
      atom_codes(Atom, Codes),
      phrase('iso3166-1'(_EN_Name, Country), Codes)
    ),
    rdfs_assert_label(Country, nl, NL_Name, G)
  ).

%! kmc_1700(+Graph:atom, +PPN:uri)// is det.

kmc_1700(G, PPN) -->
  "/", country_property(Pred),
  (
     'iso3166-1'(_Name, Country1)
  ->
    {rdf_global_id(Country1, Country3)}
  ;
    {unrecognized_country(Country1, _Name)},
    atom(Country1),
    {
      debug(kmc_1700, '[PPN ~w] Unrecognized country code: ~w.', [PPN,Country1]),
      atomic_list_concat(['Country',Country1], '/', Country2),
      rdf_global_id(stcnv:Country2, Country3)
    }
  ),

  {rdf_assert(PPN, Pred, Country3, G)}, !,
  kmc_1700(G, PPN).
kmc_1700(_G, _PPN) --> [].

% The country that is mentioned in the publication itself.
country_property(Pred) -->
  "1",
  {rdf_global_id(stcnv:displayed_country, Pred)}.
% Actual country of publication.
country_property(Pred) -->
  "2",
  {rdf_global_id(stcnv:actual_country, Pred)}.

% KMC 1700 country codes that are in ISO 3166-1.

recognized_country(Code, Name):-
  recognized_country(Code, Name, _Period).
recognized_country(ad, 'Andorra').
recognized_country(ae, 'Verenigde Arabische Emiraten').
recognized_country(af, 'Afganistan').
recognized_country(ag, 'Antigua en Baruda').
recognized_country(al, 'Albanië').
recognized_country(am, 'Armenië').
recognized_country(ao, 'Angola').
recognized_country(aq, 'Antarctica').
recognized_country(ar, 'Argentinië').
recognized_country(as, 'Samoa (Amerikaans)').
recognized_country(at, 'Oostenrijk').
recognized_country(au, 'Australië').
recognized_country(aw, 'Aruba').
recognized_country(az, 'Azerbeidjan').
recognized_country(ba, 'Bosnië-Herzogowina').
recognized_country(bb, 'Barbados').
recognized_country(bd, 'Bangladesh').
recognized_country(be, 'België').
recognized_country(bg, 'Bulgarije').
recognized_country(bh, 'Bahrein').
recognized_country(bi, 'Boeroendi').
recognized_country(bj, 'Benin').
recognized_country(bm, 'Bermuda').
recognized_country(bn, 'Brunei').
recognized_country(bo, 'Bolivia').
recognized_country(bq, 'Antarctica (Brits)').
recognized_country(br, 'Brazilië').
recognized_country(bs, 'Bahama Eilanden').
recognized_country(bt, 'Bhoetan').
recognized_country(bv, 'Bouvet Eilanden').
recognized_country(bw, 'Botswana').
recognized_country(by, 'Witrusland').
recognized_country(bz, 'Belize').
recognized_country(ca, 'Canada').
recognized_country(cc, 'Cocos (Keeling) Eilanden').
recognized_country(cf, 'Centraal Afrikaanse Republiek').
recognized_country(cg, 'Congo (Brazzaville)').
recognized_country(ch, 'Zwitserland').
recognized_country(ci, 'Ivoorkust').
recognized_country(ck, 'Cook Eilanden').
recognized_country(cl, 'Chili').
recognized_country(cm, 'Kameroen').
recognized_country(cn, 'China [Volksrepubliek]').
recognized_country(co, 'Colombia').
recognized_country(cr, 'Costa Rica').
recognized_country(cu, 'Cuba').
recognized_country(cv, 'Kaapverdische Eilanden').
recognized_country(cx, 'Christmas Eiland').
recognized_country(cy, 'Cyprus').
recognized_country(cz, 'Tsjechië', period(date(1933,01,01),inf)).
recognized_country(dd, 'Duitsland [Democratische Republiek]', period(date(1949),date(1990,10))).
recognized_country(de, 'West-Berlijn', period(date(1948),date(1990))).
recognized_country(de, 'Duitsland [Bondsrepubliek]', period(date(1990,10),inf)).
recognized_country(dj, 'Djibouti').
recognized_country(dk, 'Denemarken').
recognized_country(dm, 'Dominica').
recognized_country(do, 'Dominicaanse Republiek').
recognized_country(dz, 'Algerije').
recognized_country(ec, 'Ecuador').
recognized_country(ee, 'Estland').
recognized_country(eg, 'Egypte en Verenigde Arabische Republiek', period(date(1958),date(1961))).
recognized_country(eh, 'Westelijke Sahara').
recognized_country(er, 'Eritrea').
recognized_country(es, 'Spanje').
recognized_country(et, 'Ethiopië').
recognized_country(fi, 'Finland').
recognized_country(fj, 'Fiji Eilanden').
recognized_country(fk, 'Falkland Eilanden').
recognized_country(fm, 'Micronesië').
recognized_country(fo, 'Faeröer').
recognized_country(fr, 'Frankrijk').
recognized_country(ga, 'Gabon').
recognized_country(gb, 'Verenigd Koninkrijk').
recognized_country(gd, 'Grenada').
recognized_country(ge, 'Gilbert en Ellice Eilanden', period(inf,date(1978))).
recognized_country(gf, 'Guyana (Frans)').
recognized_country(gh, 'Ghana').
recognized_country(gi, 'Gibraltar').
recognized_country(gl, 'Groenland').
recognized_country(gm, 'Gambia').
recognized_country(gn, 'Guinee').
recognized_country(gp, 'Guadeloupe').
recognized_country(gq, 'Equatorial Guinea').
recognized_country(gr, 'Griekenland').
recognized_country(gt, 'Guatemala').
recognized_country(gu, 'Guam').
recognized_country(gw, 'Guinee-Bissau').
recognized_country(gy, 'Guyana').
recognized_country(hk, 'Hong Kong').
recognized_country(hm, 'Heard en McDonald Eilanden').
recognized_country(hn, 'Honduras').
recognized_country(hr, 'Kroatië').
recognized_country(ht, 'Haïti').
recognized_country(hu, 'Hongarije').
recognized_country(id, 'Indonesie').
recognized_country(ie, 'Ierland').
recognized_country(il, 'Israël').
recognized_country(in, 'India').
recognized_country(io, 'Indische Oceaangebied (Brits)').
recognized_country(iq, 'Irak').
recognized_country(ir, 'Iran').
recognized_country(is, 'IJsland').
recognized_country(it, 'Italië').
recognized_country(jm, 'Jamaica').
recognized_country(jo, 'Jordanië').
recognized_country(jp, 'Japan').
recognized_country(ke, 'Kenia').
recognized_country(kg, 'Kyrgyzstan').
recognized_country(kh, 'Kampuchea').
recognized_country(ki, 'Kiribati', period(date(1978,inf))).
recognized_country(km, 'Comoren').
recognized_country(kn, 'St. Kitts-Neville-Anguilla').
recognized_country(kp, 'Korea [Volksrepubliek]').
recognized_country(kr, 'Korea [Republiek]').
recognized_country(kw, 'Koeweit').
recognized_country(ky, 'Cayman Eilanden').
recognized_country(kz, 'Kazachstan').
recognized_country(la, 'Laos').
recognized_country(lb, 'Libanon').
recognized_country(lc, 'St. Lucia').
recognized_country(li, 'Liechtenstein').
recognized_country(lk, 'Sri Lanka').
recognized_country(lr, 'Liberia').
recognized_country(ls, 'Lesotho').
recognized_country(lt, 'Litouwen').
recognized_country(lu, 'Luxemburg').
recognized_country(lv, 'Letland').
recognized_country(ly, 'Libië').
recognized_country(ma, 'Marokko').
recognized_country(mb, 'Montenegro', period(date(2006),inf)).
recognized_country(mc, 'Monaco').
recognized_country(md, 'Moldavië').
recognized_country(me, 'Mayotte').
recognized_country(mg, 'Madagascar').
recognized_country(mh, 'Marshall Eilanden').
recognized_country(mk, 'Macedonië').
recognized_country(ml, 'Mali').
recognized_country(mn, 'Mongolië').
recognized_country(mo, 'Macao').
recognized_country(mp, 'Noordelijke Mariana Eilanden').
recognized_country(mq, 'Martinique').
recognized_country(mr, 'Mauretanië').
recognized_country(ms, 'Montserrat').
recognized_country(mt, 'Malta').
recognized_country(mu, 'Mauritius').
recognized_country(mv, 'Maldiven').
recognized_country(mw, 'Malawi').
recognized_country(mx, 'Mexico').
recognized_country(my, 'Maleisië').
recognized_country(mz, 'Mozambique').
recognized_country(na, 'Namibië').
recognized_country(nc, 'Nieuw-Caledonië').
recognized_country(ne, 'Niger').
recognized_country(nf, 'Norfolk').
recognized_country(ng, 'Nigeria').
recognized_country(ni, 'Nicaragua').
recognized_country(nl, 'Nederland').
recognized_country(no, 'Noorwegen').
recognized_country(np, 'Nepal').
recognized_country(nr, 'Nauru').
recognized_country(nu, 'Niue').
recognized_country(nz, 'Nieuw-Zeeland').
recognized_country(om, 'Oman').
recognized_country(pa, 'Panama').
recognized_country(pe, 'Peru').
recognized_country(pf, 'Polynesië (Frans)').
recognized_country(pg, 'Papoea-Nieuw-Guinea').
recognized_country(ph, 'Filippijnen').
recognized_country(pk, 'Pakistan').
recognized_country(pl, 'Polen').
recognized_country(pm, 'St. Pierre en Miguelon').
recognized_country(pn, 'Pitcairn Eiland').
recognized_country(pr, 'Puerto Rico').
recognized_country(pt, 'Portugal').
recognized_country(pw, 'Palau Eilanden').
recognized_country(py, 'Paraguay').
recognized_country(qa, 'Katar').
recognized_country(qa, 'Quatar').
recognized_country(rh, 'Rhodesië', period(inf,date(1979))).
recognized_country(re, 'Réunion').
recognized_country(ro, 'Roemenië').
recognized_country(ru, 'Russische Federatie').
recognized_country(rw, 'Rwanda').
recognized_country(sa, 'Saoedi-Arabië').
recognized_country(sb, 'Solomon Eilanden').
recognized_country(sc, 'Seychellen').
recognized_country(sd, 'Soedan').
recognized_country(se, 'Zweden').
recognized_country(sg, 'Singapore').
recognized_country(sh, 'St. Helena').
recognized_country(si, 'Slovenië').
recognized_country(sj, 'Spitsbergen en Jan Mayen').
recognized_country(sk, 'Slovakije', period(date(1993,01,01),inf)).
recognized_country(sl, 'Sierra Leone').
recognized_country(sm, 'San Marino').
recognized_country(sn, 'Senegal').
recognized_country(so, 'Somalië').
recognized_country(sq, 'Servië', period(date(2006, 03),inf)).
recognized_country(sr, 'Suriname').
recognized_country(st, 'Sao Tome en Principe').
recognized_country(sv, 'El Salvador').
recognized_country(sy, 'Syrië').
recognized_country(sz, 'Swaziland').
recognized_country(tc, 'Turks en Caicos Eilanden').
recognized_country(td, 'Tsjaad').
recognized_country(tg, 'Togo').
recognized_country(th, 'Thailand').
recognized_country(tj, 'Tadzjikistan').
recognized_country(tk, 'Tokelan Eilanden').
recognized_country(tm, 'Toerkmenistan').
recognized_country(tm, 'Turkmenistan').
recognized_country(tn, 'Tunesië').
recognized_country(to, 'Tonga').
recognized_country(tp, 'Timor (Oost- of Portugees)', period(date(2002,05,20),inf)).
recognized_country(tr, 'Turkije').
recognized_country(tt, 'Trinidad en Tobago').
recognized_country(tv, 'Tuvalu', period(date(1978,inf))).
recognized_country(tw, 'Taiwan').
recognized_country(tz, 'Tanzania').
recognized_country(ua, 'Oekraïne').
recognized_country(ug, 'Oeganda').
recognized_country(ug, 'Uganda').
recognized_country(us, 'Verenigde Staten').
recognized_country(uy, 'Uruguay').
recognized_country(uz, 'Oezbekistan').
recognized_country(va, 'Vaticaan').
recognized_country(vc, 'St. Vincent').
recognized_country(ve, 'Venezuela').
recognized_country(vg, 'Maagden Eilanden (Brits)').
recognized_country(vi, 'Maagden Eilanden (Amerikaans)').
recognized_country(vn, 'Vietnam', period(date(1978),inf)).
recognized_country(vu, 'Vanuatu').
recognized_country(wf, 'Wallis en Futuna Eilanden').
recognized_country(ws, 'Samoa (West)').
recognized_country(xx, 'Onbekend').
recognized_country(ye, 'Jemen [Arabische Republiek]').
recognized_country(yu, 'Joegoslavië', period(inf,date(1992,10))).
recognized_country(yu, 'Servië en Montenegro', period(date(1992, 10),date(2006,03))).
recognized_country(za, 'Zuid Afrika').
recognized_country(zm, 'Zambia').
recognized_country(zw, 'Zimbabwe', period(date(1979),inf)).

%! same_country(?AlternativeName:atom, ?Name:atom) is nondet.
% Relates alternative country names to the country abbreviation codes list.
% OCLC defines these country names as denoting the same country.
% This knowledge is not used in KMC 1700 parsing.

same_country('Afar- en Issargebied', 'Djibouti').
same_country('Anguilla', 'St. Kitts-Neville-Anguilla').
same_country('Antillen', 'Nederlandse Antillen').
same_country('Banuatu', 'Vanuatu').
same_country('Brits Honduras', 'Belize').
same_country('Burma', 'Birma').
same_country('Burundi', 'Boeroendi').
same_country('Cambodja', 'Kampuchea').
same_country('Ceylon', 'Sri Lanka').
same_country('China [Republiek]', 'Taiwan').
same_country('Congo (Kinshasa)', 'Zaïre').
same_country('Dahomey', 'Benin').
same_country('Ellice Eilanden', 'Tuvalu').
same_country('Formosa', 'Taiwan').
same_country('Frans Guyana', 'Guyana (Frans)').
same_country('Gilbert Eilanden', 'Kiribati').
same_country('Groot-Brittannië', 'Verenigd Koninkrijk').
same_country('Jan Mayen', 'Spitsbergen').
same_country('Kanaalzone', 'Panama Kanaalzone').
same_country('Kirgizië', 'Kyrgystan').
same_country('Kokoseilanden', 'Cocos (Keeling)').
same_country('Malagasy', 'Madagascar').
same_country('Malvinas', 'Falkland Eilanden').
same_country('Muscat en Oman', 'Oman').
same_country('Neutrale Zone', 'Irak-Saoedi-Arabië').
same_country('Nieuwe Hebriden', 'Vanuatu').
same_country('Opper Volta', 'Burkina Fasso').
same_country('Portugees Guinee', 'Guinee-Bissau').
same_country('Portugees Timor', 'Timor').
same_country('Sovjet-Unie', 'USSR').
same_country('Spaanse Sahara', 'Westelijke Sahara').
same_country('Sudan', 'Soedan').
same_country('Svalbard', 'Spitsbergen').
same_country('Verenigde Arabische Republiek', 'Egypte').
same_country('Zuid-West Afrika', 'Namibië').

statistics_kmc_1700(G, [[A1,V1],[A2,V2],[A3,V3],[A4,V4]]):-
  A1 = 'Publications with some country',
  count_subjects(stcnv:landcode, _, G, V1),
  debug(stcn_statistics, '~w: ~w', [A1, V1]),

  A2 = 'Publication with at least one displayed country',
  count_subjects(stcnv:displayed_country, _, G, V2),
  debug(stcn_statistics, '-- ~w: ~w', [A2, V2]),

  A3 = 'Publications with at least one actual country',
  count_subjects(stcnv:actual_country, _, G, V3),
  debug(stcn_statistics, '-- ~w: ~w', [A3, V3]),

  A4 = 'Countries used',
  count_objects(_, stcnv:country, G, V4),
  debug(stcn_statistics, '-- ~w: ~w', [A4, V4]).

% Not in ISO 3166-1.
unrecognized_country(Code, Name):-
  unrecognized_country(Code, Name, _Period).
unrecognized_country(ac, 'Ashmore en Cartier Eilanden').
unrecognized_country(an, 'Nederlandse Antillen').
unrecognized_country(bu, 'Birma').
unrecognized_country(cs, 'Tsjechoslowakije', period(date(1918),date(1992,12,31))).
unrecognized_country(ct, 'Canton en Enderbury Eilanden').
unrecognized_country(fq, 'Antarctica (Frans)').
unrecognized_country(hv, 'Burkina Fasso').
unrecognized_country(jt, 'Johnston Eiland').
unrecognized_country(go, 'Georgië').
unrecognized_country(ln, 'Central en Southern Line Eilanden').
unrecognized_country(mi, 'Midway Eilanden').
unrecognized_country(nq, 'Dronning Maud Land').
unrecognized_country(nt, 'Irak-Saoedi-Arabië Neutrale Zone').
unrecognized_country(pu, 'Pacific Eilanden (Amerikaans)').
unrecognized_country(pc, 'Pacific Eilanden (Trust Territory)').
unrecognized_country(pz, 'Panama-kanaalzone').
unrecognized_country(su, 'USSR').
unrecognized_country(wk, 'Wake').
unrecognized_country(xx, 'Onbekend').
unrecognized_country(yd, 'Jemen [Volksrepubliek]').
unrecognized_country(zr, 'Zaïre', period(date(1971,10,27),date(1997,05,17))).

