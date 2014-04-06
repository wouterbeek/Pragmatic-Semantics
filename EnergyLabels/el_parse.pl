:- module(
  el_parse,
  [
    el_parse/1, % +FromFile:atom
    el_parse/2 % +FromFile:atom
               % +ToDirectory:atom
  ]
).

/** <module> Parses the energylabels

Process all energylabels in a single parse.

@author Wouter Beek
@version 2013/06-2013/07, 2013/09-2013/12
*/

:- use_module(ap(ap_stat)).
:- use_module(dcg(dcg_generic)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(settings)).
:- use_module(library(xpath)).
:- use_module(os(datetime_ext)).
:- use_module(os(file_ext)).
:- use_module(rdf(rdf_build)).
:- use_module(rdf_file(rdf_serial)).
:- use_module(xml(xml_namespace)).
:- use_module(xml(xml_stream)).
:- use_module(xml(xml_to_rdf)).

:- xml_register_namespace(el, 'https://data.overheid.nl/data/dataset/energielabels-agentschap-nl/').

:- rdf_meta(trans(?,r,?)).

:- setting(energylabels_graph, atom, el, 'The name of the energylabels graph.').



el_parse(File):-
  file_directory_name(File, Dir),
  el_parse(File, Dir).

%! el_parse(+FromFile:atom, +ToDirectory:atom) is det.
% Since the number of entries in the energylabels dataset is too big to
% keep into memory, we are going to translate the XML into 10 separate
% RDF graphs. We do this sequentially, cleaning out the internal RDF index
% in between these 10 runs.

el_parse(FromFile, ToDir):-
  % STATS
  ap_stage_init(2354560),
  
  forall(
    between(1, 9, N),
    call_cleanup(
      (
        atom_number(Prefix, N),
        xml_stream(
          FromFile,
          'Pandcertificaat',
          process_postcode(el, Prefix)
        ),
        atomic_list_concat([el,Prefix], '_', ToFileName),
        absolute_file_name(
          ToFileName,
          ToFile,
          [access(write),file_type(turtle),relative_to(ToDir)]
        ),
        rdf_save([format(turtle)], el, ToFile)
      ),
      rdf_unload_graph(el)
    )
  ).

process_postcode(G, Prefix, DOM0):-
  Spec =.. ['Pandcertificaat',content],
  xpath_chk(DOM0, //Spec, DOM1),
  process_postcode_(G, Prefix, DOM1).

process_postcode_(G, Prefix, DOM1):-
  % Filter.
  memberchk(element('PandVanMeting_postcode', _, [Postcode]), DOM1),
  sub_atom(Postcode, 0, _Length, _After, Prefix), !,
  
  create_resource(
    DOM1,
    [
      'PandVanMeting_postcode',
      'PandVanMeting_huisnummer',
      'PandVanMeting_huisnummer_toev'
    ],
    trans,
    el:'Certificate',
    G,
    Cert,
    DOM2
  ),
  create_triples(
    DOM2,
    [
      'Meting_geldig_tot',
      'PandVanMeting_energieklasse',
      'PandVanMeting_energieprestatieindex',
      'PandVanMeting_energieverbruikco2',
      'PandVanMeting_energieverbruikelektriciteit',
      'PandVanMeting_energieverbruikgas',
      'PandVanMeting_energieverbruiktype',
      'PandVanMeting_energieverbruikmj',
      'PandVanMeting_energieverbruikwarmte',
      'PandVanMeting_gebouwcode',
      'PandVanMeting_opnameblauwdruk',
      'PandVanMeting_opnamedatum',
      'PandVanMeting_opnameeigenaarinformatie',
      'PandVanMeting_opnameobservatie'
    ],
    trans,
    Cert,
    G,
    DOM3
  ),
  create_resource(
    DOM3,
    [
      'Pand_postcode',
      'Pand_huisnummer',
      'Pand_huisnummer_toev'
    ],
    trans,
    el:'Building',
    G,
    Building,
    DOM4
  ),
  rdf_assert(Building, el:certificaat, Cert, G),
  create_triples(
    DOM4,
    [
      'Afmeldnummer',
      'Pand_cert_type',
      'Pand_gebouwcode',
      'Pand_plaats',
      'Pand_registratiedatum'
    ],
    trans,
    Building,
    G,
    []
  ),
  
  % STATS
  ap_stage_tick.
process_postcode_(_G, _Prefix, _DOM).

trans('Afmeldnummer',                               el:afmeldnummer,                   integer).
trans('Meting_geldig_tot',                          el:meting_geldig_tot,              date   ).
trans('Pand_cert_type',                             el:cert_type,                      literal).
trans('Pand_gebouwcode',                            el:gebouwcode,                     literal).
trans('Pand_huisnummer',                            el:huisnummer,                     integer).
trans('Pand_huisnummer_toev',                       el:huisnummer_toevoeging,          literal).
trans('Pand_plaats',                                el:plaats,                         literal).
trans('Pand_postcode',                              el:postcode,                       literal).
trans('Pand_registratiedatum',                      el:registratie_datum,              date   ).
trans('PandVanMeting_energieklasse',                el:energie_klasse,                 literal).
trans('PandVanMeting_energieprestatieindex',        el:energie_prestatieindex,         decimal).
trans('PandVanMeting_energieverbruikco2',           el:energie_verbruik_co2,           decimal).
trans('PandVanMeting_energieverbruikelektriciteit', el:energie_verbruik_elektriciteit, decimal).
trans('PandVanMeting_energieverbruikgas',           el:energie_verbruik_gas,           decimal).
trans('PandVanMeting_energieverbruiktype',          el:energie_verbruik_type,          literal).
trans('PandVanMeting_energieverbruikmj',            el:energie_verbruik_mj,            decimal).
trans('PandVanMeting_energieverbruikwarmte',        el:energie_verbruik_warmte,        decimal).
trans('PandVanMeting_gebouwcode',                   el:gebouwcode,                     literal).
trans('PandVanMeting_huisnummer',                   el:huisnummer,                     integer).
trans('PandVanMeting_huisnummer_toev',              el:huisnummer_toevoeging,          literal).
trans('PandVanMeting_opnameblauwdruk',              el:opname_blauwdruk,               integer).
trans('PandVanMeting_opnamedatum',                  el:opname_datum,                   date   ).
trans('PandVanMeting_opnameeigenaarinformatie',     el:opname_eigenaarinformatie,      integer).
trans('PandVanMeting_opnameobservatie',             el:opname_observatie,              integer).
trans('PandVanMeting_postcode',                     el:postcode,                       literal).

