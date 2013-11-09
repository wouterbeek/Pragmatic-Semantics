% sudo find /usr/local/lib/swipl-6.3.12 -name *.qlf | xargs sudo rm

% In this file we assert the infrastructure for the entire PraSem codebase.
% We do not use the module system yet, and we do not place the various
% initializations in their proper place but process them all at once.
% Taking modules in and out of the project therefore involves adding and
% commenting out lines in this file as well.

:- use_module(library(semweb/rdf_db)).

%% assert_file_search_paths is det.
% Loads the file hierarchy for the entire PraSem project.
% This involves both the hierarchy of the static codebase and
% the hierarchy of the personal or user files used by PraSem.

assert_file_search_paths:-
  % The file hierarchy root is determined relative to this file.
  source_file(assert_file_search_paths, PraSemFile),
  file_directory_name(PraSemFile, PraSemDirectory),
  
  assert(user:file_search_path(prasem, PraSemDirectory)),
  assert(user:file_search_path(top, prasem('..'))),

  assert(user:file_search_path(db,           prasem('DB'))),
    assert(user:file_search_path(poems,        db('Poems'))),
    assert(user:file_search_path(stcn,         db('STCN'))),
    assert(user:file_search_path(wordnet,      db('Wordnet'))),
  assert(user:file_search_path(diagnosis,    prasem('Diagnosis'))),
  assert(user:file_search_path(ile,          prasem('ILE'))),
  assert(user:file_search_path(ilp,          prasem('ILP'))),
  assert(user:file_search_path(webqr,        prasem('WebQR'))),
    assert(user:file_search_path(qsim,         webqr('QSIM'))),
  assert(user:file_search_path(server,       prasem('Server'))),
  assert(user:file_search_path(tms,          prasem('TMS'))),
    assert(user:file_search_path(atms,         tms('ATMS'))),
  
  assert(user:file_search_path(datasets,     prasem('Datasets'))),
  assert(user:file_search_path(generic,      prasem('Generics'))),
    assert(user:file_search_path(graph_theory, generic('Graph Theory'))),
    assert(user:file_search_path(math,         generic('Math'))),
    assert(user:file_search_path(owl,          generic('OWL'))),
    assert(user:file_search_path(rdf,          generic('RDF'))),
    assert(user:file_search_path(rdfs,         generic('RDFS'))),
    assert(user:file_search_path(sparql,       generic('SPARQL'))),
  assert(user:file_search_path(standards,    prasem('Standards'))),
  assert(user:file_search_path(vocabularies, prasem('Vocabularies'))),

  % The personal or user directory is relatively difficult to
  % construe, since I only know how to retrieve the user's home
  % path using the XPCE library.
  new(PCEDirectory, directory('~')),
  get(PCEDirectory, path, HomeDirectory),
  assert(user:file_search_path(home, HomeDirectory)).

%% assert_file_types is det.
% Loads the file types that PraSem recognizes based on their extension.
% This is a very limited way of defining file types, i.e. only by file
% extension. The added value is that one file type can have multiple
% file extensions.
% In the far future I could see algorithms for loading, reading,
% visualizing, saving, serializing, and otherwise processing files of
% a specific type.

assert_file_types:-
  assert(user:prolog_file_type(aux,       auxiliary)),
  assert(user:prolog_file_type(bat,       batch)),
  assert(user:prolog_file_type(bmp,       bitmap)),
  assert(user:prolog_file_type(conf,      configuration)),
  assert(user:prolog_file_type(dtd,       dtd)),
  assert(user:prolog_file_type(eps,       encapsulated_postscript)),
  assert(user:prolog_file_type(exe,       executable)),
  assert(user:prolog_file_type(fig,       fig)),
  assert(user:prolog_file_type(gif,       gif)),
  assert(user:prolog_file_type(jar,       java)),
  assert(user:prolog_file_type(mp3,       mp3)),
  assert(user:prolog_file_type(obo,       obo)),
  assert(user:prolog_file_type(ogg,       ogg_vorbis)),
  assert(user:prolog_file_type(out,       output)),
  assert(user:prolog_file_type(owl,       web_ontology_language_1)),
  assert(user:prolog_file_type(owl2,      web_ontology_language_2)),
  assert(user:prolog_file_type(owlapi,    owlapi)),
  assert(user:prolog_file_type(owlapi(F), owlapi(F))),
  assert(user:prolog_file_type(owlms,     owlms)),
  assert(user:prolog_file_type(owlpl,     prolog)),
  assert(user:prolog_file_type(owlx,      owlx)),
  assert(user:prolog_file_type(owlxml,    owlx)),
  assert(user:prolog_file_type(pl,        prolog)),
  assert(user:prolog_file_type(plsyn,     plsyn)),
  assert(user:prolog_file_type(plain,     plain_text)),
  assert(user:prolog_file_type(png,       png)),
  assert(user:prolog_file_type(pro,       prolog)),
  assert(user:prolog_file_type(prolog,    prolog)),
  assert(user:prolog_file_type(ps,        postscript)),
  assert(user:prolog_file_type(qlf,       quick_load_file)),
  assert(user:prolog_file_type(qsim,      qsim)),
  assert(user:prolog_file_type(sh,        shell)),
  assert(user:prolog_file_type(svg,       scalable_vector_graphics)),
  assert(user:prolog_file_type(tex,       latex)),
  assert(user:prolog_file_type(tif,       tag_image_file_format)),
  assert(user:prolog_file_type(tiff,      tag_image_file_format)),
  assert(user:prolog_file_type(tmp,       temporary)),
  assert(user:prolog_file_type(toc,       table_of_contents)),
  assert(user:prolog_file_type(txt,       text)),
  assert(user:prolog_file_type(wav,       wave)),
  assert(user:prolog_file_type(xml,       xml)).

%% create_directories is det.
% Creates the various user directories that are (obviously) not part of
% the static codebase.
% The important part is that some or all of these directories may be
% there already.

create_directories:-
  assert(user:file_search_path(personal, home('.PraSem'))),
  absolute_file_name(home('.PraSem'), PersonalDirectory),
  create_directory(PersonalDirectory),

  % Logging results directory.
  assert(user:file_search_path(log, personal('Log'))),
  absolute_file_name(personal('Log'), LogDirectory),
  create_directory(LogDirectory),

  % Data files directory.
  assert(user:file_search_path(data, personal('Data'))),
  absolute_file_name(personal('Data'), DataDirectory),
  create_directory(DataDirectory),

  % ILP data files directory.
  assert(user:file_search_path(data_ilp, data('ILP'))),
  absolute_file_name(data('ILP'), ILP_Directory),
  create_directory(ILP_Directory),

  % Newsgroups data files.
  assert(user:file_search_path(data_newsgroups, data('Newsgroups'))),
  absolute_file_name(data('Newsgroups'), Newsgroups_Directory),
  create_directory(Newsgroups_Directory),

  % Standards-supporting data files.
  assert(user:file_search_path(data_standards, data('Standards'))),
  absolute_file_name(data('Standards'), StandardsDirectory),
  create_directory(StandardsDirectory),

  % STCN data files directory.
  assert(user:file_search_path(data_stcn, data('STCN'))),
  absolute_file_name(data('STCN'), STCN_Directory),
  create_directory(STCN_Directory),

  % Wordnet data files directory.
  assert(user:file_search_path(data_wordnet, data('Wordnet'))),
  absolute_file_name(data('Wordnet'), WordnetDirectory),
  create_directory(WordnetDirectory),
  
  % Debug directory
  assert(user:file_search_path(debug, personal('Debug'))),
  absolute_file_name(personal('Debug'), DebugDirectory),
  create_directory(DebugDirectory).

%% load_libraries is det.
% Loads the libraries that are required before anything else can run.

load_libraries:-
  ensure_loaded(library(pce)),
  ensure_loaded(library(semweb/rdf_db)).

%% load_modules is det.
% Loads the modules that are required before anything else can run.

load_modules:-
  ensure_loaded(generic(file_ext)),
  ensure_loaded(generic(logging)),
  ensure_loaded(generic(meta_ext)),
  ensure_loaded(generic(os_ext)).

%rdf_register_prefix(atms, 'http://www.wouterbeek.com/prasem/atms.owl#'),
%rdf_register_prefix(environment, 'http://www.wouterbeek.com/prasem/environment.owl#'),
%rdf_register_prefix(ile, 'http://www.wouterbeek.com/ile/ile.owl#'),
%rdf_register_prefix(justification, 'http://www.wouterbeek.com/prasem/justification.owl#'),
%rdf_register_prefix(node, 'http://www.wouterbeek.com/prasem/node.owl#'),
%rdf_register_prefix(prasem, 'http://www.wouterbeek.com/prasem/prasem.owl#'),

%% load is det.
% Loads the PraSem generic infrastructure.
% This is the only correct way to use this file from the outside.
% This calls all other predictes in this file in the corrent order.

load:-
  % Change the setting for user-defined flags to be in accordance with the
  % ISO standard. User-defined flags should be added using
  % create_prolog_flag/3 instead.
  set_prolog_flag(user_flags, error),
  
  assert_file_types,
  assert_file_search_paths,
  load_libraries,
  load_modules,
  check_prolog_version,

  % Make sure Prolog has the correct Operating System set.
  set_os_flag,
  % Do not write module loads to the standard output stream.
  set_prolog_flag(verbose_load, silent),

  create_directories.

:- load.
