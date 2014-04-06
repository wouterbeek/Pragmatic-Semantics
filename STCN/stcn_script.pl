:- module(stcn_script, []).

/** <module> STCN script

This module only contains predicates that have a domain-specific use.
These predicate should be converted to some other module or be removed.

@author Wouter Beek
@see http://www.kb.nl/kbhtml/stcnhandleiding/frames.html
@version 2013/09-2013/10, 2013/12
*/

:- use_module(ap(ap)).
:- use_module(dcg(dcg_generic)).
:- use_module(generics(replace_in_file)).
:- use_module(generics(thread_ext)).
:- use_module(library(archive)).
:- use_module(library(debug)).
:- use_module(library(filesex)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(os(archive_ext)).
:- use_module(os(datetime_ext)).
:- use_module(os(file_ext)).
:- use_module(rdf(rdf_build)).
:- use_module(rdf(rdf_graph_name)).
:- use_module(rdf_term(rdf_literal)).
:- use_module(rdf_term(rdf_literal)).
:- use_module(rdf_file(rdf_serial)).
:- use_module(stcn(collect_lines)).
:- use_module(stcn(stcn_generic)).
:- use_module(stcn(stcn_parse)).
:- use_module(stcn(stcn_schema)).
:- use_module(stcn(stcn_scrape)).
:- use_module(stcn(stcn_void)).
:- use_module(void(void_file)).



stcn_script:-
  ap(
    [process(db_create),project(stcn),to('VoID',turtle)],
    [
      ap_stage([to(output,'STCNV',turtle)], stcn_schema),
      ap_stage(
        [from(input,'redactiebladen.txt',archive),to(_,redactiebladen,text)],
        extract_archive
      ),
      ap_stage(
        [
          from(input,'PicartaTopics.ttl',archive),
          to(output,'PicartaTopics',turtle)
        ],
        extract_archive
      ),
      ap_stage(
        [from(_,redactiebladen,text),to(_,redactiebladen,text)],
        redactiebladen_prepare
      ),
      ap_stage(
        [from(_,redactiebladen,text),to(_,'Redactiebladen',turtle)],
        redactiebladen_parse
      ),
      ap_stage(
        [from(_,'Redactiebladen',turtle),to(_,'PicartaPublications',turtle)],
        picarta_scrape_publications
      ),
      ap_stage(
        [
          from(_,'PicartaPublications',turtle),
          to(_,'PicartaPublications',turtle)
        ],
        picarta_scrape_publications_split
      ),
      ap_stage(
        [
          from(_,'PicartaPublications',turtle),
          to(_,'PicartaPublications',turtle)
        ],
        picarta_scrape_publications_ppns
      ),
      ap_stage(
        [
          from(_,'PicartaPublications',turtle),
          to(_,'PicartaPublications',turtle)
        ],
        picarta_scrape_publications_topics
      ),
      ap_stage([from(_,'PicartaPublications',turtle)], picarta_scrape_others),
      ap_stage([to(output,'VoID',turtle)], assert_stcn_void)
    ]
  ).

assert_stcn_void(_PS, _FromDir, ToFile):-
  G = 'VoID',
  stcn_void(G),
  void_save(G, ToFile).

% Picarta scraping for publications.
picarta_scrape_publications(_PS, FromFile, ToFile):-
  file_to_graph_name(FromFile, FromG),
  rdf_load([format(turtle)], FromG, FromFile),

  file_to_graph_name(ToFile, ToG),
  stcn_scrape(FromG, 'Publication', ToG),
  debug(
    stcn_script,
    'Done scraping the redactiebladen for publications.',
    []
  ),

  rdf_save([format(turtle)], ToG, ToFile),
  debug(stcn_script, 'Done scraping Picarta for publications.', []),
  rdf_unload_graph(ToG).

picarta_scrape_publications_split(_, FromFile, ToFile):-
  file_to_graph_name(FromFile, Graph),
  rdf_load([format(turtle)], Graph, FromFile),

  % Assert occurrences in literal enumerations as separate triples.
  rdf_update_literal(_, picarta:printer_publisher, _, _, _, Graph,
      split_lexical_form('; ')
  ),
  debug(stcn_script, 'Printer-publishers were split.', []),
  
  rdf_update_literal(_, picarta:printer_publisher, _, _, _, Graph,
      lexical_form(strip_atom([' ']))
  ),
  debug(stcn_script, 'Printer-publishers were stripped.', []),
  
  rdf_update_literal(_, picarta:topical_keyword, _, _, _, Graph,
      split_lexical_form('; ')
  ),
  debug(stcn_script, 'Topics were split.', []),
  
  rdf_update_literal(_, picarta:typographic_information, _, _, _, Graph,
      split_lexical_form(' , ')),
  debug(stcn_script, 'Typographic information was split.', []),

  rdf_save([format(turtle)], G, ToFile),
  debug(
    stcn_script,
    'The Picarta scrape with splits was saved to file ~w.',
    [ToFile]
  ),
  rdf_unload_graph(G).

picarta_scrape_publications_ppns(_PS, FromFile, ToFile):-
  file_to_graph_name(FromFile, G),
  rdf_load([format(turtle)], Graph, FromFile),

  % Turn PPN literals into IRIs.
  forall(
    member(
      P1-Category,
      [
        author-'Author',
        printer_publisher-'PrinterPublisher',
        translator_editor-'TranslatorEditor'
      ]
    ),
    (
      rdf_global_id(picarta:P1, P2),
      forall(
        (
          rdf_string(PublicationPPN, P2, LexicalForm, G),
          % There are both PPN literals and full name literals.
          % Only the former can be turned into URIs.
          dcg_phrase(ppn, LexicalForm)
        ),
        (
          ppn_resource(Category, LexicalForm, PPN),
          rdf_assert(PublicationPPN, P2, PPN, G),
          rdf_retractall_string(PublicationPPN, P2, LexicalForm, G)
        )
      )
    )
  ),

  % Save the processed scrape results.
  rdf_save([format(turtle)], G, ToFile),
  debug(
    stcn_script,
    'Done saving the scraped redactiebladen to file ~w.',
    [ToFile]
  ),
  rdf_unload_graph(G).

picarta_scrape_publications_topics(_PS, FromFile, ToFile):-
  % Load the Picarta publications.
  file_to_graph_name(FromFile, G),
  rdf_load([format(turtle)], Graph, FromFile),

  % Load the Picarta topics.
  absolute_file_name(
    input('PicartaTopics'),
    TopicFile,
    [access(read),file_type(turtle)]
  ),
  rdf_load([format(turtle)], 'PicartaTopics', TopicFile),

  forall(
    rdf_string(PPN, picarta:topical_keyword, TopicLit, G),
    (
      once(rdf_string(TopicPPN, _, TopicLit, G)),
      rdf_assert(PPN, picarta:topical_keyword, TopicPPN, G),
      rdf_retractall_string(PPN, picarta:topical_keyword, TopicLit, G)
    )
  ),

  % Save the processed scrape results.
  rdf_save([format(turtle)], G, ToFile),
  debug(
    stcn_script,
    'Done saving the scraped redactiebladen to file ~w.',
    [ToFile]
  ),
  rdf_unload_graph(G).

% Picarta scraping for authors, printer-publishers, topics, and
% translator-editors.
picarta_scrape_others(_PS, _FromFile, ToDir):-
  absolute_file_name(
    'PicartaAuthors',
    _ToFile,
    [access(read),file_errors(fail),file_type(turtle),relative_to(ToDir)]
  ), !.
picarta_scrape_others(_PS, FromFile, ToDir):-
  % Load the Picarta publications.
  file_to_graph_name(FromFile, FromG),
  rdf_load([format(turtle)], FromG, FromFile),
  forall_thread(
    (
      member(C, ['Author','PrinterPublisher','TranslatorEditor']),
      format(atom(Msg), 'Scraping Picarta for class ~w', [C])
    ),
    (
      atomic_list_concat(['Picarta',C,'s'], ToG),
      stcn_scrape(FromG, C, ToG),
      debug(stcn_script, 'Done scraping class ~w from Picarta.', [C]),
      absolute_file_name(
        ToG,
        ToFile,
        [access(write),file_type(turtle),relative_to(ToDir)]
      ),
      rdf_save([format(turtle)], ToG, ToFile),
      debug(
        stcn_script,
        'Done saving the scraped class ~w from Picarta.',
        [C]
      )
    ),
    stcn_script,
    Msg
  ).

% Parses the redactiebladen.
redactiebladen_parse(_PS, FromFile, ToFile):-
  parse_redactiebladen(FromFile, 'Redactiebladen'),
  debug(stcn_script, 'Done parsing the redactiebladen.', []),

  rdf_save([format(turtle)], 'Redactiebladen', ToFile),
  debug(stcn_script, 'Done saving the parsed redactiebladen.', []).

% Prepares the redactiebladen file.
redactiebladen_prepare(_PS, FromFile, ToFile):-
  collect_lines(FromFile, F1),
  % Perform some in-file replacements.
  trim_spaces(F1, F2),
  replace_in_file(F2, "Â°", "°", F3),
  replace_in_file(F3, "Ãª", "°", ToFile),
  debug(
    stcn_script,
    'Done with preparing the redactiebladen file for scraping.',
    []
  ).

% Asserts the schema for STCN.
stcn_schema(_PS, _FromDir, ToFile):-
  stcn_schema('STCNV'),
  rdf_save([format(turtle)], 'STCNV', ToFile).
