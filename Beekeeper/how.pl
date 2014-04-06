:- module(how, []).

/** <module> How does DataHives work?

Web page giving a description of the DataHives project.

@author Wouter Beek
@version 2013/11-2013/12
*/

:- use_remote_module(generics(db_ext)).
:- use_remote_module(generics(meta_ext)).
:- use_remote_module(html(html_image)).
:- use_module(library(http/html_head)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_path)).
:- use_remote_module(server(app_ui)). % Uses the default application style.
:- use_remote_module(server(web_modules)).

:- http_handler(root(how), how, []).

user:web_module('How does DataHives work?', how).

% /img
user:file_search_path(img, bk(img)).



how(_Request):-
  reply_html_page(app_style, [], \dh_body).

dh_body -->
  html(
    body([
      h1(id=toc, 'Table of contents'),
      ol([
        li(a(href='#barriers', 'Waiting for the Semantic Web to happen...')),
        li(a(href='#help', 'So... how can DataHives help?')),
        li(a(href='#reasoning', 'Reasoning')),
        li(a(href='#sharing', 'Sharing')),
        li(a(href='#technology', 'Technology used')),
        li(a(href='#more_info', 'More info'))
      ]),
      h1(id=barriers, 'Waiting for the Semantic Web to happen...'),
      p([
        'The Semantic Web has been a great idea since 2001. ',
        'Unfortunately the use of Open and Linked Data is still ',
        'relatively limited. Individuals and institutions depend on ',
        'big corporations such as Google and Facebook in order to ',
        'maintain their data. This has disadvantages in terms of ',
        'privacy and ownership.'
      ]),
      %\quote('The existing Semantic Web architecture is not that Webby. \c
      %        Actually, it still has a lot of the deficiencies \c
      %        of a traditional database environment.'),
      %\quote('The "intelligent agents" people have touted for ages will finally materialize.'),
      \html_image_thumbnail_box(
        [style='float:right;'],
        [style='height: 10cm;'],
        'Overview of a standard deployment situation for Linked Data.',
        'triple_store.png'
      ),
      p([
        'The orginal idea of the Semantic Web was to empower people and ',
        'institutions by allowing them to manage their own data. ',
        'There are three reasons why this has not yet happended ',
        'on a large scale:'
      ]),
      ol([
        li([
          b('Infrastructural & financial barrier '),
          'Existing Semantic Web deployment systems require a server ',
          'infrastructure with continuous uptime. '
        ]),
        li([
          b('Management barrier '),
          'Once you have your triple store up and running, ',
          'it is not straightforward how to go about ',
          i(curating),
          ' and ',
          i(enriching),
          ' your data.'
        ]),
        li([
          b('Sharing barrier '),
          'Once you have an interesting dataset it is not easy to share ',
          'your data with others. ',
          'Triple stores are often accessed via SPARQL endpoints, ',
          'requiring the user to write complicate database-like queries.'
        ])
      ]),
      h1(id='#help', 'So... how can DataHives help?'),
      p(['DataHives allows you to curate and share your Linked Open Data ']),
      ol([
        li('... without you having to set up a server.'),
        li([
          '... without having to be online all of the time ',
          '(just be online when you want to be)'
        ]),
        li('... without you having to write complicated queries')
      ]),
      p('DataHives puts the original data owner in control by letting her decice'),
      ul([
        li('... which knowledge is shared with whom.'),
        li('... what sources are used for dataset enrichment.')
      ]),
      hr([]),
      h1(id=reasoning, 'Reasoning'),
      p([
        'Local agents (blue dots) traverse the local graph ',
        'in order to deduce new information (blue arrows) ',
        'from the graph.'
      ]),
      \html_image_thumbnail_box(
        [],
        [],
        'A graph with locally traversing agents (blue dots) \c
         that have made local inferences (blue arrows).',
        'local_inference.png'
      ),
      h1(id=sharing, 'Sharing'),
      p([
        'Remote agents (red dots) travel across graphs. ',
        'They look for knowledge that is related to knowledge ',
        'that is already present in the home graph (red arrows).'
      ]),
      \html_image_thumbnail_box(
        [],
        [],
        'A graph with locally traversing agents (blue dots) \c
         that have made local inferences (blue arrows).',
        'knowledge_sharing.png'
      ),
      hr([]),
      h1(id=user_groups, 'User groups'),
      h2('Data publishers'),
      p([
        'By setting up DataHives, a data publisher will automatically enrich ',
        'her curated database with triples that are relevant and trusted. ',
        'Uptake of the DataHives system is made easy by the system not requiring ',
        'any changes to existing Linked Datasets and by the fact that its enrichments ',
        'can be kept separate from the original dataset.'
      ]),
      h2('MSB'),
      p([
        'Small organizations, institutions, and maybe individuals ',
        'who want to maintain their own data but do not have the time, ',
        'money, or proficiency to enrich the data themselves. ',
        'Such organizations may have data they would like to disseminate, ',
        'but the data in isolation is not interesting enough for data consumers. '
      ]),
      hr([]),
      h1(id=technology, 'Technology used'),
      p([
        'DataHives is built using only open and standards-compliant ',
        'technology exclusively. ',
        'The Web-based user interface is built in JavaScript and HTML5. ',
        'The communication of agents and data is established by following the ',
        'new WebRTC standard for between-browser communication. ',
        'For data format support the W3C standards for triple representation and ',
        'serialization are used (e.g., N-Triples). ',
        'For calculating the local deductive closure the standards-compliant ',
        'semantics for RDF and RDF Schema is used.'
      ]),
      hr([]),
      h1(id=more_info, 'More info'),
      p('DataHives was developed by:'),
      ul([
        li('Pepijn Kroes'),
        li(a(href='http://www.wouterbeek.com','Wouter Beek (KR&R, VU)')),
        li(a(href='http://www.few.vu.nl/~schlobac/', 'Stefan Schlobach (KR&R, VU)'))
      ]),
      p([
        'DataHives was developed within the context of ',
        'the NWO-funded project ',
        i('Pragmatic Semantics for the Web of Data')
      ])
      %hr([]),
      %\quote('Nice. But who wants to use this?'),
      %h1('Use case 1: MSB and small governmental institutions'),
      %p(['The Rijksmuseum has a Linked Data database of their collection. ',
      %   'This database was built in collaboration with Computer Scientists ',
      %   'over the past decade. ',
      %   'But not every museum is the Rijksmuseum...'
      %]),
      %h1('Use case: individual consumer'),
      %p(['A consumer may wants to share some data, ',
      %  'but is not generally able to run a Semantic Web server. ',
      %  'Currently a consumer can share his data through the services ',
      %  'provided by Internet companies such as Google and Facebook. ',
      %  'They publish their customer\'s  data as Linked Data. ',
      %  'However, the consumer looses ownership of the data ',
      %  'and is no longer able to manage his own data.'
      %]),
      %p(['DataHives gives the ', i(consumer),
      %   ' control over his own data management process, ',
      %   'but without requiring him to be ....'])
    ])
  ).

quote(Text) -->
  html(
    blockquote(
      style='font-family:\'Bookman Old Style\',Bookman,\'URW Bookman L\',serif;font-size:150%',
      [&(laquo), Text, &(raquo)]
    )
  ).

