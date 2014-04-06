:- module(
  svg_file,
  [
    file_to_svg/2, % +File:atom
                   % -SVG:list
    stream_to_svg/2 % +Stream:stream
                    % -SVG:list
  ]
).

/** <module> SVG file support

@author Wouter Beek
@see SVG 1.1 (Second Edition) http://www.w3.org/TR/2011/REC-SVG11-20110816/
@version 2012/10, 2013/01-2013/09, 2013/11
*/

:- use_module(dcg(dcg_generic)).
:- use_module(generics(db_ext)).
:- use_module(os(os_ext)).



% DTD file location.
user:file_search_path(dtd, svg(.)).

% A special SVG file extension is used on Macintosh HFS file systems.
:- if(is_apple).
:- db_add_novel(user:prolog_file_type('svg ', svg)).
:- endif.
% The default SVG file extension is used on Unix and Windows.
:- if((is_unix ; is_windows)).
:- db_add_novel(user:prolog_file_type(svg, svg)).
:- endif.

% GZipped SVG file extension.
% @tbd How to use these?
%%%%:- db_add_novel(user:prolog_file_type(svgz, gzip)).
%%%%:- db_add_novel(user:prolog_file_type(svgz, svg)).


file_to_svg(File, SVG):-
  setup_call_cleanup(
    open(File, read, In, [encoding(utf8),type(test)]),
    stream_to_svg(In, SVG),
    close(In)
  ).

stream_to_svg(In, SVG):-
  load_structure(
    stream(In),
    SVG,
    [
      dialect(xmlns),
      max_errors(1),
      shorttag(false),
      space(default),
      syntax_errors(quiet)
    ]
  ).
