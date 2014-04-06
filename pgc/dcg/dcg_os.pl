:- module(
  dcg_os,
  [
    newline//0
  ]
).

/** <module> DCG_OS

OS-specific DCG rules.

@author Wouter Beek
@version 2013/07
*/

:- use_remote_module(dcg(dcg_ascii)).
:- use_remote_module(os(os_ext)).



%! newline//
% A newline is a sequence of characters indicating the end of a line of text.
%
% Newlines are OS-dependent:
%   * Mac, Unix
%     A line feed.
%   * Windows
%     A carriage return followed by a line feed.

newline(List, Rest):-
  os_dependent_call(newline(List, Rest)).

:- if((is_apple ; is_unix)).
newline_unix --> line_feed.
:- endif.

:- if(is_windows).
newline_windows --> carriage_return, line_feed.
:- endif.

