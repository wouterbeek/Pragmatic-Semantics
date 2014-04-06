:- module(
  web_message,
  [
    web_message/1 % +Term
  ]
).

/** <module> Web message

Acts on messages printed by print_message/2.

@author Wouter Beek
@version 2013/02, 2013/04-2013/05, 2013/08-2013/09, 2013/11, 2014/01
*/

:- use_remote_module(dcg(dcg_content)).
:- use_remote_module(generics(logging)).
:- use_remote_module(html(html_table)).
:- use_module(library(aggregate)).
:- use_module(library(csv)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_open)).
:- use_module(library(http/http_path)).
:- use_module(library(settings)).
:- use_remote_module(math(math_ext)).
:- use_remote_module(os(ansi_ext)).
:- use_remote_module(server(email)).
:- use_remote_module(server(web_console)).
:- use_remote_module(server(web_error)).
:- use_remote_module(server(web_modules)).

:- dynamic(current_log_row/1).

:- http_handler(root(msg), web_message, [priority(1)]).

user:web_module('Messages', web_message).

% Enable notification service via email.
:- debug(email).

:- setting(
  max_log_length,
  nonneg,
  100,
  'The maximum number of log items to show.'
).



web_message(_Request):-
  reply_html_page(app_style, title('Logs'), \log_web).

log_web -->
  {setting(max_log_length, Max)},
  log_web(Max).

log_web(_) -->
  {\+ current_log_file(_File)}, !,
  html(p('Logging is currently switched off.')).
log_web(Max) -->
  {
    current_log_file(File),
    aggregate_all(
      set([DateTime,Category,Message]),
      (
        csv_read_file_row(
          File,
          row(DateTime,Category,Message),
          [arity(3),functor(row)]
        )
      ),
      TRs1
    ),
    reverse(TRs1, TRs2),
    length(Top1, Max),
    (
      append(Top1, _, TRs2)
    ->
      Top2 = Top1
    ;
      Top2 = TRs2
    )
  },
  html(
    \html_table(
      [header_row(true),indexed(true)],
      html('Log messages'),
      [['DateTime','Category','Message']|Top2]
    )
  ).

/*
%prolog:debug_print_hook(_Type, 'EXCEPTION', [Exception]):-
%  web_error(Exception, Markup),
%  push(status_pane, html, app_style, Markup), !.

prolog:debug_print_hook(Type, Format, Args):-
  format(atom(Atom), Format, Args),
  format(user_output, '[~w] ~w\n', [Type,Atom]), !.
prolog:debug_print_hook(Type, Format, Args):-
  % Write to the status pane in the Web front-end.
  %format(atom(Msg), Format, Args),
  %push(
  %  status_pane,
  %  html,
  %  app_style,
  %  [element(p,[],['[',Type,']',' ',Msg])]
  %),

  % Write to the terminal.
  %ansi_format(user_output, [bold,fg(green)], '[~w] ', [Type]),
  %ansi_formatnl(user_output, [fg(green)], '~w', [Msg]),

  % Write to the log stream/file.
  append_to_log(Type, Format, Args),

  email(Type, Format, Args).
*/

email(email, Format, Args):- !,
  format(codes(Body), Format, Args),
  send_email('me@wouterbeek.com', 'Message from script', Body).
email(_, _, _).

