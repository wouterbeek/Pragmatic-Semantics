:- module(
  html_pl_error,
  [
    html_error_context//1, % +Context:compound
    html_error_formal//1 % +Formal:compound
  ]
).

/** <module> HTML Prolog error

Generates HTML descriptions of Prolog error terms.

@author Wouter Beek
@version 2014/01-2014/03
*/

:- use_module(generics(error_ext)).
:- use_module(http(rfc2616_status_line)).
:- use_module(library(http/html_write)).
:- use_module(pl_web(html_pl_generic)).



html_error_action(Action) -->
  html(div(class=action, Action)).


html_error_context(VAR) -->
  {var(VAR)}, !, [].
html_error_context(context(Module:Name/Arity,Msg)) --> !,
  html(
    div(class=context, [
      \html_predicate(Module, Name, Arity),
      \html_error_message(Msg)
    ])
  ).
html_error_context(Context) -->
  {atom(Context)}, !,
  html(div(class=context, Context)).


html_error_formal(VAR) -->
  {var(VAR)}, !, [].
html_error_formal(domain_error(Type,Term)) --> !,
  html(
    div(class=domain_error, [
      \html_error_type(Type),
      \html_error_term(Term)
    ])
  ).
html_error_formal(existence_error(Type,Term)) --> !,
  html(
    div(class=existence_error, [
      \html_error_type(Type),
      \html_error_term(Term)
    ])
  ).
html_error_formal(http_status(StatusCode)) --> !,
  {'Status-Code'(StatusCode, Reason)},
  html(
    div(class=http_status, [
      \html_error_status_code(StatusCode),
      ': ',
      \html_error_reason(Reason)
    ])
  ).
html_error_formal(io_error(Mode,Stream)) -->
  html(
    div(class=io_error, [
      \html_error_mode(Mode),
      \html_error_stream(Stream)
    ])
  ).
html_error_formal(instantiation_error) --> !,
  html(
    div(class=instantiation_error,
      []
    )
  ).
html_error_formal(instantiation_error(Term)) --> !,
  html(
    div(class=instantiation_error,
      \html_error_term(Term)
    )
  ).
html_error_formal(limit_exceeded(max_errors,Max)) --> !,
  html(
    div(class=limit_exceeded, [
      'Max: ',
      span(class=max_errors, Max)
    ])
  ).
html_error_formal(mime_error(_,MustBe_MIME,Is_MIME)) --> !,
  html(
    span(class=mime_error, [
      'Must be ',
      \html_mime(MustBe_MIME),
      ' not ',
      \html_mime(Is_MIME)
    ])
  ).
html_error_formal(permission_error(Action,Type,Term)) --> !,
  html(
    div(class=permission_error, [
      \html_error_action(Action),
      \html_error_type(Type),
      \html_error_term(Term)
    ])
  ).
html_error_formal(process_error(Program,exit(Status))) --> !,
  html(
    div(class=process_error, [
      \html_program(Program),
      \html_exit_status(Status)
    ])
  ).
html_error_formal(representation_error(Reason)) --> !,
  html(
    div(class=representation_error,
      \html_error_reason(Reason)
    )
  ).
html_error_formal(resource_error(Reason)) --> !,
  html(
    div(class=resource_error,
      \html_error_reason(Reason)
    )
  ).
html_error_formal(socket_error(Reason)) --> !,
  html(
    div(class=socket_error,
      \html_error_reason(Reason)
    )
  ).
html_error_formal(syntax_error(Culprit)) --> !,
  html(
    div(class=syntax_error,
      Culprit
    )
  ).
html_error_formal(timeout_error(Mode,Stream)) --> !,
  html(
    div(class=timeout_error, [
      \html_error_mode(Mode),
      \html_error_stream(Stream)
    ])
  ).
html_error_formal(type_error(Type,Term)) --> !,
  html(
    div(class=type_error, [
      \html_error_type(Type),
      \html_error_term(Term)
    ])
  ).


html_error_message(VAR) -->
  {var(VAR)}, !, [].
html_error_message(Msg) -->
  html(span(class=message, Msg)).


html_error_mode(Mode) -->
  html(span(class=mode, Mode)).


html_error_reason(Reason) -->
  html(div(class=reason, Reason)).


html_error_status_code(StatusCode) -->
  html(span(class=status_code, StatusCode)).


%html_error_stream(Stream) -->
%  {stream_property(Stream, alias(Alias))}, !,
%  html(span(class=stream, Alias)).
%html_error_stream(Stream) -->
%  {stream_property(Stream, file_name(FileName))}, !,
%  html(span(class=stream, FileName)).
html_error_stream(Stream) -->
  {with_output_to(atom(Atom), write_canonical(Stream))},
  html(span(class=stream, Atom)).


html_error_term(Term) -->
  {with_output_to(atom(Atom), write_canonical(Term))},
  html(div(class=term, Atom)).


html_error_type(Type) -->
  html(span(class=error_type, Type)).


html_exit_status(Status) -->
  {exit_code_reason(Status, Reason)},
  html(
    div(class=exit_status, [
      'Status: ',
      span(class=exit_status_code, Status),
      \html_error_reason(Reason)
    ])
  ).


html_mime(MIME) -->
  html(span(class=mime, MIME)).

