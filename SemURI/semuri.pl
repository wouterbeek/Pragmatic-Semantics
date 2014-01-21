:- module(semuri, []).

/** <module> Semantic URIs

@author Wouter Beek
@version 2014/01
*/

:- use_module(html(html_table)).
:- use_module(http(rfc2616_status_line)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).
:- use_module(semuri(semuri_script)).
:- use_module(server(app_ui)).
:- use_module(server(web_modules)).

:- http_handler(root(semuri), semuri, []).

:- web_module_add('Semantic URIs', semuri).

:- dynamic(row/1).



semuri(_Request):-
  findall(
    Row,
    row(Row),
    Table
  ),
  Header = [
    'Resource',
    'Package',
    'Organization',
    'People',
    'Tags',
    'Download',
    'Archive'
  ],
  reply_html_page(
    app_style,
    title('Semantic URIs'),
    \html_table(
      [caption('The Semantic Web Hoax'),
       cell_dcg(error_and_text),
       header(true),
       indexed(true)],
      [Header|Table]
    )
  ).

error_action(Action) -->
  html(span(class=action, Action)).

error_and_text(error(Formal,Context)) --> !,
  {Formal =.. [ErrorKind|_]},
  html(
    span(class=error, [
      div(class=error_kind, ErrorKind),
      div(class=formal, \error_formal(Formal)),
      \error_context(Context)
    ])
  ).
error_and_text(Text) -->
  html(p(Text)).

error_arity(Arity) -->
  html(span(class=arity, Arity)).

error_context(VAR) -->
  {var(VAR)}, !,
  [].
error_context(context(Module:Name/Arity,Msg)) -->
  html(
    div(class=context, [
      \error_predicate(Module, Name, Arity),
      \error_message(Msg)
    ])
  ).

error_formal(VAR) -->
  {var(VAR)}, !,
  [].
error_formal(domain_error(Type,Term)) --> !,
  html(
    div(class=domain_error, [
      \error_type(Type),
      \error_term(Term)
    ])
  ).
error_formal(existence_error(Type,Term)) --> !,
  html(
    div(class=existence_error, [
      \error_type(Type),
      \error_term(Term)
    ])
  ).
error_formal(http_status(Status)) --> !,
  {'Status-Code'(Status, Reason)},
  html(div(class=http_status, [Status,': ',Reason])).
error_format(io_error(Mode,Stream)) -->
  html(
    div(class=io_error, [
      \error_mode(Mode),
      \error_stream(Stream)
    ])
  ).
error_formal(instantiation_error(Term)) --> !,
  html(
    div(class=instantiation_error,
      \error_term(Term)
    )
  ).
error_formal(permission_error(Action,Type,Term)) --> !,
  html(
    div(class=permission_error, [
      error_action(Action),
      error_type(Type),
      error_term(Term)
    ])
  ).
error_formal(process_error(Program,exit(Status))) --> !,
  html(
    div(class=process_error, [
      \error_program(Program),
      \error_status(Status)
    ])
  ).
error_formal(representation_error(Reason)) --> !,
  html(
    div(class=representation_error,
      \error_reason(Reason)
    )
  ).
error_format(socket_error(Reason)) --> !,
  html(div(class=socket_error, \error_reason(Reason))).
error_formal(syntax_error(Culprit)) --> !,
  html(div(class=syntax_error, Culprit)).
error_formal(timeout_error(Mode,Stream)) --> !,
  html(
    div(class=timeout_error, [
      \error_mode(Mode),
      \error_stream(Stream)
    ])
  ).
error_formal(type_error(Type,Term)) --> !,
  html(
    div(class=type_error, [
      \error_type(Type),
      \error_term(Term)
    ])
  ).

error_functor(Functor) -->
  html(span(class=functor, Functor)).

error_functor_and_arity(Functor, Arity) -->
  html([
    \error_functor(Functor),
    '/',
    \error_arity(Arity)
  ]).

error_message(Msg) -->
  html(span(class=message, Msg)).

error_mode(Mode) -->
  html(span(class=mode, Mode)).

error_module(Module) -->
  html(span(class=module, Module)).

error_predicate(Functor, Arity) -->
  html(
    span(class=predicate,
      \error_functor_and_arity(Functor, Arity)
    )
  ).

error_predicate(Module, Functor, Arity) -->
  html(
    span(class=predicate, [
      \error_module(Module),
      ':',
      \error_functor_and_arity(Functor, Arity)
    ])
  ).

error_program(Program) -->
  html(div(class=program, ['Program: ',Program])).

error_reason(Reason) -->
  html(span(class=reason, Reason)).

error_status(Status) -->
  html(div(class=exit_status, ['Status: ',Status])).

%error_stream(Stream) -->
%  {stream_property(Stream, alias(Alias))}, !,
%  html(span(class=stream, Alias)).
%error_stream(Stream) -->
%  {stream_property(Stream, file_name(FileName))}, !,
%  html(span(class=stream, FileName)).
error_stream(Stream) -->
  {term_to_atom(Stream, Atom)},
  html(span(class=stream, Atom)).

error_term(Term) -->
  {term_to_atom(Term, Atom)},
  html(span(term=term, Atom)).

error_type(Type) -->
  html(span(class=error_type, Type)).

