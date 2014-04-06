:- module(
  email,
  [
    send_email/3 % +To:atom
                 % +Subject:atom
                 % +Body:list(code)
  ]
).

/** <module> E-mail

@author Wouter Beek
@version 2014
*/

:- use_remote_module(generics(codes_ext)).
:- use_module(library(option)).
:- use_module(library(smtp)).



send_email(To, Subject, Body):-
  merge_options(
    [
      auth('wouter.beek@gmail.com'-'Mary_i6683'),
      from('wouter.beek@gmail.com'),
      security(ssl),
      smtp('smtp.gmail.com')
    ],
    [
      subject(Subject),
      to(To)
    ],
    O1
  ),
  smtp_send_mail('me@wouterbeek.com', message(Body), O1).

message(Codes, Stream):-
  put_codes(Stream, Codes).

