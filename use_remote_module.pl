:- module(
  use_remote_module,
  [
    use_remote_module/2 % +RepositoryId:atom
                        % +ModuleSpec:compound
  ]
).

/** <module> Use remote module

Allows remote Prolog modules to be imported in the same way in which
use_module/1 imports local Prolog modules.

@author Wouter Beek
@version 2014/04
*/

:- use_module(library(filesex)).
:- use_module(library(http/http_open)).
:- use_module(library(http/http_ssl_plugin)).
:- use_module(library(uri)).

%! github_repository(
%!   ?RepositoryId:atom,
%!   ?User:atom,
%!   ?RepositoryName:atom,
%!   ?ProjectDirectory:atom
%! ) is nondet.

:- multifile(user:github_repository/4).
:- dynamic(user:github_repository/4).

:- initialization(init_use_remote_module).

init_use_remote_module:-
  source_file(init_use_remote_module, ThisFile),
  file_directory_name(ThisFile, ThisDirectory),
  assert(user:file_search_path(project, ThisDirectory)),
  assert(user:file_search_path(os, project('OS'))),
  assert(user:github_repository(pgc, wouterbeek, 'PGC', ThisDirectory)).



%! use_remote_module(+Repository:atom, +Module:compound) is det.
% Loads a Prolog module from the Web.
%
% ~~~
% https://github.com/wouterbeek/PGC/raw/master/OS/file_ext.pl
% ~~~

use_remote_module(RepositoryId, ModuleSpec):-
  github_repository(RepositoryId, User, RepositoryName, LocalRelativeTo),
  absolute_file_name(ModuleSpec, LocalPath, [file_type(prolog)]),
  relative_file_name(LocalPath, LocalRelativeTo, RelativePath),
  atomic_list_concat(Components, '/', RelativePath),
  atomic_list_concat(['',User,RepositoryName,raw,master|Components], '/', Path),
  uri_components(Url, uri_components(htts,'github.com',Path,_,_)),
  file_directory_name(LocalPath, LocalDirectory),
  make_directory_path(LocalDirectory),
  setup_call_cleanup(
    http_open(Url, HttpStream, [cert_verify_hook(cert_verify)]),
    setup_call_cleanup(
      open(LocalPath, write, FileStream, [type(binary)]),
      copy_stream_data(HttpStream, FileStream),
      close(FileStream)
    ),
    close(HttpStream)
  ),
  use_remote_module(LocalPath).

cert_verify(_, _, _, _, _):- !.







