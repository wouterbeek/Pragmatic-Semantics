:- module(
  qlf_ext,
  [
    compiled_file/2 % +PL_File:atom
                    % -QLF_File:atom
  ]
).

/** <module> QLF_EXT

Predicates for loading Prolog files in the Quick Load Format (QLF).

@author Wouter Beek
@version 2013/06
*/

:- use_module(library(debug)).
:- use_remote_module(os(file_ext)).



%! compiled_file(PL_File, QLF_File) is det.
% Returns the compiled version of the given Prolog file.
%
% Compiled files are called Quick Load Files (QLF).
%
% This method checks whether the Prolog file needs to be recompiled,
% or whether a previously compiled QLF file can be used instead.
%
% @arg PL_File The atomic name of a Prolog file.
% @arg QLF_File The atomic name of a QLF file.

compiled_file(PL_File, QLF_File):-
  file_name_type(Base, prolog, PL_File),
  file_name_type(Base, quick_load_file, QLF_File),
  (
    exists_file(QLF_File),
    time_file(PL_File, PL_Time),
    time_file(QLF_File, QLF_Time),
    QLF_Time >= PL_Time
  ->
    true
  ;
    access_file(QLF_File, write)
  ->
    qcompile(PL_File)
  ;
    debug(qlf_ext, 'Cannot write to QLF, loading from ~w.', [PL_File])
  ).

