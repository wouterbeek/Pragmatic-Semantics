:- module(
  rdf_file,
  [
    rdf_file_correct_extension/2, % +FromFile:atom
                                  % -ToFile:atom
    rdf_directory_files/2, % +Directory:atom
                           % -Files:list(atom)
    rdf_merge_directory/4, % +Options:list(nvpair)
                           % +FromDirectory:atom
                           % +ToFile:atom
                           % +SaveOptions:list(nvpair)
    rdf_mime/1, % ?MIME:atom
    rdf_mime_format/2, % ?MIME:atom
                       % ?Format:atom
    rdf_serialization/5 % ?DefaultExtension:oneof([nt,rdf,triples,ttl])
                        % ?DefaultFileType:oneof([ntriples,rdf_xml,turtle])
                        % ?Format:oneof([ntriples,rdf_xml,triples,turtle])
                        % ?MIMEs:list(atom)
                        % ?URL:atom
  ]
).

/** <module> RDF file

Support for RDF files and file types.

@author Wouter Beek
@version 2012/01, 2012/03, 2012/09, 2012/11, 2013/01-2013/06,
         2013/08-2013/09, 2013/11, 2014/01-2014/04
*/

:- use_module(generics(db_ext)).
:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(library(semweb/rdf_db)).
:- use_module(os(dir_ext)).
:- use_module(os(file_ext)).
:- use_module(os(file_mime)).
:- use_module(rdf(rdf_meta)).



%! is_rdf_file(+File:atom) is semidet.
% Succeeds if the given file contains an RDF serialization.

is_rdf_file(File):-
  file_mime(File, MIME),
  rdf_mime(MIME), !.
is_rdf_file(File):-
  file_name_extension(_, Ext, File),
  rdf_extension(Ext, _).


%! rdf_directory_files(+Directory:atom, -RdfFiles:list(atom)) is det.
%! rdf_directory_files(
%!   +Options:list(nvpair),
%!   +Directory:atom,
%!   -RdfFiles:list(atom)
%! ) is det.
% Returns RDF files from the given directory.
% This is based on parsing (the top of) the contents of these files.
%
% @arg Options Passed to directory_files/3.
% @arg Directory The atomic name of a directory.
% @arg RdfFiles A list of atomic file names of RDF files.

rdf_directory_files(Dir, RdfFiles):-
  rdf_directory_files(
    [include_directories(false),include_self(false),recursive(true)],
    Dir,
    RdfFiles
  ).

rdf_directory_files(O1, Dir, RdfFiles):-
  % Retrieve all files.
  directory_files(O1, Dir, Files),
  include(is_rdf_file, Files, RdfFiles).


rdf_extension(Ext, MIME):-
  rdf_serialization(Ext, _, _, [MIME|_], _).


%! rdf_file_correct_extension(+FromFile:atom, -ToFile:atom) is det.

rdf_file_correct_extension(File1, File2):-
  file_mime(File1, Mime),
  rdf_serialization(Extension, _, _, Mimes, _),
  memberchk(Mime, Mimes),
  file_alternative(File1, _, _, Extension, File2),
  File1 \== File2, !,
  link_file(File1, File2, symbolic).
rdf_file_correct_extension(File, File).


%! rdf_merge_directory(
%!   +Options:list(nvpair),
%!   +FromDirectory:atom,
%!   +ToFile:atom,
%!   +SaveOptions:list(nvpair)
%! ) is det.

rdf_merge_directory(O1, FromDir, ToFile, SaveOptions):-
  rdf_directory_files(FromDir, FromFiles),
  FromFiles \== [],
  rdf_setup_call_cleanup(O1, FromFiles, rdf_graph, SaveOptions, ToFile).


rdf_mime(MIME):-
  rdf_serialization(_, _, _, MIMEs, _),
  member(MIME, MIMEs).


%! rdf_mime_format(+MIME:atom, +Format:atom) is semidet.
%! rdf_mime_format(+MIME:atom, -Format:atom) is det.
%! rdf_mime_format(-MIME:atom, +Format:atom) is det.
% Relates RDF media content types and RDF formats.

rdf_mime_format(MIME, Format):-
  rdf_serialization(_, _, Format, MIMEs, _),
  memberchk(MIME, MIMEs).


%! rdf_serialization(
%!   ?DefaultExtension:oneof([nt,rdf,triples,ttl]),
%!   ?FileType:oneof([ntriples,rdf_xml,triples,turtle]),
%!   ?Format:oneof([ntriples,xml,triples,turtle]),
%!   ?MIME:list(atom),
%!   ?URL:atom
%! ) is nondet.
%
% @arg DefaultExtension The default extension of the RDF serialization.
%      RDF serializations may have multiple non-default extensions,
%      e.g. =owl= and =xml= for RDF/XML.
% @arg DefaultFileType The default file type of the RDF serialization.
%      Every file type has the non-default file type =rdf=.
% @arg Format The format name that is used by the Semweb library.
% @arg MIMEs A list of MIME types.
% @arg URL The URL at which the serialization is described, if any.

rdf_serialization(nq, nquads, nquads, ['application/n-quads'], '').
rdf_serialization(nt, ntriples, ntriples, ['application/n-triples'], 'http://www.w3.org/ns/formats/N-Triples').
rdf_serialization(rdf, rdf_xml, xml, ['application/rdf+xml'], 'http://www.w3.org/ns/formats/RDF_XML'  ).
rdf_serialization(trig, trig, trig, ['application/x-trig'], 'http://wifo5-03.informatik.uni-mannheim.de/bizer/trig/').
rdf_serialization(ttl, turtle, turtle, ['application/x-turtle','text/turtle'], 'http://www.w3.org/ns/formats/Turtle'   ).
rdf_serialization(n3, n3, turtle, ['text/n3'], '').

% RDF file types.
:- db_add_novel(user:prolog_file_type(nq,      nquads  )).
:- db_add_novel(user:prolog_file_type(nq,      rdf     )).
:- db_add_novel(user:prolog_file_type(nt,      ntriples)).
:- db_add_novel(user:prolog_file_type(nt,      rdf     )).
:- db_add_novel(user:prolog_file_type(owl,     rdf_xml )).
:- db_add_novel(user:prolog_file_type(owl,     rdf     )).
:- db_add_novel(user:prolog_file_type(rdf,     rdf_xml )).
:- db_add_novel(user:prolog_file_type(rdf,     rdf     )).
:- db_add_novel(user:prolog_file_type(rdfs,    rdf_xml )).
:- db_add_novel(user:prolog_file_type(rdfs,    rdf     )).
:- db_add_novel(user:prolog_file_type(triples, triples)).
:- db_add_novel(user:prolog_file_type(triples, rdf     )).
:- db_add_novel(user:prolog_file_type(ttl,     turtle  )).
:- db_add_novel(user:prolog_file_type(ttl,     rdf     )).
:- db_add_novel(user:prolog_file_type(xml,     rdf_xml )).
:- db_add_novel(user:prolog_file_type(xml,     rdf     )).

