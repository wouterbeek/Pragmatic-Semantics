:- module(
  latex_ext,
  [
    bibtex_convert_file/1, % +File:atom
    latex_clean/1, % +File:atom
    latex_clean_directory/1, % +Directory:atom
    latex_code_convert/1, % +File:atom
    latex_convert_directory/1, % +Directory:atom
    latex_convert_file/1, % +File:atom
    latex_convert_file/2 % +File:atom
                         % +To:atom
  ]
).

/** <module> LaTeX extensions

Predicates for handling LaTeX files.

@author Wouter Beek
@version 2013/06, 2013/08, 2014/01
@tbd Update using new methods in DIR_EXT and FILE_EXT, e.g. directory_files/3.
*/

:- use_module(dcg(dcg_ascii)).
:- use_module(dcg(dcg_content)).
:- use_module(dcg(dcg_generic)).
:- use_module(dcg(dcg_replace)).
:- use_module(generics(codes_ext)).
:- use_module(generics(db_ext)).
:- use_module(library(apply)).
:- use_module(library(option)).
:- use_module(library(process)).
:- use_module(library(readutil)).
:- use_module(os(dir_ext)).
:- use_module(os(file_ext)).
:- use_module(os(run_ext)).
:- use_module(pl(pl_control)).

:- db_add_novel(user:prolog_file_type(aux, aux      )).
:- db_add_novel(user:prolog_file_type(aux, latex_out)).
:- db_add_novel(user:prolog_file_type(bib, bibtex   )).
:- db_add_novel(user:prolog_file_type(log, log      )).
:- db_add_novel(user:prolog_file_type(log, latex_out)).
:- db_add_novel(user:prolog_file_type(pdf, pdf      )).
:- db_add_novel(user:prolog_file_type(pdf, latex_out)).
:- db_add_novel(user:prolog_file_type(tex, tex      )).
:- db_add_novel(user:prolog_file_type(tex, latex_in )).



bibtex_convert_file(File):-
  file_name_type(Base, tex, File),
  file_directory_name(File, Dir),
  process_wrapper(bibtex, [Base], [cwd(Dir)]).

%! file_to_latex_title(+PrologFile:atom, -Title:atom) is det.
% Returns the title for the TeX file that is generated based on the given
% Prolog source file.
%
% For the title we choose the Prolog module name.
% Non-module files get their title based on the local file name.
% Note that the module name and the local file name will often be the same
% for module files.
%
% Underscore characters must be escaped in LaTeX.

file_to_latex_title(PrologFile, Title):-
  module_property(Module, file(PrologFile)),

  % Beware for plunit submodules!
  \+ module_property(Module, class(test)), !,

  % Underscores must be escaped in LaTeX.
  dcg_phrase(dcg_replace(`_`, `\\_`), Module, Title).


file_to_latex_title(PrologFile, Local):-
  file_name(PrologFile, _Directory, Local, _Extension).

%! latex(+Command:oneof([begin,end]))// is semidet.
% Succeeds if the codes list starts with a LaTeX commmand.
%
% Currently the commands =|begin(latex)|= and =|end(latex)|= are defined.

latex(Command) -->
  % Allow Prolog multiline commenting.
  (
    forward_slash,
    asterisk
  ;
    ""
  ),
  atom(Command),
  opening_round_bracket,
  atom(latex),
  closing_round_bracket,
  % Allow Prolog multiline commenting.
  (
    asterisk,
    forward_slash
  ;
    ""
  ).

%! latex_clean(+File:atom) is det.
% Cleans the LaTeX output files in the given directory recursively.

latex_clean(File):-
  access_file(File, read), !,
  forall(
    (
      file_type_alternative(File, latex_out, DeleteFile),
      access_file(DeleteFile, write)
    ),
    safe_delete_file(DeleteFile)
  ).

%! latex_clean_directory(+Directory:atom) is det.

latex_clean_directory(Directory):-
  exists_directory(Directory), !,
  directory_files(
    [
      file_types([latex_out]),
      include_directories(false),
      include_self(false),
      recursive(true)
    ],
    Files
  ),
  maplist(safe_delete_file, Files)

%! latex_code_convert(+File:atom) is det.
% @see Wrapper for latex_code_convert/2

latex_code_convert(Local):-
  latex_code_convert(Local, '.').

%!latex_code_convert(+Spec:atom, +Directory:atom) is det.
% @arg Spec Either an absolute file name or a relative file name that can
%           be resolved relative to the given directory.
%           File names may denote files proper or directories.
%           In the of a directory, all the containing files (including
%           subdirectories) are processed as well.

% Process Prolog files.
latex_code_convert(Local, Directory):-
  (
    is_absolute_file_name(Local)
  ->
    PrologFile = Local
  ;
    absolute_file_name(
      Local,
      PrologFile,
      [file_type(prolog),relative_to(Directory)]
    )
  ), !,
  file_alternative(PrologFile, _, _, tex, TeXFile),
  setup_call_cleanup(
    (
      open(PrologFile, read, InStream, [encoding(utf8),type(text)]),
      open(TeXFile, write, OutStream, [encoding(utf8),type(test)]),
      file_to_latex_title(PrologFile, Title),
      write_latex_header(
        OutStream,
        [
          arbitrary_lines([
            '\\newtheorem{convention}{Convention}',
            '\\newtheorem{definition}{Definition}',
            '\\newtheorem{theorem}{Theorem}',
            '\\theoremstyle{definition}',
            '',
            '\\newenvironment{boxdefinition}',
            '  {\\begin{mdframed}\\begin{definition}}',
            '  {\\end{definition}\\end{mdframed}}',
            '',
            '\\setlength{\\parskip}{\\baselineskip}',
            '\\setlength{\\parindent}{0cm}'
          ]),
          author('Wouter Beek'),
          document_attributes(['10pt',a4paper,draft,twocolumn,twoside]),
          packages([amsfonts,amsmath,amsthm,latexsym,listings,mdframed]),
          title(Title)
        ]
      )
    ),
    latex_code_convert(InStream, OutStream, none),
    (
      close(InStream),
      write_latex_footer(OutStream),
      close(OutStream),
      latex_convert_file(TeXFile)
    )
  ).
% Dive into directories.
latex_code_convert(Local, Directory):-
  catch(
    absolute_file_name(
      Local,
      Absolute,
      [file_type(directory), relative_to(Directory)]
    ),
    _Exception,
    fail
  ),
  !,
  directory_files(Absolute, Files),
  maplist(latex_code_convert, Files).
% Skip the rest.
latex_code_convert(_Local, _Directory).

%! latex_code_convert(
%!   +InStream:stream,
%!   +OutStream:stream,
%!   +Mode:oneof([latex,none,prolog])
%! ) is det.

latex_code_convert(InStream, OutStream, Mode):-
  at_end_of_stream(InStream), !,
  if_then(
    Mode == prolog,
    (
      write(OutStream, '\\end{lstlisting}'),
      nl(OutStream)
    )
  ).
latex_code_convert(InStream, OutStream, none):- !,
  read_line_to_codes(InStream, Codes),
  (
    phrase(latex(begin), Codes)
  ->
    Mode = latex
  ;
    Mode = none
  ),
  latex_code_convert(InStream, OutStream, Mode).
latex_code_convert(InStream, OutStream, latex):- !,
  read_line_to_codes(InStream, Codes),
  (
    phrase(latex(end), Codes)
  ->
    nl(OutStream),
    write(OutStream, '\\begin{lstlisting}'),
    LaTeXMode = prolog
  ;
    write_latex_codes_nl(OutStream, Codes),
    LaTeXMode = latex
  ),
  latex_code_convert(InStream, OutStream, LaTeXMode).
latex_code_convert(InStream, OutStream, prolog):- !,
  read_line_to_codes(InStream, Codes),
  (
    phrase(latex(begin), Codes)
  ->
    % LaTeX begin found: end listing.
    write(OutStream, '\\end{lstlisting}'),
    nl(OutStream),
    Mode = latex
  ;
    write_latex_codes_nl(OutStream, Codes),
    Mode = prolog
  ),
  latex_code_convert(InStream, OutStream, Mode).

latex_convert_file(File):-
  exists_directory(File), !,
  latex_convert_directory(File).
latex_convert_file(FromFile):-
  file_directory_name(FromFile, ToDir),
  latex_convert_file(FromFile, ToDir).

latex_convert_file(FromFile, ToDir):-
  % Check arguments.
  is_absolute_file_name(FromFile),
  access_file(FromFile, read),
  access_file(ToDir, write),

  % Exit with an error code when an error is encountered.
  process_wrapper(pdflatex, ['-halt-on-error',FromFile], [cwd(ToDir)]).

latex_convert_directory(From):-
  access_file(From, read),
  directory_files(
    [
      file_types([latex_in]),
      include_directories(false),
      order(lexicographic),
      recursive(true)
    ],
    From,
    Entries
  ),
  maplist(latex_convert_file, Entries).

print_error([]):- !.
print_error(Codes):-
  print_message(warning, latex(error(Codes))).
print_output(Codes, 0):-
  print_message(information, latex(error(Codes))).
print_output(Codes, Status):-
  Status =\= 0,
  print_message(warning, latex(error(Codes))).
prolog:message(latex(error(Codes))) -->
  ['~s'-[Codes]].

process_wrapper(ProcessName, ProcessArguments, ProcessOptions1):-
  merge_options(
    ProcessOptions1,
    [process(PID),stderr(pipe(Error)),stdout(pipe(Out))],
    ProcessOptions2
  ),

  setup_call_cleanup(
    process_create(
      path(ProcessName),
      ProcessArguments,
      ProcessOptions2
    ),
    (
      read_stream_to_codes(Out, OutCodes, []),
      read_stream_to_codes(Error, ErrorCodes, []),
      process_wait(PID, exit(Status))
    ),
    (
      close(Out),
      close(Error)
    )
  ),
  print_error(ErrorCodes),
  print_output(OutCodes, Status),
  exit_code_handler(ProcessName, Status).

write_latex_codes(Stream, Codes1):-
  phrase(dcg_replace(`_`, `\\_`), Codes1, Codes2),
  put_codes(Stream, Codes2).

write_latex_codes_nl(Stream, Codes):-
  write_latex_codes(Stream, Codes),
  nl(Stream).

write_latex_documentclass(Stream, DocumentClass, Options1):-
  atomic_list_concat(Options1, ',', Options2),
  format(Stream, '\\documentclass[~w]{~w}\n', [Options2, DocumentClass]).

write_latex_footer(Stream):-
  % End of document.
  nl(Stream),
  format(Stream, '\\end{document}\n', []).

write_latex_header(Stream, Options):-
  % The document class.
  option(document_attributes(DocumentAttributes), Options, []),
  write_latex_documentclass(Stream, article, DocumentAttributes),
  nl(Stream),

  % Use packages.
  if_then(
    (
      option(packages(Packages), Options),
      Packages \== []
    ),
    (
      maplist(write_latex_package(Stream), Packages),
      nl(Stream)
    )
  ),

  % Arbitary lines, since we cannot cater for *every* possible header setting.
  if_then(
    option(arbitrary_lines(ArbitraryLines), Options),
    (
      maplist(format(Stream, '~w\n'), ArbitraryLines),
      nl(Stream)
    )
  ),

  % Information for the title.
  if_then(
    option(author(Author), Options),
    format(Stream, '\\author{~w}\n', [Author])
  ),
  if_then(
    option(title(Title), Options),
    format(Stream, '\\title{~w}\n', [Title])
  ),
  if_then(
    (
      option(author(_Author1), Options)
    ;
      option(title(_Title1), Options)
    ),
    nl(Stream)
  ),

  % End of header.
  format(Stream, '\\begin{document}\n', []),
  nl(Stream),

  % Display the title.
  if_then(
    (
      option(author(_Author2), Options)
    ;
      option(title(_Title2), Options)
    ),
    (
      format(Stream, '\\maketitle\n', []),
      nl(Stream)
    )
  ).

write_latex_package(Stream, Package):-
  format(Stream, '\\usepackage{~w}\n', [Package]).

