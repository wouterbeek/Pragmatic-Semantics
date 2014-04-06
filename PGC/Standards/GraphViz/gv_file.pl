:- module(
  gv_file,
  [
    graph_to_gv_file/3, % +Options:list(nvpair)
                        % +GraphInterchangeFormat:compound
                        % ?ToFile:atom
    graph_to_svg_dom/3, % +Options:list(nvpair)
                        % +GraphInterchangeFormat:compound
                        % -SvgDom:list(compound)
    tree_to_gv_file/3 % +Options:list(nvpair)
                      % +Tree:compound
                      % ?ToFile:atom
  ]
).

/** <module> GV_FILE

Predicates for converting GIF-formatted terms
into GraphViz output files or SVG DOM structures.

Also converts between GraphViz DOT formatted files
and GraphViz output files or SVG DOM structures.

@author Wouter Beek
@version 2011-2013/09, 2013/11-2014/01
*/

:- use_module(generics(codes_ext)).
:- use_module(generics(db_ext)).
:- use_module(generics(error_ext)).
:- use_module(gv(gv_dcg)).
:- use_module(library(option)).
:- use_module(library(process)).
:- use_module(os(file_ext)).
:- use_module(os(run_ext)).
:- use_module(os(safe_file)).
:- use_module(svg(svg_file)).

:- db_add_novel(user:module_uses_program(gv_file, dot)).

:- db_add_novel(user:prolog_file_type(dot,  graphviz       )).
:- db_add_novel(user:prolog_file_type(jpeg, jpeg           )).
:- db_add_novel(user:prolog_file_type(jpeg, graphviz_output)).
:- db_add_novel(user:prolog_file_type(jpg,  jpeg           )).
:- db_add_novel(user:prolog_file_type(jpg,  graphviz_output)).
:- db_add_novel(user:prolog_file_type(pdf,  pdf            )).
:- db_add_novel(user:prolog_file_type(pdf,  graphviz_output)).
:- db_add_novel(user:prolog_file_type(png,  graphviz_output)).
:- db_add_novel(user:prolog_file_type(svg,  graphviz_output)).
:- db_add_novel(user:prolog_file_type(svg,  svg            )).
:- db_add_novel(user:prolog_file_type(xdot, graphviz_output)).
:- db_add_novel(user:prolog_file_type(xdot, xdot           )).



%! graph_to_gv_file(
%!   +Options:list(nvpair),
%!   +GIF:compound,
%!   -ToFile:atom
%! ) is det.
% Returns a file containing a GraphViz visualization of the given graph.
%
% The following options are supported:
%   * =|method(+Method:oneof([dot,sfdp])|=
%     The algorithm used by GraphViz for positioning the tree nodes.
%     Either =dot= (default) or =sfdp=.
%   * =|to_file_type(+FileType:oneof([jpeg,pdf,svg,xdot])|=
%     The file type of the generated GraphViz file.
%
% @arg Options A list of name-value pairs.
% @arg GIF A compound term representing a graph.
% @arg ToFile The atomic name of a file.

graph_to_gv_file(O1, GIF, ToFile):-
  once(phrase(gv_graph(GIF), Codes)),
  to_gv_file(O1, Codes, ToFile).


%! graph_to_svg_dom(
%!   +Options:list(nvpair),
%!   +GraphInterchangeFormat:compound,
%!   -SvgDom:list(compound)
%! ) is det.
% The following options are supported:
%   * =|method(+Method:oneof([dot,sfdp])|=
%     The algorithm used by GraphViz for positioning the tree nodes.
%     Either =dot= (default) or =sfdp=.

graph_to_svg_dom(O1, GIF, SvgDom):-
  % Make sure the file type of the output file is SvgDom.
  merge_options([to_file_type(svg)], O1, O2),
  graph_to_gv_file(O2, GIF, ToFile),
  file_to_svg(ToFile, SvgDom),
  safe_delete_file(ToFile).


%! tree_to_gv_file(+Options:list(nvpair), +Tree:compound, ?ToFile:atom) is det.
% Stores the given tree term into a GraphViz file.
%
% The following options are supported:
%   * =|method(+Method:oneof([dot,sfdp])|=
%     The algorithm used by GraphViz for positioning the tree nodes.
%     Either =dot= (default) or =sfdp=.
%   * =|to_file_type(+FileType:oneof([jpeg,pdf,svg,xdot])|=
%     The file type of the generated GraphViz file.
%
% @arg Options A list of name-value pairs.
% @arg Tree A compound term representing a tree.
% @arg ToFile The atomic name of the generated file.

tree_to_gv_file(O1, Tree, ToFile):-
  once(phrase(gv_tree(O1, Tree), Codes)),
  to_gv_file(O1, Codes, ToFile).



% SUPPORT PREDICATES %

%! convert_gv(+Options:list(nvpair), +FromFile:atom, ?ToFile:atom) is det.
% Converts a GraphViz DOT file to an image file, using a specific
% visualization method.
%
% The following options are supported:
%   * =|method(+Method:oneof([dot,sfdp])|=
%     The algorithm used by GraphViz for positioning the tree nodes.
%     Either =dot= (default) or =sfdp=.
%   * =|to_file_type(+FileType:oneof([jpeg,pdf,svg,xdot])|=
%     The file type of the generated GraphViz file.
%
% @arg Options
% @arg FromFile
% @arg ToFile

convert_gv(O1, FromFile, ToFile):-
  % The input file must be readable.
  access_file(FromFile, read),

  % The method option.
  option(method(Method), O1, dot),
  must_be(oneof([dot,sfdp]), Method),

  % The file type option.
  option(to_file_type(ToFileType), O1, pdf),
  prolog_file_type(ToExtension, ToFileType),
  prolog_file_type(ToExtension, graphviz_output), !,

  % The output file is either given or created.
  (
    var(ToFile)
  ->
    absolute_file_name(
      personal(export),
      ToFile,
      [access(write),file_type(ToFileType)]
    )
  ;
    is_absolute_file_name(ToFile),
    % The given output file must match a certain file extension.
    file_name_extension(_, ToExtension, ToFile)
  ),
  % Now that we have the output file we can prevent the
  % file type / file extension translation predicates from bakctracking.
  !,

  % Run the GraphViz conversion command in the shell.
  format(atom(OutputType), '-T~w', [ToExtension]),
  process_create(
    path(Method),
    [OutputType,FromFile,'-o',ToFile],
    [process(PID)]
  ),
  process_wait(PID, exit(ShellStatus)),
  exit_code_handler('GraphViz', ShellStatus).

%! to_gv_file(+Options:list(nvpair), +Codes:list(code), ?ToFile:atom) is det.
% The following options are supported:
%   * =|method(+Method:oneof([dot,sfdp])|=
%     The algorithm used by GraphViz for positioning the tree nodes.
%     Either =dot= (default) or =sfdp=.
%   * =|to_file_type(+FileType:oneof([jpeg,pdf,svg,xdot])|=
%     The file type of the generated GraphViz file.

to_gv_file(O1, Codes, ToFile):-
  absolute_file_name(
    project(tmp),
    FromFile,
    [access(write),file_type(graphviz)]
  ),
  setup_call_cleanup(
    open(FromFile, write, Out, [encoding(utf8),type(test)]),
    put_codes(Out, Codes),
    close(Out)
  ),
  convert_gv(O1, FromFile, ToFile),

  % DEB: Store DOT file.
  ignore((
    file_type_alternative(ToFile, graphviz, DOT_File),
    safe_copy_file(FromFile, DOT_File)
  )),

  safe_delete_file(FromFile).

