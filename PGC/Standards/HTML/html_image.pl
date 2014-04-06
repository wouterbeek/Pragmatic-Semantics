:- module(
  html_image,
  [
    html_image_thumbnail//5, % +DivOptions:list(nvpair)
                             % +Alt:atom
                             % +Width:float
                             % +Height:float
                             % +File:atom
    html_image_thumbnail_box//5, % +DivOptions:list(nvpair)
                                 % +Description:atom
                                 % +Width:float
                                 % +Height:float
                                 % +File:atom
    html_image_thumbnail_box_grid//5 % +Columns:positive_integer
                                     % +Rows:positive_integer
                                     % +Width:float
                                     % +Height:float
                                     % +Pairs:list(pair)
  ]
).

/** <module> HTML image

Support for the HTML image tag.

@author Wouter Beek
@version 2012/09-2013/06, 2013/10, 2014/03
*/

:- use_module(generics(db_ext)).
:- use_module(generics(option_ext)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_path)).
:- use_module(library(option)).
:- use_module(math(dimension)).
:- use_module(os(file_ext)).
:- use_module(os(image_ext)).

% Register the supported image file types.
% These are shared with module RDF_DATATYPE.
:- dynamic(user:image_file_type/1).
:- multifile(user:image_file_type/1).

:- db_add_novel(user:prolog_file_type(jpeg, jpeg)).
:- db_add_novel(user:prolog_file_type(jpg,  jpeg)).
:- db_add_novel(user:image_file_type( jpeg      )).
:- db_add_novel(user:prolog_file_type(png,  png )).
:- db_add_novel(user:image_file_type( png       )).



%! html_image_thumbnail(
%!   +DivOptions:list(nvpair),
%!   +Alt:atom,
%!   +Width:float,
%!   +Height:float,
%!   +File:atom
%! )// is det.

html_image_thumbnail(DivO1, Alt, Width2, Height2, File) -->
  {
    % Make sure the file has a supported image file type.
    file_type(Type, File),
    user:image_file_type(Type),
    absolute_file_name(data(.), DataDir, [access(read),file_type(directory)]),
    relative_file_path(File, DataDir, RelativeFile),
    http_absolute_location(img(RelativeFile), RelativeUri, []),
    absolute_file_name(img(RelativeFile), AbsoluteFile, [access(read)]),
    image_dimensions(AbsoluteFile, Width1, Height1),
    (
      var(Width2),
      var(Height2)
    ->
      Width3 = Width1,
      Height3 = Height1
    ;
      dimension_scale([Width1,Height1], [Width2,Height2], [Width3,Height3])
    ),
    format(atom(Style), 'height: ~a; width: ~a;', [Height3,Width3]),
    merge_options([class=image_thumbnail,style=Style], DivO1, DivO2)
  },
  % Make the image clickable, linking to the image file.
  html(
    div(
      DivO2,
      a([href=RelativeUri,target='_blank'],
        img([alt=Alt,height=Height3,src=RelativeUri,width=Width3], [])
      )
    )
  ).


%! html_image_thumbnail_box(
%!   +DivOptions:list(nvpair),
%!   +Description:atom,
%!   +Width:float,
%!   +Height:float,
%!   +File:atom
%! ) is det.
% Generates an HTML thumbnail box for an image.

html_image_thumbnail_box(DivO1, Description, Width, Height, File) -->
  % Construe the DIV containing the image, the link, and the description.
  html(
    div(class=image_thumbnail_box, [
      \html_image_thumbnail(DivO1, Description, Width, Height, File),
      \html_image_thumbnail_caption(Description)
    ])
  ).


%! html_image_thumbnail_box_grid(
%!   +Columns:positive_integer,
%!   +Rows:positive_integer,
%!   +Width:float,
%!   +Height:float,
%!   +Pairs:list(pair)
%! )// is det.
% Generates an HTML grid of boxes containing image thumbnails.

html_image_thumbnail_box_grid(Columns, Rows, Width, Height, Pairs) -->
  html(
    div(
      class=image_thumbnail_box_grid,
      \html_image_thumbnail_box_rows(Columns, Rows, Width, Height, Pairs)
    )
  ).


%! html_image_thumbnail_box_row(
%!   +Columns:positive_integer,
%!   +Width:float,
%!   +Height:float,
%!   +Pairs1:list(pair)
%!   -Pairs2:list(pair)
%! )// is det.
% Generates an HTML row of boxes containing image thumbnails.
% The rows are part of a grid.

% No more pairs: the last row may not have the full length.
html_image_thumbnail_box_row(_, _, _, [], []) --> !.
% No more columns: the row is full, return the remaining pairs,
% for use in any further rows.
html_image_thumbnail_box_row(0, _, _, Pairs, Pairs) --> !.
html_image_thumbnail_box_row(
  Columns1,
  Width,
  Height,
  [Description-File|Pairs1],
  Pairs2
) -->
  {Columns2 is Columns1 - 1},
  html([
    \html_image_thumbnail_box([], Description, Width, Height, File),
    \html_image_thumbnail_box_row(Columns2, Width, Height, Pairs1, Pairs2)
  ]).


%! html_image_thumbnail_box_rows(
%!   +Columns:positive_integer,
%!   +Rows:positive_integer,
%!   +Width:float,
%!   +Height:float,
%!   +Pairs:list(pair)
%! )// is det.
% Generates HTML rows of boxes containing image thumbnails.
% The rows are part of a grid.

% No more pairs: no further rows will be generated.
html_image_thumbnail_box_rows(_, _, _, _, []) --> !.
% No more rows. The remaining pairs are discarded.
html_image_thumbnail_box_rows(_, 0, _, _, _) --> !.
html_image_thumbnail_box_rows(Columns, Rows1, Width, Height, Pairs1) -->
  {Rows2 is Rows1 - 1},
  html([
    div(class=image_row,
      \html_image_thumbnail_box_row(Columns, Width, Height, Pairs1, Pairs2)
    ),
    \html_image_thumbnail_box_rows(Columns, Rows2, Width, Height, Pairs2)
  ]).


%! html_image_thumbnail_caption(+Caption:atom)// is det.
% Generates an HTML image caption.

html_image_thumbnail_caption(Caption) -->
  % The DIV containing the image description.
  html(div(class=image_caption, Caption)).

