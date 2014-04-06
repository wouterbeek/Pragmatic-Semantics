:- module(latex_db, []).

/** <module> LaTeX database

@author Wouter Beek
@version 2014/03
*/

:- use_remote_module(generics(db_ext)).



:- db_add_novel(user:prolog_file_type(tex, latex)).

