% Debug file for the PraSem project.

% Indicate to the support modules that we are running in debug mode.
:- assert(user:debug_project).

:- [run].
:- assert(user:file_search_path(test, prasem('Tests'))).

