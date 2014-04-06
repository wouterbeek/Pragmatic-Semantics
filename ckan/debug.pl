% Project-specific debug file for the CKAN project.


% Debug topics.
:- use_module(library(debug)).
:- debug(ckan).


% Automatic entry of API keys.
:- multifile(user:api_key_hook/2).
user:api_key_hook(datahub_io, '78cecbc1-f84e-47dc-8625-1bff7ac0eca0').

:- use_remote_module(ckan(ckan_lod)).
:- ckan_download_lod(datahub_io).

