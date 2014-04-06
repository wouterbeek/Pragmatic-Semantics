:- module(
  html_form,
  [
    submission_form//2, % +URL:url
                        % :FormContent
    submit_button//0
  ]
).

/** <module> HTML Form

Support for generating HTML forms.

@author Wouter Beek
@version 2013/11
*/

:- use_module(library(http/html_write)).
:- use_module(library(http/http_path)).

:- html_meta(submission_form(+,html,?,?)).



submission_form(URL, FormContent) -->
  html(
    form([
      action=URL,
      class='pure-form',
      enctype='application/x-www-form-urlencoded',
      method=post
    ], FormContent)
  ).

submit_button -->
  html(
    button([
      class=['pure-button','pure-button-primary'],
      name=submit,
      type=submit,
      value='Submit'
    ], 'Submit')
  ).

