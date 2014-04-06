:- module(app_ui, []).

/** <module> Application UI

The Web UI for the PGC application server.

This provides a simple menu with the loaded Web modules.

# Centering content

Sometimes I want to center content (not just text).

~~~{.css}
margin-left: auto;
margin-right: auto;
width: 50em;
~~~

@author Wouter Beek
@version 2013/11-2014/03
*/

:- use_remote_module(html(html_form)).
:- use_remote_module(html(html_list)).
:- use_module(library(http/html_head)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/js_write)).
:- use_remote_module(server(app_server)). % Make sure there is an application server.
:- use_remote_module(server(web_login)).
:- use_remote_module(server(web_modules)).
:- use_remote_module(server(web_ui)). % Make sure the Web paths are defined.

:- http_handler(root(.), home, []).

:- if(predicate_property(user:debug_mode, visible)).
  :- html_resource(css('pure-debug-0.4.2.css'), []).
  :- html_resource(css('app_ui.css'), [requires([css('pure-debug-0.4.2.css')])]).
:- else.
  :- html_resource(css('pure-min-0.4.2.css'), []).
  :- html_resource(css('app_ui.css'), [requires([css('pure-min-0.4.2.css')])]).
:- endif.

:- multifile(user:head//2).
:- multifile(user:body//2).



user:body(app_style, Content) -->
  html(
    body([
      div([class='pure-g-r',id=layout], [
        \menulink,
        \menu,
        \main(Content)
      ]),
      % Animates the menu.
      \js_script({|javascript(_)||
        (function (window, document) {
          var layout = document.getElementById('layout'),
            menu = document.getElementById('menu'),
            menuLink = document.getElementById('menuLink');
          function toggleClass(element, className) {
            var classes = element.className.split(/\s+/),
              length = classes.length,
              i = 0;
            for(; i < length; i++) {
              if (classes[i] === className) {
              classes.splice(i, 1);
              break;
              }
            }
            // The className is not found
            if (length === classes.length) {
              classes.push(className);
            }
            element.className = classes.join(' ');
          }
          menuLink.onclick = function (e) {
            var active = 'active';
            e.preventDefault();
            toggleClass(layout, active);
            toggleClass(menu, active);
            toggleClass(menuLink, active);
          };
        }(this, this.document));
      |})
    ])
  ).

content(Content) -->
  html(div([class=content,id=content], Content)).

footer -->
  html(
    footer(class=footer,
      div(class=['pure-menu','pure-menu-horizontal','pure-menu-open'],
        ul(li('Developed between 2012/05 and 2014/01 by Wouter Beek.'))
      )
    )
  ).

user:head(app_style, Head) -->
  html(head([\html_requires(css('app_ui.css'))|Head])).

home(_Request):-
  reply_html_page(app_style, [], []).

login -->
  html(
    \submission_form('/',
      fieldset(class='pure-group', [
        input([
          class=text,
          id=username,
          required=required,
          size=10,
          type=text
        ], []),
        input([
          class=text,
          id=password,
          required=required,
          size=10,
          type=password
        ], []),
        \submit_button
      ])
    )
  ).

:- meta_predicate(main(//,?,?)).
main(Content) -->
  html(div([class='pure-u-1',id=main], \content(Content))).

menu -->
  html(
    div([class='pure-u',id=menu],
      div(class=['pure-menu','pure-menu-open'], [
        a([class='pure-menu-heading',href='/'], 'PraSem'),
        \html_web_modules_list
      ])
    )
  ).

menulink -->
  html(a([class='menu-link',href='#menu',id=menuLink], span([]))).

