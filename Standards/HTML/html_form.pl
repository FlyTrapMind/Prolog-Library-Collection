:- module(
  html_form,
  [
    input_ui/1, % -Markup:list
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



%! input_ui(-Markup:list) is det.
% HTML markup for an input form.

input_ui([
  element(
    form,
    [
      action=URI,
      enctype='application/x-www-form-urlencoded',
      method=post
    ],
    [
      element(
        textarea,
        [cols=100, name=web_input, rows=40, type=text, value=''],
        ['']
      ),
      element(
        button,
        [name=submit, type=submit, value='Submit'],
        ['Submit']
      )
    ]
  )
]):-
  http_absolute_location(root(console), URI, []).

submission_form(URL, FormContent) -->
  html(
    form(
      [
        action=URL,
        class='pure-form',
        enctype='application/x-www-form-urlencoded',
        method=post
      ],
      FormContent
    )
  ).

submit_button -->
  html(
    button(
      [
        class=['pure-button','pure-button-primary'],
        name=submit,
        type=submit,
        value='Submit'
      ],
      'Submit'
    )
  ).

