:- module(
  email,
  [
    send_email/3 % +To:atom
                 % +Subject:atom
                 % :Body
  ]
).

/** <module>

email:login('wouter.beek@gmail.com').
email:smtp('smtp.gmail.com).
email:from('me@wouterbeek.com').

@author Wouter Beek
@version 2014/08
*/

:- use_module(library(settings)).
:- use_module(library(smtp)).

:- use_module(generics(codes_ext)).

:- meta_predicate(send_email(+,+,//)).
:- meta_predicate(body(//,+)).

:- setting(email:login, atom, '', 'The name of the account from which a message is sent.').
:- setting(email:from, atom, '', 'The email address from which a message is sent.').
:- setting(email:password, atom, '', 'The password for the email service.').
:- setting(email:smtp, atom, '', 'The name of the SMTP server.').

:- initialization(load_settings('settings.db')).

:- at_halt(save_settings('settings.db')).



%! send_email(+To:atom, +Subject:atom, :Body) is det.

send_email(To, Subject, Goal):-
  setting(email:from, From),
  setting(email:login, Login),
  setting(email:password, Password),
  setting(email:smtp, Smtp),
  smtp_send_mail(
    From,
    body(Goal),
    [
      auth(Login-Password),
      from(Login),
      security(ssl),
      smtp(Smtp),
      subject(Subject),
      to(To)
    ]
  ).

body(Goal, Stream):-
  once(phrase(Goal, Codes)),
  with_output_to(Stream, put_codes(Codes)).

