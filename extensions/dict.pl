
:- module(dict, [dict/1]).

:- use_module(dispatch).
:- use_module(library(http/http_open)).
:- use_module(library(xpath)).
:- use_module(library(uri)).


chan("##prolog").
dict_start(`http://dictionary.reference.com/browse/`).
dict_end(`?s=t`).

dict(Msg) :-
  thread_create(ignore(dict_(Msg)), _Id, [detached(true)]).


dict_(Msg) :-
  chan(Chan),
  Msg = msg(_Prefix, "PRIVMSG", [Chan], Text),
  append(`?dict `, Q0, Text),
  atom_codes(Atom, Q0),
  uri_encoded(query_value, Atom, Encoded),
  atom_codes(Encoded, Q),
  append(Q, Diff, Query),
  dict_start(Start),
  dict_end(End),
  append(Start, Query, L),
  Diff = End,
  string_codes(Link, L),
  setup_call_cleanup(
    http_open(Link, Stream, [timeout(20)]),
    (
       load_html(Stream, Content, []),
       xpath_chk(Content, //div(@class='def-content', normalize_space), P0),
       atom_codes(P0, P),
       maplist(dict:change, P, Paragraph),
       send_msg(priv_msg, Link, Chan),
       send_msg(priv_msg, Paragraph, Chan)
    ),
    close(Stream)
  ).


change(10, 32).
change(X, 32) :- X < 10.
change(X, 63) :- X > 255.
change(X, X) :- X > 10, X =< 255.
