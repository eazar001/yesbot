
:- module(dict, [dict/1]).

:- use_module(dispatch).
:- use_module(library(http/http_open)).
:- use_module(library(xpath)).
:- use_module(library(uri)).
:- use_module(submodules/html).
:- use_module(submodules/utils).

target("##prolog", "yesbot").
dict_start(`http://dictionary.reference.com/browse/`).
dict_end(`?s=t`).

dict(Msg) :-
  thread_create(ignore(dict_(Msg)), _Id, [detached(true)]).


dict_(Msg) :-
  Msg = msg(_Prefix, "PRIVMSG", _, Text),
  determine_recipient(dict, Msg, Recip),
  append(`?dict `, Q0, Text),
  string_codes(Q1, Q0),
  normalize_space(atom(Atom), Q1),
  append(atom_codes $ uri_encoded(query_value) $ Atom, Diff, Query),
  dict_start(Start),
  dict_end(End),
  Diff = End,
  string_codes(Link, append(Start) $ Query),
  setup_call_cleanup(
    http_open(Link, Stream, [timeout(20), status_code(_)]),
    dict_search(Link, Stream, Recip),
    close(Stream)
  ).


dict_search(Link, Stream, Recip) :-
  load_html(Stream, Content, []),
  xpath_chk(Content, //div(@class='def-content', normalize_space), P),
  maplist(change, atom_codes $ P, Paragraph),
  send_msg(priv_msg, Link, Recip),
  send_msg(priv_msg, Paragraph, Recip), !.

dict_search(_, _, Recip) :-
  send_msg(priv_msg, "No matches found", Recip).


