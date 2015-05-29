
:- module(dict, [dict/1]).

:- use_module(dispatch).
:- use_module(library(http/http_open)).
:- use_module(library(http/http_client)).
:- use_module(library(xpath)).
:- use_module(library(uri)).
:- use_module(submodules/html).
:- use_module(submodules/utils).

target("##prolog", "yesbot").

dict(Msg) :-
  ignore(dict_(Msg)).


dict_(Msg) :-
  Msg = msg(_Prefix, "PRIVMSG", _, Text),
  determine_recipient(dict, Msg, Recip),
  is_question(Text, Query),
  format(string(Link),
    "http://dictionary.reference.com/browse/~a?s=t",
    [uri_encoded(query_value) $ Query]),
  setup_call_cleanup(
    http_open(Link, Stream, [timeout(20), status_code(_Code)]),
    do_search(Link, Stream, Query, Recip),
    close(Stream)
  ).


is_question(Text, Query) :-
  append(`?dict `, Q0, Text),
  string_codes(Q1, Q0),
  normalize_space(string(Query), Q1).


do_search(Link, Stream, _Term, Recip) :-
  dict_search(Link, Stream, Recip).

do_search(_, _, _, Recip) :-
  send_msg(priv_msg, "No matches found", Recip).


dict_search(Link, Stream, Recip) :-
  load_html(Stream, Content, []),
  xpath_chk(Content, //div(@class='def-content', normalize_space), P),
  clean_sequence(atom_codes $ P, Paragraph),
  send_msg(priv_msg, Link, Recip),
  send_msg(priv_msg, Paragraph, Recip).


