
:- module(dict, [dict/1]).

:- use_module(dispatch).
:- use_module(library(http/http_open)).
:- use_module(library(http/http_client)).
%:- use_module(library(http/json)).
:- use_module(library(uri)).
:- use_module(library(xpath)).
:- use_module(library(uri)).
:- use_module(submodules/html).
:- use_module(submodules/utils).

target("##prolog", "yesbot").

dict(Msg) :-
  thread_create(ignore(dict_(Msg)), _Id, [detached(true)]).


dict_(Msg) :-
  Msg = msg(_Prefix, "PRIVMSG", _, Text),
  determine_recipient(dict, Msg, Recip),
  is_question(Text, Query),
  format(string(Link),
    "http://dictionary.reference.com/browse/~a?s=t",
    [uri_encoded(query_value) $ Query]),
  setup_call_cleanup(
    http_open(Link, Stream, [timeout(20), status_code(_)]),
    do_search(Link, Stream, Query, Recip),
    close(Stream)
  ).


is_question(Text, Query) :-
  append(`?dict `, Q0, Text),
  string_codes(Q1, Q0),
  normalize_space(string(Query), Q1).
/*
is_question(Text, Query) :-
  append(Q0, `?`, Text),
  string_codes(Query, Q0).
*/

do_search(Link, Stream, _Term, Recip) :-
  dict_search(Link, Stream, Recip).

/*
do_search(_, _, Term, Recip) :-
  urban_search(Term, Recip).
*/

do_search(_, _, _, Recip) :-
  send_msg(priv_msg, "No matches found", Recip).


dict_search(Link, Stream, Recip) :-
  load_html(Stream, Content, []),
  xpath_chk(Content, //div(@class='def-content', normalize_space), P),
  maplist(change, atom_codes $ P, Paragraph),
  send_msg(priv_msg, Link, Recip),
  send_msg(priv_msg, Paragraph, Recip), !.

/*
urban_search(Term, Recip) :-
  format(string(Link),
    "http://api.urbandictionary.com/v0/define?term=~s", [Term]),
  
  http_get(Link, Reply, [timeout(20), status_code(_)]),
  atom_json_dict(Reply, Dict, []),
  memberchk(First, Dict.list),
  send_msg(priv_msg, First.permalink, Recip),
  send_msg(priv_msg, maplist(change) $ string_codes $ First.definition, Recip).
*/

