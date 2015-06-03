
:- module(omdb, [omdb/1]).

:- use_module(dispatch).
:- use_module(library(http/json)).
:- use_module(library(uri)).
:- use_module(library(http/http_open)).
:- use_module(library(list_util)).
:- use_module(submodules/utils).


target("#testeazarbot", "eazarbot").


omdb(Msg) :-
  Msg = msg(_Prefix, "PRIVMSG", _Target, Text),
  append(`?movie `, Q0, Text),
  atom_codes(Q, Q0),
  normalize_space(string(Req), Q),
  get_params(Req, Args),
  % Remember to split this into Query and option Year later
  format(string(API),
    "http://www.omdbapi.com/?t=~s&y=~s&plot=short&r=json", Args),
  setup_call_cleanup(
    http_open(API, Stream, [timeout(20)]),
    json_read_dict(Stream, Dict),
    close(Stream)
  ),
  determine_recipient(omdb, Msg, Recipient),
  decode(Dict, Recipient).


get_params(Req, [Title, Year]) :-
  split_string(Req, "[]", "", Attrs),
  (
     Attrs = [T],
     Year = ""
  ;
     Attrs = [T, Year|_]
  ),
  uri_encoded(query_value, T, Title).


decode(Dict, Recipient) :-
  Dict.'Response' = "False",
  send_msg(priv_msg, Dict.'Error', Recipient).

decode(Dict, Recipient) :-
  Send = (\Entry^send_msg(priv_msg, Entry, Recipient)),
  Dict.'Response' = "True",
  findnsols(3, Val:Y, Y = Dict.Val, Ys),
  writeln(Ys).
  %send_msg(priv_msg, Dict.'Title', Recipient).



  