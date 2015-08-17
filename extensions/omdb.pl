
:- module(omdb, [omdb/1]).

:- use_module(dispatch).
:- use_module(parser).
:- use_module(library(http/json)).
:- use_module(library(uri)).
:- use_module(library(http/http_open)).
:- use_module(library(list_util)).
:- use_module(library(func)).
:- use_module(library(dcg/basics)).
:- use_module(submodules/utils).

:- dynamic session/1.
:- dynamic session/2.

target("#testeazarbot", "dead_weight_bot").


omdb(Msg) :-
  Msg = msg(Prefix, "PRIVMSG", _Target, Text),
  prefix_id(Prefix, Nick, _, _),
  append(`?movie `, Q0, Text),
  determine_recipient(omdb, Msg, Recipient),
  (  session(Nick)
  -> retractall(session(Nick, _))
  ;  asserta(session(Nick))
  ),
  atom_codes(Q, Q0),
  normalize_space(codes(Req), Q),
  get_params(Req, Args),
  % Remember to split this into Query and option Year later
  format(string(API),
    "http://www.omdbapi.com/?t=~s&y=~s&plot=full&tomatoes=true&r=json", Args),
  setup_call_cleanup(
    http_open(API, Stream, [timeout(20)]),
    json_read_dict(Stream, Dict),
    close(Stream)
  ),
  decode(Dict, Recipient).


% TBD: refactor this

get_params(Req, [Title, Year]) :-
  % Remove the first `[` bracket
  phrase(string_without([91], T0), Req, [_|Rest]),
  split_string(Rest, "]", "", [Year|_]),
  string_codes(T, T0),
  uri_encoded(query_value, T, Title).

get_params(Req, [Title, ""]) :-
  % No year requested
  phrase(string_without([91], T0), Req),
  string_codes(T, T0),
  uri_encoded(query_value, T, Title).
  

decode(Dict, Recipient) :-
  Dict.'Response' = "False",
  priv_msg(Dict.'Error', Recipient).

decode(Dict, Recipient) :-
  Dict.'Response' = "True",
  priv_msg(Dict.'Title', Recipient),
  priv_msg(Dict.'Year', Recipient),
  priv_msg(Dict.'Plot', Recipient).

  