
:- module(omdb, [omdb/1]).

:- use_module(library(irc_client)).
:- use_module(library(http/json)).
:- use_module(library(uri)).
:- use_module(library(http/http_open)).
:- use_module(library(list_util)).
:- use_module(library(func)).
:- use_module(library(dcg/basics)).
:- use_module(submodules/utils).

:- dynamic session/1.
:- dynamic session/2.

target("##prolog", "yesbot").

omdb(Msg) :-
  thread_create(ignore(omdb_(Msg)), _, [detached(true)]).


omdb_(Me-Msg) :-
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
  decode(Me, Dict, Nick, Recipient), !.


% Handle ?more signal to page output
omdb_(Me-Msg) :-
  Msg = msg(Prefix, "PRIVMSG", _, Codes),
  prefix_id(Prefix, Nick, _, _),
  determine_recipient(omdb, Msg, Rec),
  string_codes(Text, Codes),
  normalize_space(string("?more"), Text),
  (  session(Nick)
  -> display(Me, Nick, Rec)
  ;  true
  ), !. % can only succeed once

% Handle anything else that is not a valid request that pertains to dict
omdb_(_-msg(Prefix, "PRIVMSG", _, _)) :-
  prefix_id(Prefix, Nick, _, _),
  (  session(Nick)
  -> maplist(retractall, [session(Nick), session(Nick,_)])
  ;  true
  ).


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
  

decode(Me, Dict, _, Recipient) :-
  Dict.'Response' = "False",
  priv_msg(Me, Dict.'Error', Recipient).

decode(Me, Dict, Nick, Recipient) :-
  Dict.'Response' = "True",
  format(string(R0), "~s [~s]~n", [Dict.'Title', Dict.'Year']),
  format(string(R1), "Rotten Tomato Meter: ~s~nIMDB Rating: ~s",
    [Dict.tomatoMeter, Dict.imdbRating]),
  priv_msg(Me, R0, Recipient),
  priv_msg(Me, R1, Recipient),
  sleep(1),
  priv_msg_rest(Me, Dict.'Plot', Recipient, Rest, [at_most(1)]),
  Rest = [_|_],
  priv_msg(Me, "You can type ?more for the next line.", Recipient),
  asserta(session(Nick, Rest)).


display(Me, Nick, Rec) :-
  session(Nick, [Line|Rest]),
  priv_msg(Me, Line, Rec, [auto_nl(false)]),
  (  Rest \= []
  -> priv_msg(Me, "You can type ?more for the next line.", Rec)
  ;  priv_msg(Me, "End of output.", Rec)
  ),
  update_session(Nick, Rest).


update_session(Nick, []) :-
  maplist(retractall, [session(Nick), session(Nick, _)]).

update_session(Nick, Rest) :-
  retractall(session(Nick, _)),
  asserta(session(Nick, Rest)).


