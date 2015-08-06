
:- module(wiki_search, [wiki_search/1]).

:- use_module(library(http/http_open)).
:- use_module(library(http/json)).
:- use_module(library(uri)).
:- use_module(dispatch).
:- use_module(submodules/html).
:- use_module(submodules/utils).
:- use_module(parser).

:- dynamic session/1.
:- dynamic session/2.

target("##prolog", "yesbot").


wiki_search(Msg) :-
  ignore(wiki_search_(Msg)).


% Handle ?wiki <query> requests
wiki_search_(Msg) :-
  Msg = msg(Prefix, "PRIVMSG", _, Text),
  append(`?wiki `, Q0, Text),
  prefix_id(Prefix, Nick, _, _),
  determine_recipient(wiki_search, Msg, Rec),
  (  session(Nick)
  -> retractall(session(Nick,_))
  ;  asserta(session(Nick))
  ),
  atom_codes(Atom, Q0),
  uri_encoded(query_value, Atom, Query),
  format(string(Link), "https://en.wikipedia.org/w/api.php?format=json&\c
    action=query&prop=extracts&exintro=&explaintext=&redirects&\c
    titles=~s",[Query]),
  setup_call_cleanup(
    http_open(Link, Stream, [timeout(20)]),
    search(Nick, Rec, Stream),
    close(Stream)
  ), !.  % can only succeed once

% Handle ?more signal to page output
wiki_search_(Msg) :-
  Msg = msg(Prefix, "PRIVMSG", _, Codes),
  prefix_id(Prefix, Nick, _, _),
  determine_recipient(wiki_search, Msg, Rec),
  string_codes(Text, Codes),
  normalize_space(codes(`?more`), Text),
  (  session(Nick)
  -> display(Nick, Rec)
  ;
     true
  ), !. % can only succeed once


% Handle anything else that is not a valid request that pertains to wiki_search
wiki_search_(msg(Prefix, "PRIVMSG", _, _)) :-
  prefix_id(Prefix, Nick, _, _),
  (  session(Nick)
  -> maplist(retractall, [session(Nick), session(Nick,_)])
  ;  true
  ).


% The search after initiation of a session
search(Nick, Rec, Stream) :-
  catch(
    (  json_read_dict(Stream, Dict, []),
       format(string(Paragraph), "~s", [Dict.query.pages._.extract]),
       priv_msg_rest(Paragraph, Rec, Rest, [auto_nl(true), at_most(1)]),
       (  Rest \= []
       -> priv_msg("You can type ?more for the next line.", Rec)
       ;  priv_msg("End of output.", Rec)
       ),
       update_session(Nick, Rest)
    ),
    _Error,
    priv_msg("Oooh damn. There's a little problem with your request, son.", Rec)).


display(Nick, Rec) :-
  session(Nick, [Line|Rest]),
  priv_msg(Line, Rec, [auto_nl(false)]),
  (  Rest \= []
  -> priv_msg("You can type ?more for the next line.", Rec)
  ;  priv_msg("End of output.", Rec)
  ),
  update_session(Nick, Rest).


update_session(Nick, []) :-
  maplist(retractall, [session(Nick), session(Nick, _)]).

update_session(Nick, Rest) :-
  retractall(session(Nick, _)),
  asserta(session(Nick, Rest)).


