
:- module(wiki_search, [wiki_search/1]).

:- use_module(library(irc_client)).
:- use_module(library(http/http_open)).
:- use_module(library(http/json)).
:- use_module(library(uri)).
:- use_module(submodules/html).
:- use_module(submodules/utils).

:- dynamic session/1.
:- dynamic session/2.

target("#testeazarbot", "dead_weight_bot").


wiki_search(Msg) :-
  thread_create(ignore(wiki_search_(Msg)), _, [detached(true)]).


% Handle ?wiki <query> requests
wiki_search_(Me-Msg) :-
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
    search(Me, Nick, Rec, Stream),
    close(Stream)
  ), !.  % can only succeed once

% Handle ?more signal to page output
wiki_search_(Me-Msg) :-
  Msg = msg(Prefix, "PRIVMSG", _, Codes),
  prefix_id(Prefix, Nick, _, _),
  determine_recipient(wiki_search, Msg, Rec),
  string_codes(Text, Codes),
  normalize_space(codes(`?more`), Text),
  (  session(Nick)
  -> display(Me, Nick, Rec)
  ;  true
  ), !. % can only succeed once


% Handle anything else that is not a valid request that pertains to wiki_search
wiki_search_(_-msg(Prefix, "PRIVMSG", _, _)) :-
  prefix_id(Prefix, Nick, _, _),
  (  session(Nick)
  -> maplist(retractall, [session(Nick), session(Nick,_)])
  ;  true
  ).


% The search after initiation of a session
search(Me, Nick, Rec, Stream) :-
  catch(
    (  json_read_dict(Stream, Dict, []),
       format(string(Paragraph), "~s", [Dict.query.pages._.extract]),
       format(string(URL), "~s~nhttp://en.wikipedia.org/?curid=~a",
         [Dict.query.pages._.title, Dict.query.pages._.pageid]),
       priv_msg(Me, URL, Rec),
       priv_msg_rest(irc, Paragraph, Rec, Rest, [auto_nl(true), at_most(1)]),
       (  Rest \= []
       -> priv_msg(Me, "You can type ?more for the next line.", Rec)
       ;  priv_msg(Me, "End of output.", Rec)
       ),
       update_session(Nick, Rest)
    ),
    _Error,
    priv_msg(Me, "Oooh damn. There's a little problem with your request, Son.", Rec)).


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


