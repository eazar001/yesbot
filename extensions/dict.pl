
:- module(dict, [dict/1]).

:- use_module(dispatch).
:- use_module(library(http/http_open)).
:- use_module(library(http/http_client)).
:- use_module(library(xpath)).
:- use_module(library(uri)).
:- use_module(parser).
:- use_module(submodules/html).
:- use_module(submodules/utils).

:- dynamic session/1.
:- dynamic session/2.

target("##prolog", "yesbot").

dict(Msg) :-
  ignore(dict_(Msg)).

% Handle ?dict <query> requests
dict_(Msg) :-
  Msg = msg(Prefix, "PRIVMSG", _, Text),
  prefix_id(Prefix, Nick, _, _),
  is_question(Text, Query),
  determine_recipient(dict, Msg, Recip),
  (  session(Nick)
  -> retractall(session(Nick,_))
  ;  asserta(session(Nick))
  ),
  format(string(Link),
    "http://dictionary.reference.com/browse/~a?s=t",
    [uri_encoded(query_value) $ Query]),
  setup_call_cleanup(
    http_open(Link, Stream, [timeout(20), status_code(_)]),
    do_search(Link, Stream, Nick, Recip),
    close(Stream)
  ), !. % can only succeed once

% Handle ?more signal to page output
dict_(Msg) :-
  Msg = msg(Prefix, "PRIVMSG", _, Codes),
  prefix_id(Prefix, Nick, _, _),
  determine_recipient(dict, Msg, Rec),
  string_codes(Text, Codes),
  normalize_space(codes(`?more`), Text),
  (  session(Nick)
  -> display(Nick, Rec)
  ;  true
  ), !. % can only succeed once


% Handle anything else that is not a valid request that pertains to dict
dict_(msg(Prefix, "PRIVMSG", _, _)) :-
  prefix_id(Prefix, Nick, _, _),
  (  session(Nick)
  -> maplist(retractall, [session(Nick), session(Nick,_)])
  ;  true
  ).


is_question(Text, Query) :-
  append(`?dict `, Q0, Text),
  string_codes(Q1, Q0),
  normalize_space(string(Query), Q1).


% The search after initiation of a session
do_search(Link, Stream, Nick, Recip) :-
  dict_search(Link, Stream, Nick, Recip).

do_search(_, _, _, Recip) :-
  priv_msg("No matches found", Recip).


dict_search(Link, Stream, Nick, Recip) :-
  load_html(Stream, Content, []),
  findall(Paragraph,
    (  xpath(Content, //div(@class='def-content', normalize_space), P),
       atom_codes(P, Seq),
       clean_sequence(Seq, Paragraph)
    ),
    Paragraphs
  ),
  Paragraphs = [_|_],
  priv_msg(Link, Recip),
  asserta(session(Nick, Paragraphs)),
  display(Nick, Recip).


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


