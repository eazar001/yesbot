
:- module(news, [news/1]).

:- use_module(dispatch).
:- use_module(submodules/html).
:- use_module(submodules/web).
:- use_module(submodules/utils).
:- use_module(library(sgml)).
:- use_module(library(http/http_open)).
:- use_module(library(xpath)).
:- use_module(library(uri)).
:- use_module(library(persistency)).


target("#testeazarbot", "eazarbot").

news(Msg) :-
  thread_create(ignore(news_(Msg)), _Id, [detached(true)]).


%% news_(Msg:compound) is semidet.
%
% True if the message is a join message to the specified channel, then run a
% persistent thread in the background that checks for news and download updates.
% The extension will be removed for the duration of the program and will be
% loaded on restart.

news_(Msg) :-
  target(Chan, _),
  Msg = msg(_Prefix, "JOIN", [Chan]),
  thread_create(news_feed, _, [alias(news), detached(true)]),
  setting(config:extensions, Es),
  selectchk(news, Es, Update),
  retractall(core:extensions(_,_)),
  set_setting(config:extensions, Update),
  core:init_extensions,
  set_setting(config:extensions, Es).


news_feed :-
  target(Chan, _),
  http_open('http://www.swi-prolog.org/news/archive', Stream, [timeout(20)]),
  load_html(Stream, Content, []),
  xpath_chk(Content, //h2(@class='post-title', normalize_space), Heading),
  send_msg(priv_msg, atom_string $ Heading, Chan).


