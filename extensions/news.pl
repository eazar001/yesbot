
:- module(news, [news/1]).

:- use_module(dispatch).
:- use_module(submodules/html).
:- use_module(submodules/web).
:- use_module(submodules/utils).
:- use_module(library(sgml)).
:- use_module(library(http/http_open)).
:- use_module(library(xpath)).
:- use_module(library(uri)).


target("#testeazarbot", "eazarbot").

news(Msg) :-
  thread_create(ignore(news_(Msg)), _Id, [detached(true)]).


news_(Msg) :-
  target(Chan, _),
  Msg = msg(_Prefix, "JOIN", [Chan]),
  thread_create(news_feed, _, [alias(news), detached(true)]).


news_feed :-
  http_open('http://www.swi-prolog.org/news/archive', Stream, [timeout(20)]),
  load_html(Stream, Content, []),
  xpath_chk(Content, //h2(@class='post-title', normalize_space), Heading),
  writeln(Heading).


