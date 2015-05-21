
%% Extension designed to fetch news and download updates from swi-prolog.org


:- module(news,
  [ news/1
   ,news_abort/0 ]).

:- use_module(dispatch).
:- use_module(submodules/html).
:- use_module(submodules/web).
:- use_module(submodules/utils).
:- use_module(library(sgml)).
:- use_module(library(http/http_open)).
:- use_module(library(xpath)).
:- use_module(library(uri)).
:- use_module(library(solution_sequences)).
:- use_module(library(persistency)).


target("#testeazarbot", "eazarbot").
news_link("http://www.swi-prolog.org/news/archive").
time_limit(3600). % Time limit in seconds

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
  thread_create(news_loop, _, [alias(news), detached(true)]),
  setting(config:extensions, Es),
  selectchk(news, Es, Update),
  retractall(core:extensions(_,_)),
  set_setting(config:extensions, Update),
  core:init_extensions,
  set_setting(config:extensions, Es).


%% news_loop is det.
%
% Check the news feed on startup and evalute for side effects. Check the news feed
% every hour for updates. This predicate is always true.

news_loop :-
  time_limit(Limit),
  ignore(news_feed),
  get_time(T1),
  news_check(T1, Limit).


%% news_check(+T1:float, +Limit:integer) is det.

news_check(T1, Limit) :-
  sleep(0.05),
  get_time(T2),
  Delta is T2 - T1,
  (
     Delta >= Limit
  ->
     ignore(news_feed),
     get_time(T0),
     news_check(T0, Limit)
  ;
     news_check(T1, Limit)
  ).

%% news_feed is det.
%
% Attempt to scan swi-prolog.org news archive for updates and display to channel.
news_feed :-
  target(Chan, _),
  news_link(Link),
  setup_call_cleanup(
    http_open(Link, Stream, [timeout(20)]),
    valid_post(Stream, Chan, Link),
    close(Stream)
  ).


%% valid_post(+Stream, +Chan:string, +Link:string) is nondet.
%
% Run io side effects for all valid posts. Valid posts are posts that are
% determined to match the current day of the month for this year.

valid_post(Stream, Chan, Link) :-
  count_valid_posts(Stream, Count, Content),
  forall(
    limit(Count, xpath(Content, //h2(@class='post-title', normalize_space), H)),
    (  atom_string(H, Heading),
       format(string(Report), "News Update: ~s", [Heading]),
       send_msg(priv_msg, Report, Chan),
       sleep(5)
    )
  ),
  (  Count > 0
  -> send_msg(priv_msg, Link, Chan)
  ;  true
  ).


count_valid_posts(Stream, Count, Content) :-
  load_html(Stream, Content, []),
  get_time(Stamp2),
  aggregate_all(count,
  (  xpath(Content, //span(@class=date, normalize_space), Text),
     parse_time(Text, Stamp1),
     stamp_date_time(Stamp1, Dt1, local),
     stamp_date_time(Stamp2, Dt2, local),
     date_time_value(date, Dt1, Same),
     date_time_value(date, Dt2, Same)), Count).


%% news_abort is det.
%
% Kill the thread when no longer needed. Exposed in the API.
news_abort :-
  core:thread_signal(news, throw(abort)).


