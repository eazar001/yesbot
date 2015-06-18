
%% Extension designed to fetch news and download updates from swi-prolog.org


:- module(news,
     [ news/1
      ,news_abort/0 ]).

:- use_module(dispatch).
:- use_module(info).
:- use_module(config).
:- use_module(submodules/html).
:- use_module(submodules/web).
:- use_module(submodules/utils).
:- use_module(library(sgml)).
:- use_module(library(http/http_open)).
:- use_module(library(xpath)).
:- use_module(library(uri)).
:- use_module(library(solution_sequences)).
:- use_module(library(persistency)).

:- dynamic current_day/1.

:- persistent
     heading(headline:string).

:- persistent
     version(type:atom, number:string).

:- persistent
     kjv_quote(quote:string).


target("##prolog", "yesbot").
news_link("http://www.swi-prolog.org/news/archive").
version_link(stable, "http://www.swi-prolog.org/download/stable/src/").
version_link(development, "http://www.swi-prolog.org/download/devel/src/").
news_time_limit(3600). % Time limit in seconds


news(Msg) :-
  ignore(news_trigger(Msg)).


news_trigger(Msg) :-
  with_mutex(news_db,
    (  db_attach('extensions/news.db', []),
       ignore(news_(Msg))
    )
  ).


%% news_(Msg:compound) is semidet.
%
% True if the message is a join message to the specified channel, then run a
% persistent thread in the background that checks for news and download updates.
% The extension will be removed for the duration of the program and will be
% loaded on restart.

news_(Msg) :-
  target(Chan, _),
  Msg = msg(_Prefix, "JOIN", [Chan]),
  setting(config:extensions, Es),
  selectchk(news, Es, Update),
  retractall(info:extensions(_,_)),
  retractall(info:sync_extensions(_,_)),
  set_setting(config:extensions, Update),
  init_extensions,
  set_setting(config:extensions, Es),
  thread_create(news_loop, _, [alias(news), detached(true)]).



%% news_loop is det.
%
% Check the news feed on startup and evalute for side effects. Check the news feed
% every hour for updates. This predicate is always true.

news_loop :-
  news_time_limit(Limit),
  ignore(news_feed),
  get_time(T1),
  stamp_date_time(T1, DateTime, local),
  date_time_value(day, DateTime, Day),
  asserta(current_day(Day)),
  news_check(T1, Limit).


%% news_check(+T1:float, +Limit:integer) is det.
news_check(T1, Limit) :-
  sleep(0.05),
  compare_days,
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


%% news_feed is semidet.
%
% Attempt to scan swi-prolog.org news archive for updates and display to channel.
news_feed :-
  target(Chan, _),
  news_link(Link),
  ignore(fetch_news(Link, Chan)),
  ignore(fetch_version),
  ignore(fetch_king_james).


%% fetch_news(+Link:string, +Chan:string) is semidet.
fetch_news(Link, Chan) :-
  setup_call_cleanup(
    http_open(Link, Stream, [timeout(20)]),
    valid_post(Stream, Chan, Link),
    close(Stream)
  ).


%% fetch_version is det.
fetch_version :-
  ignore(get_latest_version(stable)),
  ignore(get_latest_version(development)).


%% fetch_king_james is semidet.
%
% Get the latest quote from the KJV site and pass the Quote to fetch_kv_quote/1.
fetch_king_james :-
  setup_call_cleanup(
    http_open("http://kingjamesprogramming.tumblr.com/", Stream, []),
    (  load_html(Stream, Structure, []),
       xpath_chk(Structure, //blockquote(normalize_space), Content),
       fetch_kjv_quote(Content)
    ),
    close(Stream)
  ).


%% fetch_kjv_quote(+Content:atom) is det.
%
% Analyzes quotes, and only displays quotes that have not yet been displayaed.
fetch_kjv_quote(Content) :-
  target(Chan, _),
  atom_string(Content, Quote),
  (
     % Found a stored quote
     kjv_quote(Q)
  ->
     (
        Q \= Quote
     ->
	% Current quote is not equal to stored quote
	% Therefore delete the old one and store the new one
	% Display it to the channel
	retract_kjv_quote(Q),
	assert_kjv_quote(Quote),  
	priv_msg(Quote, Chan)
     ;
	% Current quote is the same as the stored one
	% Therefore succeed and don't do anything
        true
     )
  ;
     % No stored quote found
     % Therefore store the new quote and display to channel
     assert_kjv_quote(Quote),
     priv_msg(Quote, Chan)
  ).
  

%% valid_post(+Stream, +Chan:string, +Link:string) is semidet.
%
% Run IO side effects for all valid posts. Valid posts are posts that are
% determined to match the current day of the month for this year.

valid_post(Stream, Chan, Link) :-
  count_valid_posts(Stream, Count, Content),
  forall(
    limit(Count, xpath(Content, //h2(@class='post-title', normalize_space), H)),
    (  atom_string(H, Heading),
       \+heading(Heading),
       assert_heading(Heading),
       format(string(Report), "News Update: ~s", [Heading]),
       send_msg(priv_msg, Report, Chan),
       sleep(5)
    )
  ),
  Count > 0,
  send_msg(priv_msg, Link, Chan).


%% count_valid_posts(+Stream, -Count:integer, -Content) is det.
%
% Count the total amount of valid posts (match today's date) and unify content
% with the parsed html.

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


%% compare_days is semidet.
%
% Get current day stored in the database and compare it to the system's realtime
% representation of the current day. If they are unequal then assert the systems
% representation; nothing should be done otherwise. NOTE: this predicate should
% be deterministic, but is not because current_day/1 is dynamic. Therefore an
% error should be thrown if there is failure. So for now, unti this is resolved,
% this predicate should be marked as semidet. This predicate also retracts all
% headings from the persistent database daily, for cleanup/maintenence purposes.

compare_days :-
  current_day(Current),
  get_time(Time),
  stamp_date_time(Time, DateTime, local),
  date_time_value(day, DateTime, Day),
  (  Current = Day
  -> true
  ;  retractall(current_day(_)),
     asserta(current_day(Day)),
     db_sync(gc),
     retractall_heading(_)
  ).


%% get_latest_version(+Type:atom) is semidet.
%
% Attempt to retrieve latest software version of Type (either stable or devel)
% and display the result via IO side effects.

get_latest_version(Type) :-
  target(Chan, _),
  version_link(Type, Link),
  latest_version(Link, Version),
  format(string(Update),
    "New ~a build available for download: version ~s", [Type, Version]),
  (
     \+version(Type, Version)
  ->
     retractall_version(Type, _),
     assert_version(Type, Version),
     send_msg(priv_msg, Update, Chan),
     (  Type = stable
     -> send_msg(priv_msg, "http://www.swi-prolog.org/download/stable", Chan)
     ;  send_msg(priv_msg, "http://www.swi-prolog.org/download/devel", Chan)
     )
  ;
     true
  ).


%% latest_version(+Link:string, -Version:string) is semidet.
%
% Parse a link and attempt to extract the last (latest version) file that is
% available for download in the appropriate formatted table in the swi site.

latest_version(Link, Version) :-
  setup_call_cleanup(
    http_open(Link, Stream, [timeout(20)]),
    load_html(Stream, Content, []),
    close(Stream)
  ),
  findall(File,
    (  xpath(Content, //tr(@class=Parity), Row),
       memberchk(Parity, [odd,even]),
       xpath(Row, //a(normalize_space), File)
    ),
    Files
  ),
  String_is_num = (\Str^number_string(_, Str)),
  split_string(last $ Files, "-.", "", Latest),
  include(String_is_num, Latest, Nums),
  atomic_list_concat(Nums, ., Atom),
  atom_string(Atom, Version).


%% news_abort is det.
%
% Kill the thread when no longer needed. Exposed in the API.
news_abort :-
  core:thread_signal(news, throw(abort)).


