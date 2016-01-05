%% Extension designed to fetch news and download updates from swi-prolog.org

:- module(news,
     [ news/1
      ,news_abort/0 ]).

:- use_module(library(irc_client)).
:- use_module(library(lambda)).
:- use_module(library(func)).
:- use_module(submodules/html).
:- use_module(submodules/web).
:- use_module(submodules/utils).
:- use_module(library(sgml)).
:- use_module(library(http/http_open)).
:- use_module(library(http/http_client)).
:- use_module(library(http/json)).
:- use_module(library(xpath)).
:- use_module(library(uri)).
:- use_module(library(solution_sequences)).
:- use_module(library(persistency)).

:- dynamic current_date/1.
:- dynamic current_day/1.
:- dynamic line_status/1.

:- persistent
     heading(headline:string).

:- persistent
     version(type:atom, number:string).

:- persistent
     kjv_quote(quote:string).

:- persistent
     commit(comm:string).

:- persistent
     issue(state:string, number:integer).


% TBD: Use the foundation of this module to prime the state infrastructure


target("##prolog", "yesbot").
news_link("http://www.swi-prolog.org/news/archive").
version_link(stable, "http://www.swi-prolog.org/download/stable/src/").
version_link(development, "http://www.swi-prolog.org/download/devel/src/").
kjv_link("http://kingjamesprogramming.tumblr.com/").
swi_commit_link("https://api.github.com/repos/SWI-Prolog/swipl-devel/commits").
swi_issue_link("https://api.github.com/repos/SWI-Prolog/swipl-devel/issues?state=all").
news_time_limit(3600). % Time limit in seconds


news(Msg) :-
  catch(once(thread_property(news, _)), _E, ignore(news_trigger(Msg))).


news_trigger(Msg) :-
  with_mutex(news_db,
    (  db_attach('extensions/news.db', []),
       ignore(news_(Msg))
    )
  ).


%% update_line_status(+Id, +Status) is det.
%
%  If swi-prolog line is down then retract it to signify that it is now up.
%  A message is displayed to the channel that the line is now up. If the line
%  status is already up, then assert it as down. A corresponding message is also
%  sent to the channel regarding this state.

update_line_status(_Me, Status) :-
  \+line_status(_),
  (  Status = up,
     asserta(line_status(up))
  ;  Status = down,
     asserta(line_status(down))
  ), !.

update_line_status(_Me, up) :-
  line_status(Status),
  (  Status = down
  -> retractall(line_status(_)),
     asserta(line_status(up))
  ;  Status = up
  -> true
  ;  asserta(line_status(up))
  ), !.

update_line_status(_Me, down) :-
  line_status(Status),
  (  Status = up
  -> retractall(line_status(_)),
     asserta(line_status(down))
  ;  Status = down
  -> true
  ;  asserta(line_status(down))
  ).


%% news_(+Msg) is semidet.
%
%  True if the message is a ping message to the bot, then run a persistent
%  thread in the background that checks for news and download updates.

news_(Me-Msg) :-
  Msg = msg("PING", _, _),
  thread_create(news_loop(Me), _, [alias(news), detached(true)]).


%% news_loop(+Me) is det.
%
%  Check the news feed on startup and evalute for side effects. Check the news feed
%  every hour for updates. This predicate is always true.

news_loop(Me) :-
  news_time_limit(Limit),
  get_time(T1),
  stamp_date_time(T1, DateTime, local),
  date_time_value(day, DateTime, Day),
  date_time_value(date, DateTime, Date),
  (  current_day(_)
  -> retractall(current_day(_)),
     asserta(current_day(Day))
  ;  asserta(current_day(Day))
  ),
  (  current_date(_)
  -> retractall(current_date(_)),
     asserta(current_date(Date))
  ;  asserta(current_date(Date))
  ),
  ignore(news_feed(Me, Date)),
  news_check(Me, T1, Limit).


%% news_check(+Id, +T1:float, +Limit:integer) is det.
news_check(Me, T1, Limit) :-
  sleep(0.05),
  compare_days,
  get_time(T2),
  Delta is T2 - T1,
  (  Delta >= Limit,
     current_date(Date)
  -> ignore(news_feed(Me, Date)),
     get_time(T0),
     news_check(Me, T0, Limit)
  ;  news_check(Me, T1, Limit)
  ).


%% news_feed(+Id, +Date) is det.
%
%  Attempt to scan swi-prolog.org news archive for updates and display to channel.
news_feed(Me, Date) :-
  target(Chan, _),
  news_link(Link),
  catch(ignore(fetch_news(Me, Link, Chan)),
    _E, ignore(update_line_status(Me, down))),
  (  \+ line_status(down)
  -> ignore(fetch_version(Me))
  ;  true
  ),
  ignore(fetch_king_james(Me)),
  ignore(fetch_swi_commit(Me,Date)),
  ignore(fetch_swi_issue(Me)).


%% fetch_news(+Id, +Link:string, +Chan:string) is semidet.
fetch_news(Me, Link, Chan) :-
  setup_call_cleanup(
    http_open(Link, Stream, [timeout(20)]),
    valid_post(Me, Stream, Chan, Link),
    close(Stream)
  ).


%% fetch_version(+Id) is det.
fetch_version(Me) :-
  ignore(get_latest_version(Me, stable)),
  ignore(get_latest_version(Me, development)).


%% fetch_king_james(+Id) is semidet.
%
%  Get the latest quote from the KJV site and pass the Quote to fetch_kv_quote/1.
fetch_king_james(Me) :-
  kjv_link(Link),
  setup_call_cleanup(
    http_open(Link, Stream, []),
    (  load_html(Stream, Structure, []),
       xpath_chk(Structure, //blockquote(normalize_space), Content),
       fetch_kjv_quote(Me, Content)
    ),
    close(Stream)
  ).


%% fetch_kjv_quote(+Id, +Content:atom) is det.
%
%  Analyzes quotes, and only displays quotes that have not yet been displayed.
fetch_kjv_quote(Me, Content) :-
  target(Chan, _),
  atom_string(Content, Quote),
  (  % Found a stored quote
     kjv_quote(Q)
  -> (  Q \= Quote
     ->	% Current quote is not equal to stored quote
        % Therefore delete the old one and store the new one
        % Display it to the channel
        retract_kjv_quote(Q),
        assert_kjv_quote(Quote),
        priv_msg(Me, Quote, Chan)
      ; % Current quote is the same as the stored one
        % Therefore succeed and don't do anything
        true
      )
  ;   % No stored quote found
      % Therefore store the new quote and display to channel
      assert_kjv_quote(Quote),
      priv_msg(Me, Quote, Chan)
  ).


%% fetch_swi_commit(+Id, +Date) is semidet.
%
%  Access swi commits on github using the JSON API. Then print commits.
fetch_swi_commit(Me, Date) :-
  swi_commit_link(Link),
  setup_call_cleanup(
    http_open(Link, Stream, []),
    json_read_dict(Stream, Array),
    close(Stream)
  ),
  print_swi_commit(Me, Array, Date).


%% print_swi_commit(+Id, +Array, +Date) is failure.
%
%  Print commits only for this day. Only prints commits that haven't been printed.
print_swi_commit(Me, Array, Date) :-
  member(Dict, Array),
  is_dict(Dict),
  parse_time(Dict.commit.committer.date, Stamp),
  stamp_date_time(Stamp, DateTime, local),
  date_time_value(date, DateTime, Date),
  Msg = Dict.commit.message,
  \+commit(Msg),
  assert_commit(Msg),
  target(Chan, _),
  format(string(MsgLine), "swipl-devel commit: ~s~n", [Msg]),
  format(string(Url),"~s", [Dict.html_url]),
  git_io(Url, Shortened),
  priv_msg(Me, MsgLine, Chan, [at_most(7)]),
  sleep(1),
  priv_msg(Me, Shortened, Chan, [at_most(7)]),
  sleep(1),
  priv_msg(Me, " ", Chan),
  sleep(5),
  fail.


%% fetch_swi_issue(+Id) is semidet.
%
%  Access swi issues on github using the JSON API. Then print issues.
fetch_swi_issue(Me) :-
  swi_issue_link(Link),
  setup_call_cleanup(
    http_open(Link, Stream, []),
    json_read_dict(Stream, Array),
    close(Stream)
  ),
  print_swi_issue(Me, Array).


%% print_swi_issue(+Id, +Array) is failure.
%
%  Print issue opens and closes. Only issue events that haven't been printed.
print_swi_issue(Me, Array) :-
  member(Dict, Array),
  is_dict(Dict),
  % If pull_request is not a key then P is null (normal issue)
  catch(P = Dict.pull_request, _E, P = null),
  handle_swi_issue(Me, P, Dict).


handle_swi_issue(Me, null, Dict) :-
  Args = [Dict.html_url, Dict.title, Dict.body],
  S = Dict.state,
  N = Dict.number,
  handle_stored_issue(Me, S, N, "swipl-devel issue ", Args),
  fail.

handle_swi_issue(Me, Paragraph, Dict) :-
  Paragraph \= null,
  Args = [Dict.html_url, Dict.title, Dict.body],
  S = Dict.state,
  N = Dict.number,
  handle_stored_issue(Me, S, N, "swipl-devel pull-request ", Args),
  fail.


handle_stored_issue(Me, State, N, Title, Args) :-
  target(Chan, _),
  (  issue(Stored, N)
  -> (  State \= Stored
     ->	% If the issue has been mentioned in channel, but the state has changed
        % retract the issue, and assert it with the new state, while also
        % mentioning the issue again.
        retract_issue(Stored, N),
        assert_issue(State, N),
        format(string(Report), "~s[~s]~n~s~n~s~n~s", [Title,State|Args]),
        priv_msg(Me, Report, Chan, [at_most(7)]),
        sleep(1),
        priv_msg(Me, " ", Chan),
        sleep(5)
     ;	% Do nothing if the issue has been mentioned and the state is identical
        true
     )
  ;  State = "open"
  -> % If the issue hasn't been mentioned and the state is open
     % assert the issue and mention it in the channel
     format(string(Report), "~s[~s]~n~s~n~s~n~s", [Title,State|Args]),
     assert_issue(State, N),
     priv_msg(Me, Report, Chan, [at_most(7)]),
     sleep(1),
     priv_msg(Me, " ", Chan),
     sleep(5)
  ;  % If the issue hasn't been mentioned and has a closed state, do nothing.
     true
  ).


%% valid_post(+Id, +Stream, +Chan:string, +Link:string) is semidet.
%
%  Run IO side effects for all valid posts. Valid posts are posts that are
%  determined to match the current day of the month for this year.

valid_post(Me, Stream, Chan, Link) :-
  update_line_status(Me, up),
  count_valid_posts(Stream, Count, Content),
  forall(
    limit(Count, xpath(Content, //h2(@class='post-title', normalize_space), H)),
    (  atom_string(H, Heading),
       \+heading(Heading),
       assert_heading(Heading),
       format(string(Report), "News Update: ~s~n ", [Heading]),
       priv_msg(Me, Report, Chan),
       sleep(5)
    )
  ),
  Count > 0,
  send_msg(Me, priv_msg, Link, Chan).


%% count_valid_posts(+Stream, -Count:integer, -Content) is det.
%
%  Count the total amount of valid posts (match today's date) and unify content
%  with the parsed html.

count_valid_posts(Stream, Count, Content) :-
  load_html(Stream, Content, []),
  get_time(Stamp2),
  aggregate_all(count,
    (  xpath(Content, //span(@class=date, normalize_space), Text),
       parse_time(Text, Stamp1),
       stamp_date_time(Stamp1, Dt1, local),
       stamp_date_time(Stamp2, Dt2, local),
       date_time_value(date, Dt1, Same),
       date_time_value(date, Dt2, Same)
    ),
    Count
  ).


%% compare_days is semidet.
%
%  Get current day stored in the database and compare it to the system's realtime
%  representation of the current day. If they are unequal then assert the systems
%  representation; nothing should be done otherwise. NOTE: this predicate should
%  be deterministic, but is not because current_day/1 is dynamic. Therefore an
%  error should be thrown if there is failure. So for now, unti this is resolved,
%  this predicate should be marked as semidet. This predicate also retracts all
%  headings from the persistent database daily, for cleanup/maintenence purposes.

compare_days :-
  current_day(Current),
  get_time(Time),
  stamp_date_time(Time, DateTime, local),
  date_time_value(day, DateTime, Day),
  date_time_value(date, DateTime, Date),
  (  Current = Day
  -> true
  ;  retractall(current_day(_)),
     retractall(current_date(_)),
     asserta(current_date(Date)),
     asserta(current_day(Day)),
     db_sync(gc),
     retractall_heading(_),
     retractall_commit(_),
     retractall_issue("closed", _)
  ).


%% get_latest_version(+Id, +Type:atom) is semidet.
%
%  Attempt to retrieve latest software version of Type (either stable or devel)
%  and display the result via IO side effects.

get_latest_version(Me, Type) :-
  target(Chan, _),
  version_link(Type, Link),
  latest_version(Link, Version),
  format(string(Update),
    "New ~a build available for download: version ~s", [Type, Version]),
  (  \+version(Type, Version)
  -> retractall_version(Type, _),
     assert_version(Type, Version),
     send_msg(Me, priv_msg, Update, Chan),
     (  Type = stable
     -> send_msg(Me, priv_msg, "http://www.swi-prolog.org/download/stable", Chan)
     ;  send_msg(Me, priv_msg, "http://www.swi-prolog.org/download/devel", Chan)
     ),
     priv_msg(Me, " ", Chan)
  ;  true
  ).


%% latest_version(+Link:string, -Version:string) is semidet.
%
%  Parse a link and attempt to extract the last (latest version) file that is
%  available for download in the appropriate formatted table in the swi site.

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
  Split_file = (\F^S^split_string(F, "-.", "", S)),
  String_num = (\Str^Num^number_string(Num, Str)),
  maplist(Split_file, Files, Split),
  maplist(include(String_is_num), Split, NumStrings),
  maplist(maplist(String_num), NumStrings, Nums),
  msort(Nums, Sorted),
  last(Sorted, Latest),
  atomic_list_concat(Latest, ., Atom),
  atom_string(Atom, Version).


%% git_io(+Link:string, -Shortened:string) is det.
%
%  Shorten a git link. If the shortening service fails, then Link will be unified
%  with Shortened.

git_io(Link, Shortened) :-
  catch(git_io_(Link, Shortened), _Error, Shortened=Link).

git_io_(Link, Shortened) :-
  http_post("http://git.io/", form([url=Link]), _Reply,
    [timeout(20), reply_header(Header)]),
  memberchk(location(Short), Header),
  atom_string(Short, Shortened).


%% news_abort is det.
%
%  Kill the thread when no longer needed. Exposed in the API.
news_abort :-
  core:thread_signal(news, throw(abort)).
