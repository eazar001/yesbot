
%% Submodule for html related utilities

:- module(html,
	  [ has_link/4
	   ,get_title/2 ]).


%--------------------------------------------------------------------------------%
% Utilities for IRC/Private Mesages
%--------------------------------------------------------------------------------%


%% NOTE : It is more robust to actually parse these tags from the whole stream
%% using library(SGML). However, the disadvantage there is that you lose the
%% ability to parse lazily and close the connection early. So in a module like
%% this it is probably more beneficial to performance to implement in a way
%% that resembles what is below.

%% parse for existence of a link in a sequence of characters

has_link(http, [104,116,116,112,58,47,47|L]) -->
  `http://`, get_link(http, L), !.

has_link(https, [104,116,116,112,115,58,47,47|L]) -->
  `https://`, get_link(https, L), !.
  
has_link(Protocol, L) -->
  [_], has_link(Protocol, L).

get_link(_, []) --> [32|_], !.

get_link(Protocol, [C|L]) -->
  [C], get_link(Protocol, L).

get_link(_, []) --> [].


%% html


%% get_title(+Msg, -Title) is semidet.
%
% Any sequence that contains the opening 'title' tag is consumed. The following
% characters are parsed as the title until the closing tag is reached. The
% characters are stored as a list as they are parsed.

get_title(Msg, Title) :-
  maplist(to_lower, Msg, Lower),
  get_title_open(Lower, Msg, T, L),
  get_title_close(L, T, Title), !.


%% Return all that follows <title>

get_title_open([60,116,105,116,108,101|R0], [_,_,_,_,_,_|R1], T0, T) :-
  once(get_title_open_(R0, R1, T0, T)).

get_title_open([_|R0], [_|R1], T0, T) :-
  get_title_open(R0, R1, T0, T).


get_title_open_([62|R0], [_|R1], R1, R0).

get_title_open_([_|R0], [_|R1], T0, T) :-
  get_title_open_(R0, R1, T0, T).


%% Return all that precedes </title>
get_title_close([60,47,116,105,116,108,101,62|_], _, []).

get_title_close([_|R0], [R|R1], [R|T0]) :-
  get_title_close(R0, R1, T0).


