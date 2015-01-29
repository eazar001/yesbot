
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



%--------------------------------------------------------------------------------%
% Escape Codes
%--------------------------------------------------------------------------------%


html_unescape(E, U) :-
  once(html_unescape(U0, E, [])),
  flatten(U0, U).


%% Unify with an escape sequence of arbitrary length. If there is a failure then
%% parse one single character and continue the sequence recursively.

html_unescape([]) --> [].
html_unescape([C|Cs]) -->
  escape_sequence([C|Cs]),
  html_unescape(Cs).


%% Parse out decimals in html entities

html_unescape([L|Rest]) -->
  escape_sequence_num(L),
  html_unescape(Rest).


%% Parse out special html entities

html_unescape(Cs) -->
  escape_sequence(Cs),
  html_unescape([]).


% Parse a single character

html_unescape([C|Cs]) -->
  [C], {\+member(C, [38,35,59])},
  html_unescape(Cs).


%% HTML escape sequences

escape_sequence([32|_]) --> `&nbsp;`.
escape_sequence([34|_]) --> `&quot;`.
escape_sequence([38|_]) --> `&amp;`.
escape_sequence([60|_]) --> `&lt;`.
escape_sequence([62|_]) --> `&gt;`.


escape_sequence_num([Dec]) -->
  `&#`, escape_sequence_num(D),
  {number_codes(Dec, D)}.


escape_sequence_num([]) --> `;`.
escape_sequence_num([C|Cs]) -->
  [C], {\+member(C, [38,35,59])},
  escape_sequence_num(Cs).


