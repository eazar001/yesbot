
%% Submodule for html related utilities

:- module(html,
     [ has_link/4
      ,html_unescape/2
      ,clean_sequence/2
      ,unescape_title/2 ]).


%--------------------------------------------------------------------------------%
% Utilities for IRC/Private Mesages
%--------------------------------------------------------------------------------%

% Illegal chars
illegal(60).
illegal(62).
illegal(34).

%% parse for existence of a link in a sequence of characters

has_link(http, [104,116,116,112,58,47,47|L]) -->
  `http://`, get_link(http, L), !.

has_link(https, [104,116,116,112,115,58,47,47|L]) -->
  `https://`, get_link(https, L), !.
  
has_link(Protocol, L) -->
  [_], has_link(Protocol, L).

get_link(_, []) --> [32|_], !.

get_link(Protocol, [C|L]) -->
  [C], {\+illegal(C)},
  get_link(Protocol, L).

get_link(_, []) --> [].


%--------------------------------------------------------------------------------%
% HTML Escape Codes
%--------------------------------------------------------------------------------%


%% unescape_title(+Title, -T) is det.
%
% If the Title contains escape characters then format the title for appropriate
% viewing by unescaping them. If there are no escape characters, then the
% formatted title is simply the same as the original.

unescape_title(Title, T) :-
  (
     html_unescape(Title, T)
  ->
     true
  ;
     T = Title
  ).


%% html_unescape(+E, -U) is nondet.
%
% Unescape a sequence of escaped HTML.

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
  {
     number_codes(10, D)
  ->
     Dec = 32
  ;
     number_codes(Dec, D)
  }.


escape_sequence_num([]) --> `;`.
escape_sequence_num([C|Cs]) -->
  [C], {\+member(C, [38,35,59])},
  escape_sequence_num(Cs).


%% clean_sequence(+Sequence, -Cleaned) is det.
%
% Remove all irrelevant and invalid sequences in a possibly utf8 encoded title.

clean_sequence(Sequence, Cleaned) :-
  exclude(invalid_utf8, Sequence, Cleaned).

invalid_utf8(Char) :-
  (  between(0, 9, Char), !
  ;  between(11, 31, Char)
  ), !.

invalid_utf8(Char) :-
  Char > 0xffff.


