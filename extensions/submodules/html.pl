
%% Submodule for html related utilities

:- module(html,
	  [ has_link/4
	   ,get_title/3 ]).


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
%% opening and closing title tag variations

title_open(`<title>`).
title_open(`<TITLE>`).
title_open(`<Title>`).

title_close(`</title>`).
title_close(`</TITLE>`).
title_close(`</Title>`).


%% get_title(-D, +S0, -S) is semidet.
%
% Any sequence that contains the opening 'title' tag is consumed. The following
% characters are parsed as the title until the closing tag is reached. The
% characters are stored as a list as they are parsed.

get_title(T) -->
  {title_open(Tag)},
  Tag, title(T), !.

get_title(T) -->
  [_], get_title(T).

%% title(-D, +S0, -S) is semidet.
%
% Consume any sequence containing the closing title tag and succeed by closing
% the list of stored contents that reside in the tag.

title([]) -->
  {title_close(Tag)},
  Tag, !.

title([C|T]) -->
  [C], title(T).
