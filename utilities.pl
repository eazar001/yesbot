
%% Utilities module for anything that might be generally useful for Yesbot


%% NOTE : There are probably some SWI library alternatives that will beat at
%% least some of these utilities out. Investigate later.

:- module(utilities,
	  [ has_link/4
	   ,get_title/3 ]).


%--------------------------------------------------------------------------------%
% IRC/Private Mesages
%--------------------------------------------------------------------------------%


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


%--------------------------------------------------------------------------------%
% html
%--------------------------------------------------------------------------------%


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
