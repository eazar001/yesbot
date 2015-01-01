%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                                %
%                                                                                %
% Author: Ebrahim Azarisooreh                                                    %
% E-mail: ebrahim.azarisooreh@gmail.com                                          %
% IRC Nick: eazar001                                                             %
% Title: Yes-Bot                                                                 %
% Descripton: IRC Bot Parsing Module                                             %
%                                                                                %
%                                                                                %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- module(parser,
	  [
	    has_link/4
	   ,chat_log/2
	   ,priv_msg/4
	   ,get_title/3
	   ,ping_from/2
	  ]).

%--------------------------------------------------------------------------------%
% irc/ping requests
%--------------------------------------------------------------------------------%


%% ping_from(+S0, -S) is semidet.
%
% Any string that starts with `PING :` will be deemed a ping request. The
% information after successful parse up to this point will contain the origin
% as the difference of the list of characters.

ping_from --> `PING :`.


%--------------------------------------------------------------------------------%
% irc/private mesages
%--------------------------------------------------------------------------------%


%% priv_msg(-Nick, -Recip, +S0, -S) is semidet.
%
% Any string containing `PRIVMSG ` will be deemed a private message. The suffix
% that follows `PRIVMSG ` will be returned as the difference. This difference will
% then be parsed for a whitespaced cushioned sequence of readable characters.
% These characters will be parsed and stored as NICK of the sender. The next
% sequence of the former pattern will be determined the recipient of the message.
% Finally, the actual message will be returned as the difference of the successful
% parse.

priv_msg(Nick, Recip) -->
  `:`, priv_msg(Nick, Recip), !.

priv_msg([], Recip) -->
  `!`, priv_msg_get([], Recip), !.

priv_msg([C|Nick], Recip) -->
  [C], priv_msg(Nick, Recip).

priv_msg_get(Nick, Recip) -->
  `PRIVMSG `, priv_msg_rec(Nick, Recip), !.

priv_msg_get(Nick, Recip) -->
  [_], priv_msg_get(Nick, Recip).

priv_msg_rec(_, []) --> [32|_], !.

priv_msg_rec(Nick, [C|Recip]) -->
  [C], priv_msg_rec(Nick, Recip).


% XXX TODO : incorporate the parsing of https urls and and possibly
% XXX the recognition of other protocols as well (research) --

%% chat_log(+S0, -S) is semidet.
%
% A substring can be part of a chat transcript if it is a private message and
% follows a colon. The sending NICK comes before a `!`, which precedes the
% hostname of the sending NICK. This predicate should generally only be used after
% a message has been parsed as a private message.

chat_log --> `:`, !.
chat_log --> [_], chat_log.

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


%% get_title(-D, +S0, -S) is semidet.
%
% Any sequence that contains the opening 'title' tag is consumed. The following
% characters are parsed as the title until the closing tag is reached. The
% characters are stored as a list as they are parsed.

get_title(T) -->
  `<title>`, title(T), !.

get_title(T) -->
  [_], get_title(T).

%% title(-D, +S0, -S) is semidet.
%
% Consume any sequence containing the closing title tag and succeed by closing
% the list of stored contents that reside in the tag.

title([]) -->
  `</title>`, !.

title([C|T]) -->
  [C], title(T).

