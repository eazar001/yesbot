
%% Parsing module


:- module(parser,
	  [
	    has_link/4
	   ,chat_log/2
	   ,priv_msg/4
	   ,get_title/3
	   ,ping_from/2
	  ]).


/*

Documentation Source: http://www.networksorcery.com/enp/protocol/irc.htm
Message syntax:
  
message =	[ ":" prefix SPACE ] command [ params ] crlf	
prefix =	servername / ( nickname [ [ "!" user ] "@" host ])	
command =	1*letter / 3digit	
params =	*14( SPACE middle ) [ SPACE ":" trailing ]	
=/	14( SPACE middle ) [ SPACE [ ":" ] trailing ]

// Any byte except NUL, CR, LF, " " and ":".
nospcrlfcl =	%x01-09 / %x0B-0C / %x0E-1F / %x21-39 / %x3B-FF
middle =	nospcrlfcl *( ":" / nospcrlfcl )	
trailing =	*( ":" / " " / nospcrlfcl )	
SPACE =	%x20	; Whitespace.
crlf =	%x0D %x0A	; Carriage return/linefeed.
  
*/


%% is_prefix(+S0, -S) is semidet.
%
% This will succeed iff a message has a colon as its first character. The rest
% of the message will be returned as the difference.

is_prefix --> `:`.

%% trailing(+S0, -S) is semidet.
%
% This will succeed iff a parameter has a colon as its first character. The
% rest of the parameter will be returned as the difference.

trailing --> `:`.


%--------------------------------------------------------------------------------%


%% parse_line(+Line, -Msg) is det.
%
% After splitting the line from any potential trailing parameters, format
% the line in a message compound term for proper reading outside of this
% module.

parse_line(Line, Msg) :-
  split_from_trailer(Line, Out),
  once(fmt_line(Out, Msg)).


%% fmt_line(+Line, -Msg) is det.
%
% Split a server message into (potentially) 3 parts. The message can be split
% into a prefix, command, and some command parameters. However, the prefix
% is optional, and the parameter list can be potentially empty.
%
% Msg output is of the format:
% msg(prefix, command, [parameters...], trailing_parameter).

fmt_line(["", Main, Trailer], msg(Prefix, Cmd, Params, Trailer)) :-
  split_string(Main, " ", "", [Prefix,Cmd|Params]).

fmt_line(["", Main], msg(Prefix, Cmd, Params)) :-
  split_string(Main, " ", "", [Prefix,Cmd|Params]).

fmt_line([Main, Trailer], msg(Cmd, Params, Trailer)) :-
  split_string(Main, " ", "", [Cmd|Params]).

fmt_line([Main], msg(Cmd, Params)) :-
  split_string(Main, " ", "", [Cmd|Params]).


%% split_from_trailer(+Line, -Out) is det.
%
% Split the main portion of the message from the trailer portion of the message
% if a trailer does exist. These are the possibilities when operating
% under the current IRC protocol:
%
% 1) ["", Main, Trailer]
% 2) ["", Main]
% 3) [Main, Trailer]
% 4) [Main]

split_from_trailer(Line, Out) :-
  split_string(Line, ":", " ", Out).





/*
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

*/