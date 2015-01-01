
:- module(operator, [msg/2, dbg/2]).


%--------------------------------------------------------------------------------%
% commands
%--------------------------------------------------------------------------------%


% Convenience commands for specific message relays.
% XXX NOTE : Not all message types are implemented yet.
% XXX TODO : Make decision as to what should be done wrt strings, atoms, etc.

% NOTICE [Target, Notice]
msg(notice, 'NOTICE ~s :~s\r~n').

% PRIVMSG [Target, PrivMsg]
msg(priv_msg, 'PRIVMSG ~s :~s\r~n').

% TIME [Server]
msg(time, 'TIME ~a\r~n').

% KICK [Chan, Target]
msg(kick, 'KICK ~a ~a\r~n').

% INVITE [Recipient, Chan]
msg(invite, 'INVITE ~a ~a\r~n').

% NAMES [CSV] (a csv of channels)
msg(names, 'NAMES ~a\r~n').

% LIST [CSV] (a csv of channels)
msg(list, 'LIST ~a\r~n').

% TOPIC [Chan, Topic]
msg(topic, 'TOPIC ~a ~a\r~n').

% MODE [+/-, Mode, NICK]
msg(user_mode, 'MODE ~a~a ~a\r~n').

% PART [CSV] (a csv of channels)
msg(part, 'PART ~a\r~n').

% PING [Recipient]
msg(ping, 'PING ~a\r~n').

% PONG [Nick, Origin]
msg(pong, 'PONG ~a ~s\r~n').

% PASS [Pass]
msg(pass, 'PASS ~a\r~n').

% USER [Nick, HostName, ServerName, RealName]
msg(user, 'USER ~a ~a ~a :~a\r~n').

% NICK [Nick]
msg(nick, 'NICK ~a\r~n').

% JOIN [Chan]
msg(join, 'JOIN ~a\r~n').

% QUIT
msg(quit, 'QUIT :disconnect\r\n').


%--------------------------------------------------------------------------------%
% debugging
%--------------------------------------------------------------------------------%


dbg(pong, 'PONG ~a ~s~n').

