
%% Operator module that determines appropriate messaging templates for specific
%% atoms which connote particular irc operations.


:- module(operator,
     [ cmd/2
      ,dbg/2 ]).


%--------------------------------------------------------------------------------%
% Commands
%--------------------------------------------------------------------------------%


% Convenience commands for specific message relays.

% FIXME: Not all message types are implemented yet.
% TBD: implement a cmd/3 so non-generic commands with more complex argument
% patterns can directly inject the templates with their instantiated variables
% into the dispatch.pl interface.

%% cmd(+Type:atom, -Template:atom) is semidet.
%
% Attempt to unify with a type and a corresponding string template. The string
% template will be fed to dispatch in order to send a message.

% ADMIN [Target]
cmd(admin, 'ADMIN ~s\r~n').

% AWAY [Message]
cmd(away, 'AWAY :~s\r~n').

% CONNECT [Server, Port]
cmd(connect, 'CONNECT ~s ~s\r~n').

% DIE
cmd(die, 'DIE\r~n').

% HELP
cmd(help, 'HELP\r~n').

% INFO
cmd(info, 'INFO\r~n').

% ISON [Names] (comma separated values)
cmd(ison, 'ISON :~s\r~n').

% KILL [Client, Comment]
cmd(kill, 'KILL ~s :~s\r~n').

% LINKS
cmd(links, 'LINKS\r~n').

% LIST [Channels] (comma separated values)
cmd(list, 'LIST :~s\r~n').

% LUSERS
cmd(lusers, 'LUSERS :~s\r~n').

% NAMES [Channels]
cmd(names, 'NAMES :~s\r~n').

% OPER [Username, Pass]
cmd(oper, 'OPER ~s ~s\r~n').

% REHASH
cmd(rehash, 'REHASH\r~n').

% RESTART
cmd(restart, 'RESTART\r~n').

% RULES
cmd(rules, 'RULES\r~n').

% SERVER [Servername, Hopcount, Info]
cmd(server, 'SERVER ~s ~s :~s\r~n').

% SERVICE [Nick, Reserved, Distribution, Type, Reserved, Info]
cmd(service, 'SERVICE ~s ~s ~s ~s ~s :~s\r~n').

% SERVLIST
cmd(servlist, 'SERVLIST\r~n').

% STATS [Query]
cmd(stats, 'STATS ~s\r~n').

% USERHOST [Nick(s)]
cmd(userhost, 'USERHOST :~s\r~n').

% USERIP [NICK]
cmd(userip, 'USERIP ~s\r~n').

% USERS
cmd(users, 'USERS\r~n').

% VERSION
cmd(version, 'VERSION\r~n').

% WHO [Name]
cmd(who, 'WHO ~s\r~n').

% WHO [Name] (is an op)
cmd(who_op, 'WHO ~s o\r~n').

% WHO (are ops)
cmd(who_ops, 'WHO o\r~n').

% WHOIS [Nicks]
cmd(whois, 'WHOIS :~s\r~n').

% WHOWAS [Nick]
cmd(whowas, 'WHOWAS ~s\r~n').

% NOTICE [Target, Notice]
cmd(notice, 'NOTICE ~s :~s\r~n').

% PRIVMSG [Target, PrivMsg]
cmd(priv_msg, 'PRIVMSG ~s :~s\r~n').

% TIME [Server]
cmd(time, 'TIME ~s\r~n').

% KICK [Chan, Target]
cmd(kick, 'KICK ~s ~s\r~n').

% INVITE [Target, Chan]
cmd(invite, 'INVITE ~s ~s\r~n').

% NAMES [Chan]
cmd(names, 'NAMES ~s\r~n').

% LIST [Chans] (a csv of channels)
cmd(list, 'LIST :~s\r~n').

% TOPIC [Chan, Topic]
cmd(topic, 'TOPIC ~s :~s\r~n').

% MODE [+/-(Mode), NICK]
cmd(mode, 'MODE ~s ~s\r~n').

% PART [CSV] (a csv of channels)
cmd(part, 'PART :~s\r~n').

% PING [Target]
cmd(ping, 'PING ~s\r~n').

% PONG [Target]
cmd(pong, 'PONG ~s\r~n').

% PASS [Pass]
cmd(pass, 'PASS ~s\r~n').

% USER [Nick, HostName, ServerName, RealName]
cmd(user, 'USER ~s ~s ~s :~s\r~n').

% NICK [Nick]
cmd(nick, 'NICK ~s\r~n').

% JOIN [Chan]
cmd(join, 'JOIN ~s\r~n').

% QUIT
cmd(quit, 'QUIT :disconnect\r\n').


%--------------------------------------------------------------------------------%
% Debugging
%--------------------------------------------------------------------------------%

%% dbg(+Type:atom, -Template:atom) is semidet.
%
% This is used to debug pings, to ensure the client that pongs are being sent.

% PONG [Origin]
dbg(pong, 'PONG ~s~n').


