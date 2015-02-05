
:- module(config,
     [ host/1
      ,port/1
      ,nick/1
      ,pass/1
      ,chans/1
      ,bot_hostname/1
      ,bot_servername/1
      ,bot_realname/1
      ,desired_extensions/1]).

:- use_module(library(settings)).

%--------------------------------------------------------------------------------%
% Connection Constants
%--------------------------------------------------------------------------------%

% DO NOT modify this file to set up a bot. When you
% check in code it'll publish your settings, including your
% password, and it'll always be a merge problem, and you'll have
% to set each time this file changes.
%
% Instead, alter the
% settings by querying set_setting/2 followed by save_settings/0. e.g.
% to change the port do
%
% ?- set_setting(config:port, 9999), save_settings.
%
% a file settings.db will appear in your working
% directory.
%
% DO NOT check in your
% settings.db file

%Constants for general server connection specs
:- setting(host, atom, 'chat.freenode.net', 'IRC host to connect to').
:- setting(port, between(1,0x7FFF), 6667, 'Port to connect to').
:- setting(nick, text, mybot, 'Bots nick (name) on IRC').
:- setting(pass, text, notpassword, 'Bots password.').
:- setting(chans, list(text), [ '##prolog', '##math' ],
	   'List of channels to connect to').
:- setting(extensions, list(atom), [link_shortener, chat_log],
	   'list of extensions to load').

% Constants for user registration specs
:- setting(bot_hostname, atom, hostname,
	   'User Registration host- name of local server.').
:- setting(bot_servername, atom, servername,
    'User Registration server- usually same as host. Use use_host to make it same').
:- setting(realname, atom, anonymous, 'Bot owner\'s real name').

:- initialization load_settings('settings.db').

host(Host) :-
	setting(host, Host).
port(Port) :-
	setting(port, Port).
nick(Nick) :-
	setting(nick, Nick).
pass(Pass) :-
	setting(pass, Pass).
chans(Chans) :-
	setting(chans, Chans).

bot_hostname(Name) :-
	setting(bot_hostname, BotHostName),
	(   BotHostName = use_host
	->  setting(host, Name)
	;   Name = BotHostName
	).

bot_servername(Name) :-
	setting(bot_servername, BotServerName),
	(   BotServerName = use_host
	->  setting(host, Name)
	;   Name = BotServerName
	).

bot_realname(Name) :-
	setting(realname, Name).

desired_extensions(List) :-
	setting(extensions , List).

