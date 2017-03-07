:- module(ringer, [ringer/1]).

:- use_module(library(irc_client)).
:- use_module(submodules/emoticon_lib).
:- use_module(submodules/utils).
:- use_module(library(settings)).
:- use_module(library(dcg/basics)).

% target any channel, any bot
%
target(_, _).

:- setting(ringer:handle, atom, none, 'IRC nick to alert on').
:- setting(ringer:shell, atom, 'rem', 'script that will be shelled').

ringer(Msg) :-
	thread_create(ignore(ringer_(Msg)), _, [detached(true)]).

ringer_(Me-Msg) :-
	Msg = msg(_Prefix, "PRIVMSG", _, [63,114,105,110,103,32|Rest]),
	determine_recipient(ringer, Msg, Rec),
	setting(ringer:handle, Handle),
	atom_codes(Handle, CHandle),
	phrase(contains(CHandle), Rest),
	setting(ringer:shell, Shell),
	once(shell(Shell)),
	send_msg(Me, priv_msg, `party has been rung`, Rec).

contains(Str) -->
	string(_),
	Str,
	string(_).
