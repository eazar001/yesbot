%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                                %
%      Chatty
%
%      ChatScript interface for yesbot
%
%      Author: Anne Ogborn
%	       adapted from link_shortener by Ebrahim Azarisooreh
%	       E-mail: annie@theelginworks.com
%              IRC Nick     Anniepoo
%              Title: Yes-Bot ChatScript interface
%	       Description: Allows a ChatScript based chatbot to
%	       operate as part of yesbot
%
%   This module depends on some other module loading the settings file
%
%  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- module(chatty, [chatty/1]).

:- use_module(dispatch).
:- use_module(info).
:- use_module(parser).
:- use_module(utilities).
:- use_module(submodules/html).
:- use_module(submodules/web).
:- use_module(library(http/http_open)).
:- use_module(library(http/http_ssl_plugin)).
:- use_module(library(xpath)).
:- use_module(library(settings)).
:- use_module(library(dcg/basics)).
:- use_module(library(chatscript)).

%--------------------------------------------------------------------------------%
% Main Extension
%--------------------------------------------------------------------------------%


%% chatty (+Msg) is det.
%
% This chatscript extension looks for messages of the form
%  ?chatty
%  which starts a conversation with the person who posted it, and
%  ?chatty <nick>
%  which starts a conversation with the named nick
%
%  Subsequently the bot will chat with the chatter until the bot emits
%  [endconversation] blah blah
%
%  @arg	Msg is a message term, of the form
%
%  =|msg(Prefix, Verb, ChannelList, Body)|=
%  or
%  =|msg(Prefix, Verb, ChannelList)|=
%
%  The only verb we're interested in is =|"PRIVMSG"|=
%  which comes as msg/4 above
%
%  Prefix is a string of the format
%       =|  servername / ( nickname [ [ "!" user ] "@" host ])  |=
%       the parts can be extracted with parser:prefix_id/4
%
%  Verb is the IRC protocol command.
%
%  ChannelList is a list of Channel Names, as strings
%
%  Body is the actual content of the message  "hey, what's up dude?"
%  as codes apparently
%

chatty(Msg) :-
	debug(chatty(any), '~w', [Msg]),
        ignore(chatty_(Msg)).

chatty_(Msg) :-
  Msg = msg(Prefix, "PRIVMSG", [Chan], Body),
  debug(chatty(msg), '~w in ~w: ~s', [Prefix, Chan, Body]),
  in_right_chan(Msg),
  respond_privmsg(Prefix, Chan, Body).

in_right_chan(msg(_, _, [Chan], _)) :-
  connection(_Nick, _Pass, Chans, _Hostname, _Servername, _Realname),
  member(Chan, Chans).
in_right_chan(msg(_, _, [Chan])) :-
  connection(_Nick, _Pass, Chans, _Hostname, _Servername, _Realname),
  member(Chan, Chans).

%%	respond_privmsg(+Prefix:prefix, +Chan:string, +Body:string) is
%	det
%
%	stateful, responds appropriately to user PRIVMSG's
%
%    @arg Prefix the user's name, see @chatty/1
%    @arg Chan   the channel name
%    @arg Body   the actual message e.g. "hey, how's it going"
%

% user ends conversation
% has to be first because ?chatty is a prefix of ?chatty bye
respond_privmsg(Prefix, Chan, Body) :-
  currently_talking_with_speaker(Prefix, Chan),
  this_message_ends_a_conversation(Body),
  !,
  do_end_conversation(Prefix, Chan).
% start a new conversation
respond_privmsg(Prefix, Chan, Body) :-
  \+ currently_talking_with_speaker(Prefix, Chan),
  this_message_starts_a_conversation(Body),
  do_start_conversation(Prefix, Chan),
  !.
% user asks to start a conversation and one is going
% so we do nothing
respond_privmsg(Prefix, Chan, Body) :-
  currently_talking_with_speaker(Prefix, Chan),
  this_message_starts_a_conversation(Body),
  !.
% user talks and we're not in a conversation
respond_privmsg(Prefix, Chan, _Body) :-
  \+ currently_talking_with_speaker(Prefix, Chan),
  !, fail.
% bot ends conversation
respond_privmsg(Prefix, Chan, Body) :-
  currently_talking_with_speaker(Prefix, Chan),
  !,
  talk_with_bot(Prefix, Chan, Body, EndConversation),
  (   EndConversation = true ->
      do_end_conversation(Prefix, Chan)
  ).

		 /*******************************
		 *  Conversation Management	*
		 *******************************/


:- dynamic conversation/3.

currently_talking_with_speaker(Prefix, Chan) :-
	prefix_id(Prefix, Nick, _User, _Host),
	conversation(Nick, Chan, T),
	get_time(Now),
	setting(chatty:bot_conversation_timeout, Limit),
	Now - T < Limit.

this_message_starts_a_conversation(Body) :-
	phrase(start_convo, Body).

this_message_ends_a_conversation(Body) :-
	phrase(end_convo, Body).

start_convo -->
	"?chatty",
	string(_).

end_convo -->
	"?chatty bye",
	string(_).

do_start_conversation(Prefix, Chan) :-
	prefix_id(Prefix, Nick, _User, _Host),
	get_time(Now),
	retractall(conversation(Nick, Chan, _)),
	asserta(conversation(Nick, Chan, Now)),
	setting(chatty:chatscript_bot_name, Bot),
	start_conversation(Nick, Bot, Reply),
	priv_msg(Reply, Chan, [auto_nl(true), at_most(8)]).

do_end_conversation(Prefix, Chan) :-
	prefix_id(Prefix, Nick, _User, _Host),
	retractall(conversation(Nick, Chan, _)).

talk_with_bot(Prefix, Chan, Body, EndConversation) :-
	prefix_id(Prefix, Nick, _User, _Host),
	get_time(Now),
	retractall(conversation(Nick, Chan, _)),
	asserta(conversation(Nick, Chan, Now)),
	setting(chatty:chatscript_bot_name, Bot),
	string_codes(SBody, Body),
	talk(Nick, Bot, SBody, Reply),
	string_codes(Reply, CReply),
	(   phrase(bot_ends_convo(CParthianShot), CReply)
	->  EndConversation = true,
	    string_codes(ParthianShot, CParthianShot),
	    priv_msg(ParthianShot, Chan, [auto_nl(true), at_most(8)])
	;   EndConversation = false,
	    priv_msg(Reply, Chan, [auto_nl(true), at_most(8)])
	).

bot_ends_convo(ParthianShot) -->
	"[endconvo]",
	ParthianShot.

		 /*******************************
		 *    settings		*
		 *******************************/


:- setting(chatty:bot_conversation_timeout, between(1, 3600), 600,
	   'Time a user must be silent before the bot decides they\'re not in conversation any more').

% this setting was interpreted differently until recently by the sockets package
%
:- (current_prolog_flag(version, V), V >= 70300) ; writeln('Requires SWI-Prolog 7.3.0 or better').
:- setting(chatty:chatscript_server_location, acyclic, localhost:5678,
	   'The location of the chatscript server, as a term atom:number, the host and port').

:- initialization
	setting(chatty:chatscript_server_location, Loc),
	set_chatscript_address(Loc).

:- setting(chatty:chatscript_bot_name, atom, 'Yesbot',
	   'Name of the bot as chatscript knows it').
