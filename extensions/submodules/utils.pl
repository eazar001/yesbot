
:- module(utils, [determine_recipient/3]).

:- use_module(parser).


%% determine_recipient(+Module:atom, +Msg:compound, -Recipient:string) is semidet.
%
% Determine the recipient of message feedback. If the original sender addressed
% a request to the channel, then the feedback is directed to the channel. If the
% target was the bot, then the feedback is directed back to the original sender.
% The calling module must have a fact target/2 declared in the form:
% target(Channel, Bot).

determine_recipient(Mod, msg(_, "PRIVMSG", [Chan], _), Chan) :-
  Mod:target(Chan, _).

determine_recipient(Mod, msg(Prefix, "PRIVMSG", [Bot], _), Sender) :-
  Mod:target(_, Bot),
  prefix_id(Prefix, Sender, _, _).

