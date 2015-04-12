%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                                %
%                                                                                %
% Author: Ebrahim Azarisooreh                                                    %
% E-mail: ebrahim.azarisooreh@gmail.com                                          %
% IRC Nick: eazar001                                                             %
% Title: Yes-Bot Prolog References Extension                                     %
% Description: Informational References Extension for ##prolog                   %
%                                                                                %
%                                                                                %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- module(references, [references/1]).

:- use_module(dispatch).
:- use_module(submodules/html).
:- use_module(submodules/references_kb).

chan("#testeazarbot").

references(Msg) :-
  thread_create(ignore((references_(Msg), fail)), _Id, [detached(true)]).

references_(Msg) :-
  chan(Chan),
  (
     Msg = msg(_Prefix, "PRIVMSG", [Chan], [63|Codes]),
     atom_codes(R0, Codes),
     normalize_space(atom(R), R0),
     name_pair(R, Value-Title),
     send_msg(priv_msg, Title, Chan),
     send_msg(priv_msg, Value, Chan), !
  ;
     Msg = msg(_Prefix, "PRIVMSG", [Chan], V),
     has_link(_, Link, V, _),
     string_codes(Value, Link),
     name_pair(R, Value-_),
     format(string(S), 'Hey there! I just wanted to let you know that you can \c
       use the "?~s" keyword. Pardon my interruption.', [R]),
     send_msg(priv_msg, S, Chan)
  ).


