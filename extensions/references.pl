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
:- use_module(submodules/utils).

target("##prolog", "yesbot").

references(Msg) :-
  ignore((references_(Msg))).

references_(Msg) :-
  determine_recipient(references, Msg, Rec),
  (
     Msg = msg(_Prefix, "PRIVMSG", _, [63|Codes]),
     atom_codes(R0, Codes),
     normalize_space(atom(R), R0),
     name_pair(R, Value-Title),
     send_msg(priv_msg, Title, Rec),
     send_msg(priv_msg, Value, Rec), !
  ;
     Msg = msg(_Prefix, "PRIVMSG", [Rec], V),
     has_link(_, Link, V, _),
     string_codes(Value, Link),
     name_pair(R, Value-_),
     format(string(S), 'Hey there! I just wanted to let you know that you can \c
       use the "?~s" keyword. Pardon my interruption.', [R]),
     send_msg(priv_msg, S, Rec)
  ).


