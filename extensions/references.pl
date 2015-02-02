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

references(Msg) :-
  Msg = msg(_Prefix, "PRIVMSG", ["##prolog"], R0),
  string_codes(R0, R),
  is_ref(R, Value),
  send_msg(priv_msg, Value, "##prolog").
  

is_ref -->
  initial,
  valid_cmd.


initial --> `?`.
valid_cmd --> ref.


ref(`faq`, `http://www.pathwayslms.com/swipltuts/student/index.html`).
ref(`modules`, `http://michael.richter.name/tutorials/swiplmod`).
ref(`tutorials`, `http://www.pathwayslms.com/swipltuts/`).
ref(`dcg`, `http://www.pathwayslms.com/swipltuts/dcg/index.html`).
ref(`webapps`, `http://www.pathwayslms.com/swipltuts/html/index.html`).
ref(`clpfd`, `http://www.pathwayslms.com/swipltuts/clpfd/clpfd.html`).

