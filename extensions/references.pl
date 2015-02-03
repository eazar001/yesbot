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


chan("##prolog").

references(Msg) :-
  chan(Chan),
  Msg = msg(_Prefix, "PRIVMSG", [Chan], R0),
  string_concat("?", R, R0),
  ref_value(R, Value),
  ref_title(R, Title),
  send_msg(priv_msg, Title, Chan),
  send_msg(priv_msg, Value, Chan).


%--------------------------------------------------------------------------------%
% Knowledge Base
%--------------------------------------------------------------------------------%


ref_value("faq", "http://www.pathwayslms.com/swipltuts/student/index.html").
ref_value("modules", "http://michael.richter.name/tutorials/swiplmod").
ref_value("tutorials", "http://www.pathwayslms.com/swipltuts/").
ref_value("dcg", "http://www.pathwayslms.com/swipltuts/dcg/index.html").
ref_value("webapps", "http://www.pathwayslms.com/swipltuts/html/index.html").
ref_value("clpfd", "http://www.pathwayslms.com/swipltuts/clpfd/clpfd.html").

ref_title("faq", "Frequently Asked Questions for ##Prolog").
ref_title("modules", "Using SWI-Prolog's Modules").
ref_title("tutorials", "Real World Programming in SWI-Prolog").
ref_title("dcg", "Tutorial -- Using Definite Clause Grammars in SWI-Prolog").
ref_title("webapps", "Tutorial -- Web Applications in SWI-Prolog").
ref_title("clpfd", "http://www.pathwayslms.com/swipltuts/clpfd/clpfd.html").


