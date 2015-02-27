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

chan("##prolog").
references(Msg) :-
  chan(Chan),
  (
     Msg = msg(_Prefix, "PRIVMSG", [Chan], [63|R]),
     ref_value(R, Value),
     ref_title(R, Title),
     send_msg(priv_msg, Title, Chan),
     send_msg(priv_msg, Value, Chan), !
  ;
     Msg = msg(_Prefix, "PRIVMSG", [Chan], V),
     has_link(_, Link, V, _),
     string_codes(Value, Link),
     ref_value(R, Value),
     format(string(S), 'Hey there! I just wanted to let you know that you can \c
       use the "?~s" keyword. Pardon my interruption.', [R]),
     send_msg(priv_msg, S, Chan)
  ).
   

  

%--------------------------------------------------------------------------------%
% Knowledge Base
%--------------------------------------------------------------------------------%


ref_value(`faq`, "http://www.pathwayslms.com/swipltuts/student/index.html").
ref_value(`modules`, "http://michael.richter.name/tutorials/swiplmod").
ref_value(`tutorials`, "http://www.pathwayslms.com/swipltuts/").
ref_value(`dcg`, "http://www.pathwayslms.com/swipltuts/dcg/index.html").
ref_value(`webapps`, "http://www.pathwayslms.com/swipltuts/html/index.html").
ref_value(`clpfd`, "http://www.pathwayslms.com/swipltuts/clpfd/clpfd.html").
ref_value(`swipl-devel`, "https://github.com/SWI-Prolog/swipl-devel").
ref_value(`lpn`, "http://www.learnprolognow.org/").
ref_value(`amzi`, "http://www.amzi.com/AdventureInProlog/index.php").
ref_title(`yesbot`, "https://github.com/eazar001/yesbot/commits/prolog-irc").

ref_title(`faq`, "Frequently Asked Questions for ##Prolog").
ref_title(`modules`, "Using SWI-Prolog's Modules").
ref_title(`tutorials`, "Real World Programming in SWI-Prolog").
ref_title(`dcg`, "Tutorial -- Using Definite Clause Grammars in SWI-Prolog").
ref_title(`webapps`, "Tutorial -- Web Applications in SWI-Prolog").
ref_title(`clpfd`, "http://www.pathwayslms.com/swipltuts/clpfd/clpfd.html").
ref_title(`swipl-devel`, "SWI-Prolog/swipl-devel").
ref_title(`lpn`, "Learn Prolog Now!").
ref_title(`amzi`, "Amzi! inc. Adventure in Prolog").
ref_title(`yesbot`, "Bug to report? Feature request? Want to hack me?").


