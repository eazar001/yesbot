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
  thread_create(ignore((references_(Msg), fail)), _Id, [detached(true)]).

references_(Msg) :-
  chan(Chan),
  (
     Msg = msg(_Prefix, "PRIVMSG", [Chan], [63|Codes]),
     atom_codes(R0, Codes),
     normalize_space(codes(R), R0),
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


/*
 * FAQ Sections 
 *
 */

ref_value(`faq`, "http://www.pathwayslms.com/swipltuts/student/index.html").
ref_value(`advice`,
  "http://www.pathwayslms.com/swipltuts/student/index.html#strong_advice").
ref_value(`hammer`,
  "http://www.pathwayslms.com/swipltuts/student/index.html#blue").
ref_value(`regulars`,
  "http://www.pathwayslms.com/swipltuts/student/index.html#regulars").
ref_value(`recursion`,
  "http://www.pathwayslms.com/swipltuts/student/index.html#recursion").
ref_value(`results`,
  "http://www.pathwayslms.com/swipltuts/student/index.html#list").
ref_value(`loop`,
  "http://www.pathwayslms.com/swipltuts/student/index.html#loop").
ref_value(`loops`,
  "http://www.pathwayslms.com/swipltuts/student/index.html#loop").
ref_value(`return`,
  "http://www.pathwayslms.com/swipltuts/student/index.html#return").


%--------------------------------------------------------------------------------%


ref_value(`modules`, "http://michael.richter.name/tutorials/swiplmod").
ref_value(`module`, "http://michael.richter.name/tutorials/swiplmod").
ref_value(`tutorials`, "http://www.pathwayslms.com/swipltuts/").
ref_value(`tutorial`, "http://www.pathwayslms.com/swipltuts/").
ref_value(`dcg`, "http://www.pathwayslms.com/swipltuts/dcg/index.html").
ref_value(`webapps`, "http://www.pathwayslms.com/swipltuts/html/index.html").
ref_value(`webapp`, "http://www.pathwayslms.com/swipltuts/html/index.html").
ref_value(`clpfd`, "http://www.pathwayslms.com/swipltuts/clpfd/clpfd.html").
ref_value(`swipl-devel`, "https://github.com/SWI-Prolog/swipl-devel").
ref_value(`lpn`, "http://www.learnprolognow.org/").
ref_value(`lpn`, "http://www.learnprolognow.org").
ref_value(`lpn-swish`, "http://lpn.swi-prolog.org/").
ref_value(`lpn-swish`, "http://lpn.swi-prolog.org").
ref_value(`amzi`, "http://www.amzi.com/AdventureInProlog/index.php").


%--------------------------------------------------------------------------------%


ref_title(`yesbot`, "https://github.com/eazar001/yesbot/commits/prolog-irc").

ref_title(`faq`, "Frequently Asked Questions for ##Prolog").
ref_title(`advice`, "Strong Advice for Students on ##prolog").
ref_title(`regulars`, "And a Plea to the Regulars").
ref_title(`recursion`, "The Standard Recursion Pattern").
ref_title(`hammer`, "I get true, then false when I expected just true").
ref_title(`results`, "How Do I get all the Results in a List?").
ref_title(`loop`, "How Do I Make a Loop?").
ref_title(`loops`, "How Do I Make a Loop?").
ref_title(`return`, "So Everything Returns True or False, Right?").


ref_title(`modules`, "Using SWI-Prolog's Modules").
ref_title(`module`, "Using SWI-Prolog's Modules").
ref_title(`tutorials`, "Real World Programming in SWI-Prolog").
ref_title(`dcg`, "Tutorial -- Using Definite Clause Grammars in SWI-Prolog").
ref_title(`webapps`, "Tutorial -- Web Applications in SWI-Prolog").
ref_title(`webapp`, "Tutorial -- Web Applications in SWI-Prolog").
ref_title(`clpfd`, "http://www.pathwayslms.com/swipltuts/clpfd/clpfd.html").
ref_title(`swipl-devel`, "SWI-Prolog/swipl-devel").
ref_title(`lpn`, "Learn Prolog Now!").
ref_title(`lpn-swish`, "Learn Prolog Now! (SWISH Integration)").
ref_title(`amzi`, "Amzi! inc. Adventure in Prolog").
ref_title(`yesbot`, "Bug to report? Feature request? Want to hack me?").


