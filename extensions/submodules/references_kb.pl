
:- module(references_kb, [name_pair/2]).


%% name_pair(?Name:codes, ?Value-Title) is semidet.
%
% True if Name corresponds to a Value-Title pair.

name_pair(Name, Value-Title) :-
  ref_value(Name, Value),
  ref_title(Name, Title).

name_pair(Name, Value-Title) :-
  alias_name(Name, Ref),
  ref_value(Ref, Value),
  ref_title(Ref, Title).


%% alias_name(?Alias:codes, ?Name:codes) is semidet.
%
% True if Alias is a valid alias for a name.

alias_name(`modules`, `module`).
alias_name(`tutorials`, `tutorial`).
alias_name(`webapp`, `webapps`).
alias_name(`hammers`, `hammer`).
alias_name(`result`, `results`).
alias_name(`loop`, `loops`).
alias_name(`looping`, `loops`).
alias_name(`return`, `returning`).
alias_name(`returns`, `returning`).


%--------------------------------------------------------------------------------%
% Knowledge Base
%--------------------------------------------------------------------------------%


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

ref_value(`loops`,
  "http://www.pathwayslms.com/swipltuts/student/index.html#loop").

ref_value(`return`,
  "http://www.pathwayslms.com/swipltuts/student/index.html#return").

ref_value(`module`, "http://michael.richter.name/tutorials/swiplmod").
ref_value(`tutorial`, "http://www.pathwayslms.com/swipltuts/").
ref_value(`tutorial`, "http://www.pathwayslms.com/swipltuts").
ref_value(`dcg`, "http://www.pathwayslms.com/swipltuts/dcg/index.html").
ref_value(`webapps`, "http://www.pathwayslms.com/swipltuts/html/index.html").
ref_value(`clpfd`, "http://www.pathwayslms.com/swipltuts/clpfd/clpfd.html").
ref_value(`swipl-devel`, "https://github.com/SWI-Prolog/swipl-devel").
ref_value(`lpn-old`, "http://www.learnprolognow.org/").
ref_value(`lpn-old`, "http://www.learnprolognow.org").
ref_value(`lpn`, "http://lpn.swi-prolog.org/").
ref_value(`lpn`, "http://lpn.swi-prolog.org").
ref_value(`amzi`, "http://www.amzi.com/AdventureInProlog/index.php").
ref_value(`yesbot`, "https://github.com/eazar001/yesbot/commits/prolog-irc").


%--------------------------------------------------------------------------------%


ref_title(`faq`, "Frequently Asked Questions for ##Prolog").
ref_title(`advice`, "Strong Advice for Students on ##prolog").
ref_title(`regulars`, "And a Plea to the Regulars").
ref_title(`recursion`, "The Standard Recursion Pattern").
ref_title(`hammer`, "I get true, then false when I expected just true").
ref_title(`results`, "How Do I get all the Results in a List?").
ref_title(`loops`, "How Do I Make a Loop?").
ref_title(`returning`, "So Everything Returns True or False, Right?").
ref_title(`module`, "Using SWI-Prolog's Modules").
ref_title(`tutorial`, "Real World Programming in SWI-Prolog").
ref_title(`dcg`, "Tutorial -- Using Definite Clause Grammars in SWI-Prolog").
ref_title(`webapps`, "Tutorial -- Web Applications in SWI-Prolog").
ref_title(`clpfd`, "http://www.pathwayslms.com/swipltuts/clpfd/clpfd.html").
ref_title(`swipl-devel`, "SWI-Prolog/swipl-devel").
ref_title(`lpn-old`, "Learn Prolog Now!").
ref_title(`lpn`, "Learn Prolog Now! (SWISH Integration)").
ref_title(`amzi`, "Amzi! inc. Adventure in Prolog").
ref_title(`yesbot`, "Bug to report? Feature request? Want to hack me?").


