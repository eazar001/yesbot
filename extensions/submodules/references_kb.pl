
:- module(references_kb, [name_pair/2]).

:- op(1200, xfx, @).
:- op(1200, yfx, &).

term_expansion(Alias@Name, alias_name(Alias, Name)).
term_expansion(Ref&Value&Title, ref(Ref, Value, Title)).

%% name_pair(?Name:atom, ?Pair) is semidet.
%
% True if Name corresponds to a Value-Title pair.

name_pair(Name, Value-Title) :-
  ref(Name, Value, Title).

name_pair(Name, Value-Title) :-
  alias_name(Name, Ref),
  ref(Ref, Value, Title).


%% alias_name(?Alias:atom, ?Name:atom)
%
% True if Alias is a valid alias for a name.

web@webapps.
'web-apps'@webapps.
modules@module.
tutorials@tutorial.
webapp@webapps.
hammers@hammer.
result@results.
loop@loops.
looping@loops.
return@returning.
returns@returning.
hello@'hello-world'.
which@'which-prolog'.
cheat@'cheat-sheet'.
repl@'not-repl'.
hang@hung.
hangs@hung.
'prolog-hangs'@hung.
'prolog-hung'@hung.
debugging@debug.
debugger@debug.
string@strings.
term@terms.
'if-statement'@if.
control@if.
'control-flow'@if.
conditional@if.
conditionals@if.
arg@args.
argument@args.
arguments@args.


%--------------------------------------------------------------------------------%
% Knowledge Base
%--------------------------------------------------------------------------------%


%% ref(Reference:atom, Value:string, Title:string)
%
% True if reference is related to a corresponding link/value.

faq &
"http://www.pathwayslms.com/swipltuts/student/index.html" &
"Frequently Asked Questions for ##Prolog".

advice &
"http://www.pathwayslms.com/swipltuts/student/index.html#strong_advice" &
"Strong Advice for Students on ##prolog".

regulars &
"http://www.pathwayslms.com/swipltuts/student/index.html#regulars" &
"And a Plea to the Regulars".

hammer &
"http://www.pathwayslms.com/swipltuts/student/index.html#blue" &
"I get true, then false when I expected just true".

recursion &
"http://www.pathwayslms.com/swipltuts/student/index.html#recursion" &
"The Standard Recursion Pattern".

results &
"http://www.pathwayslms.com/swipltuts/student/index.html#list" &
"How Do I get all the Results in a List?".

loops &
"http://www.pathwayslms.com/swipltuts/student/index.html#loop" &
"How Do I Make a Loop?".

returning &
"http://www.pathwayslms.com/swipltuts/student/index.html#return" &
"So Everything Returns True or False, Right?".

'hello-world' &
"http://www.pathwayslms.com/swipltuts/student/index.html#hello" &
"Hello World".

'which-prolog' &
"http://www.pathwayslms.com/swipltuts/student/index.html#which" &
"Which Prolog to Use".

'cheat-sheet' &
"http://www.pathwayslms.com/swipltuts/student/index.html#cheat" &
"Ten Second Syntax Cheat Sheet".

'not-repl' &
"http://www.pathwayslms.com/swipltuts/student/index.html#not_repl" &
"Why can’t I type in code at the repl?".

hung &
"http://www.pathwayslms.com/swipltuts/student/index.html#hung" &
"Prolog Hung".

debug &
"http://www.pathwayslms.com/swipltuts/student/index.html#debug" &
"How can I see what my code is doing?".

strings &
"http://www.pathwayslms.com/swipltuts/student/index.html#howdy" &
"Difference between \"hello\", hello, HowAreYa, and 'Howdy!'".

terms &
"http://www.pathwayslms.com/swipltuts/student/index.html#wtf" &
"WTF, 4 = 2 + 2 fails?".

if &
"http://www.pathwayslms.com/swipltuts/student/index.html#if" &
"How do I make an if statement?".

args &
"http://www.pathwayslms.com/swipltuts/student/index.html#args" &
"How do I return if everything’s void?".

module &
"http://michael.richter.name/tutorials/swiplmod" &
"Using SWI-Prolog's Modules".

tutorial &
"http://www.pathwayslms.com/swipltuts/" &
"Real World Programming in SWI-Prolog".

tutorial &
"http://www.pathwayslms.com/swipltuts" &
"Real World Programming in SWI-Prolog".

dcg &
"http://www.pathwayslms.com/swipltuts/dcg" &
"Tutorial -- Using Definite Clause Grammars in SWI-Prolog".

dcg &
"http://www.pathwayslms.com/swipltuts/dcg/" &
"Tutorial -- Using Definite Clause Grammars in SWI-Prolog".

dcg &
"http://www.pathwayslms.com/swipltuts/dcg/index.html" &
"Tutorial -- Using Definite Clause Grammars in SWI-Prolog".

webapps &
"http://www.pathwayslms.com/swipltuts/html/index.html" &
"Tutorial -- Web Applications in SWI-Prolog".

clpfd &
"http://www.pathwayslms.com/swipltuts/clpfd/clpfd.html" &
"CLP(FD) Constraint Logic Programming over Finite Domains".

'swipl-devel' &
"https://github.com/SWI-Prolog/swipl-devel" &
"SWI-Prolog/swipl-devel".

'lpn-old' & "http://www.learnprolognow.org/" & "Learn Prolog Now!".
'lpn-old' & "http://www.learnprolognow.org" & "Learn Prolog Now!".
lpn & "http://lpn.swi-prolog.org/" & "Learn Prolog Now! (SWISH Integration)".
lpn & "http://lpn.swi-prolog.org" & "Learn Prolog Now! (SWISH Integration)".

amzi &
"http://www.amzi.com/AdventureInProlog/index.php" &
"Amzi! inc. Adventure in Prolog".

yesbot &
"https://github.com/eazar001/yesbot/tree/prolog-irc" &
"Bug to report? Feature request? Want to hack me?".

'c++' &
"http://www.swi-prolog.org/pldoc/doc_for?object=section%27pac\c
kages/pl2cpp.html%27" &
"A C++ interface to SWI-Prolog".

fli &
"http://www.swi-prolog.org/pldoc/man?section=foreign" &
"Foreign Language Interface".

style &
"http://arxiv.org/pdf/0911.2899v3.pdf" &
"Coding Guidelines for Prolog".

'99' &
"http://www.ic.unicamp.br/~meidanis/courses/mc336/2009s2/prolog/problemas/" &
"P-99: Ninety-Nine Prolog Problems".


