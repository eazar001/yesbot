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
% True if Alias is a valid alias for a Name.

web@webapps.
'web-apps'@webapps.
modules@module.
tutorials@tutorial.
webapp@webapps.
hammers@false.
hammer@false.
true@false.
result@results.
loop@loops.
looping@loops.
return@returning.
returns@returning.
hello@'hello-world'.
which@'which-prolog'.
cheat@'cheat-sheet'.
syntax@'cheat-sheet'.
repl@'not-repl'.
hang@hung.
hangs@hung.
'prolog-hangs'@hung.
'prolog-hung'@hung.
debugging@debug.
debugger@debug.
string@strings.
atom@strings.
atoms@strings.
variable@strings.
variables@strings.
term@terms.
wtf@terms.
arith@terms.
arithmetic@terms.
math@terms.
'if-statement'@if.
control@if.
'control-flow'@if.
conditional@if.
conditionals@if.
pack@packs.
addon@packs.
addons@packs.
'add-on'@packs.
'add-ons'@packs.
package@packs.
packages@packs.
experts@expert.
expertsystem@expert.
expertsystems@expert.
'expert-systems'@expert.
'expert-system'@expert.
swipl@swi.
top@toplevel.
'?-'@toplevel.
'CHR'@chr.
'PTTP'@pttp.
cuts@cut.
message@messages.
error_messages@messages.
error_message@messages.
printing@messages.
'11'@eleventh.
'lgt'@logtalk.
'logtalk-gitter'@'logtalk-chat'.
'logtalk-discussion'@'logtalk-chat'.
'lgt-chat'@'logtalk-chat'.
'lgt-discussion'@'logtalk-chat'.
'lgt-gitter'@'logtalk-chat'.
'lgt-forum'@'logtalk-forum'.
'learn-lgt'@'learn-logtalk'.
'theexpanse'@expanse.
'the-expanse'@expanse.
syfy@expanse.


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
"http://www.pathwayslms.com/swipltuts/student/index.html#_asking_questions_on_prolog" &
"Asking Questions on ##prolog".

regulars &
"http://www.pathwayslms.com/swipltuts/student/index.html#_and_a_plea_to_the_regulars" &
"And a Plea to the Regulars".

false &
"http://www.pathwayslms.com/swipltuts/student/index.html#_i_get_true_then_false_when_i_expected_just_true" &
"I get true, then false when I expected just true".

recursion &
"http://www.pathwayslms.com/swipltuts/student/index.html#_the_standard_recursion_pattern" &
"The Standard Recursion Pattern".

results &
"http://www.pathwayslms.com/swipltuts/student/index.html#aggregate" &
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
"http://www.pathwayslms.com/swipltuts/student/index.html#whichprolog" &
"Which Prolog to Use".

'cheat-sheet' &
"http://www.pathwayslms.com/swipltuts/student/index.html#syntax" &
"Ten Second Syntax Cheat Sheet".

'not-repl' &
"http://www.pathwayslms.com/swipltuts/student/index.html#not_repl" &
"Why can’t I type in code at the repl?".

hung &
"http://www.pathwayslms.com/swipltuts/student/index.html#hung" &
"Prolog Hung".

debug &
"http://www.pathwayslms.com/swipltuts/student/index.html#debugger" &
"How can I see what my code is doing?".

strings &
"http://www.pathwayslms.com/swipltuts/student/index.html#variablesatomsstrings" &
"Difference between \"hello\", hello, HowAreYa, and 'Howdy!'".

terms &
"http://www.pathwayslms.com/swipltuts/student/index.html#arithmetic" &
"Why can’t I do arithmetic?".

if &
"http://www.pathwayslms.com/swipltuts/student/index.html#if" &
"How do I make an if statement?".

void &
"http://www.pathwayslms.com/swipltuts/student/index.html#void" &
"How do I return if everything’s void?".

module &
"https://chiselapp.com/user/ttmrichter/repository/gng/doc/trunk/output/tutorials\c
/swiplmodtut.html" &
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
"https://github.com/eazar001/yesbot" &
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

swish &
"http://swish.swi-prolog.org/" &
"SWISH -- SWI-Prolog for SHaring".

packs &
"http://www.swi-prolog.org/pack/list" &
"SWI-Prolog packages".

expert &
"http://www.amzi.com/ExpertSystemsInProlog/" &
"Amzi! inc. Expert Systems in Prolog".

expert &
"http://www.amzi.com/ExpertSystemsInProlog" &
"Amzi! inc. Expert Systems in Prolog".

datalog &
"http://www.learndatalogtoday.org/" &
"Learn Datalog Today!".

datalog &
"http://www.learndatalogtoday.org" &
"Learn Datalog Today!".

swi &
"http://www.swi-prolog.org" &
"SWI Prolog".

swi &
"http://www.swi-prolog.org/" &
"SWI Prolog".

lamp &
"http://www.swi-prolog.org/FAQ/PrologLAMP.txt" &
"Can I replace a LAMP stack with SWI-Prolog?".

toplevel &
"http://www.swi-prolog.org/FAQ/ToplevelMode.txt" &
":- vs ?-, the top level, and files".

chr &
"https://dtai.cs.kuleuven.be/CHR/" &
"CHR - Constraint Handling Rules".

pttp &
"http://www.ai.sri.com/~stickel/pttp.html" &
"Prolog Technology Theorem Prover".

cut &
"http://www.pathwayslms.com/swipltuts/student/index.html#_the_cut" &
"The Cut".

messages &
"http://www.pathwayslms.com/swipltuts/message/index.html" &
"Printing Messages in SWI-Prolog".

eleventh &
"http://imgur.com/f2qcmlt" &
"Richter\'s Eleventh Rule".

logtalk &
"http://www.logtalk.org" &
"Logtalk Website".

logtalk &
"http://www.logtalk.org/" &
"Logtalk Website".

'learn-logtalk' &
"https://learnxinyminutes.com/docs/logtalk/" &
"Learn Logtalk in Y Minutes".

'learn-logtalk' &
"https://learnxinyminutes.com/docs/logtalk" &
"Learn Logtalk in Y Minutes".

'logtalk-chat' &
"https://gitter.im/LogtalkDotOrg/logtalk3" &
"Logtalk Live Discussion".

'logtalk-forum' &
"http://http://forums.logtalk.org/" &
"Lotalk Discussion Forum".

'logtalk-forum' &
"http://forums.logtalk.org" &
"Lotalk Discussion Forum".

'expanse' &
"http://www.syfy.com/theexpanse" &
"The Expanse TV Series".
