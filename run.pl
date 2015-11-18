#!/usr/bin/swipl

%% Run yesbot here

:- use_module(pack_checker).


:- set_prolog_flag(verbose, silent).
:- set_prolog_flag(optimise, true).
:- install_deps.
:- initialization run.


:- use_module(core).
:- use_module(library(lambda)).
:- use_module(library(func)).


run :-
  main(35320).


