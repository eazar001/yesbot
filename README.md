Yesbot
======
### An IRC Bot Written in Prolog

* Multi-threaded extensible bot with a "pluggable" system.
* Extensions are assumed to be mutually exclusive and will run concurrently.
* Written with the SWI Prolog implementation -- [SWI Prolog Website](http://www.swi-prolog.org/)


Configuration
=============

Yesbot requires some extra pack dependencies. If you are running this bot for the first time you should install the 
lambda, func, and mavis packs using pack_install/1.

The next required step is to configure the bot using the settings tools in SWI-Prolog. 

Look at config.pl 

Any settings you need to change, you can change by

?- setting(config:nick, examplebot).

When done persist them with 

?- save_settings.

They'll be saved in a new file, settings.db, in your working directory.

This file should not be added to git's managed files.

settings.db can be edited manually if you desire.

Do be aware that the default logs the bot onto ##prolog and ##math

Please bot responsibly.


Running the bot
===============
Execute the bot like such:
```prolog

swipl -q -O run.pl
```

Extending the bot
=================

There are two demo plugins that come with Yesbot. One is a basic chat-logging extension
and the other is a TinyURL powered link shortener. On startup Yesbot will ask you if you would
like to load these extensions. Extensions for Yesbot are very simple. They are essentially
Prolog predicates with the same name as their containing modules. All Yesbot extensions 
are required to have an arity of 1 (this may change in the future). Extensions should all take in 
one argument: the current server line. The main extension predicate should have the same
name as the Prolog module itself (sans the `.pl` portion). The demo extension source code is
located in the extensions directory.
