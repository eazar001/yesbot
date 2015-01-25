Yesbot
======
### An IRC Bot Written in Prolog

* Multi-threaded extensible bot with a "pluggable" system.
* Extensions are assumed to be mutually exclusive and will run concurrently.
* Written with the SWI Prolog implementation -- [SWI Prolog Website](http://www.swi-prolog.org/)


Configuration
=============

The first required step is to configure the bot. Here is a sample template for `config.pl`
```prolog

host('chat.freenode.net').
port(6667).
nick(mybotname).
pass('mypasshere1234').
chan('##math').
chan('##prolog').

bot_hostname(hostname).
bot_servername(servername).
bot_realname(realname).

% Add this line for custom list of preloaded extensions
preload([link_shortener, chat_log]).
```

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
