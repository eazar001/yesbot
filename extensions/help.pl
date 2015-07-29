
:- module(help, [help/1]).

:- use_module(dispatch).
:- use_module(parser).
:- use_module(submodules/utils).


target("##prolog", "yesbot").

help(Msg) :-
  Msg = msg(_Prefix, "PRIVMSG", [_Target], Rest),
  (
     append(`?help `, Q0, Rest),
     string_codes(Q, Q0),
     normalize_space(string(Ext), Q),
     determine_recipient(help, Msg, Recipient),
     once(ext_help(Ext, Response)),
     priv_msg(Response, Recipient), !
  ;
     Rest = `?help`,
     determine_recipient(help, Msg, Recipient),
     help_msg(Response),
     priv_msg(Response, Recipient)
  ).


%--------------------------------------------------------------------------------%
% Knowledge Base
%--------------------------------------------------------------------------------%


ext_help(Query, Msg) :-
  (
     Query = "google",
     google(Msg)
  ;
     Query = "",
     help_msg(Msg)
  ;
     Query = "wiki",
     wiki(Msg)
  ;
     Query = "dict",
     dict(Msg)
  ;
     Query = "emoticons",
     emoticons(Msg)
  ;
     Query = "search",
     search(Msg)
  ;
     Query = "yesbot",
     yesbot(Msg)
  ;
     Query = "keywords",
     keywords(Msg)
  ;
     Query = "messages",
     messages(Msg)
  ;
     Query = "website-status",
     website_status(Msg)
  ).


google("?google <search term> will perform an ""I'm feeling lucky search""").

wiki("?wiki <search term> will attempt a wikipedia lookup").

dict("?dict <search term> will attempt to look up a word in dictionary.com").

emoticons("?emote <emoticon> will attempt to define wacky emoticons").

search("?search predicate/N OR f(function/N) to search for some predicate \c
  or function with N arity in the swi object database/documentation (use \c
  ?search -q for quiet output OR ?search -qq for quieter). You can also search \c
  for a library using ?search library(libname) [no quietness options available \c
  for library searches]. Limited manual searches are done with ?search \c
  manual(section_name)").

help_msg("?help <option> where your options are keywords, search, google, \c
  wiki, dict, emoticons, yesbot, messages, website-status").

yesbot("Do I have a bug? Want to request a feature? Want to hack me? You can \c
  pursue all this at: https://github.com/eazar001/yesbot/tree/prolog-irc").

keywords("?<keyword> where currently available keywords are: faq, modules, \c
  tutorials, dcg, webapps, clpfd, swipl-devel, lpn, amzi, yesbot").

messages("?record message('somenickname', \"your message text goes here\") to \c
  send a message to a user with 'somenickname' (must have single quotes) with \c
  your desired message text (must have double quotes). The recipient will be \c
  notified when rejoining the channel. Message length is limited to how much \c
  your client and the bot's client is able to send on irc. If you'd like to \c
  send longer messages simple say ?record. Yesbot will then go into recording \c
  mode where you can record multiple lines that are prefixed with the '>' char \c
  (WITHOUT THE SINGLE QUOTES). (For example: >this is a line being recorded in \c
  recording mode). When you are done you can say anything that is not prefixed \c
  with the '>' char. Now you can tell Yesbot who you intend to send the message to. \c
  (In this mode, you should NOT specify your recipient's nickname in single quotes)").

website_status("?isup <domain> to check on the status of a website, e.g. \c
  ?isup www.google.com   (Do not identify protocol such as http, https, etc.)").


