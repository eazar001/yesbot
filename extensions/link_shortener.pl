%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                                %
%                                                                                %
% Author: Ebrahim Azarisooreh                                                    %
% E-mail: ebrahim.azarisooreh@gmail.com                                          %
% IRC Nick: eazar001                                                             %
% Title: Yes-Bot Link Shortener Extension                                        %
% Description: Tinyurl API Link Shortener for the Yes-Bot IRC Bot                %
%                                                                                %
%                                                                                %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- module(link_shortener, [link_shortener/1]).

:- use_module(dispatch).
:- use_module(parser).
:- use_module(utilities).
:- use_module(info).
:- use_module(submodules/html).
:- use_module(submodules/web).
:- use_module(library(http/http_open)).
:- use_module(library(http/http_ssl_plugin)).
:- use_module(library(xpath)).

%--------------------------------------------------------------------------------%
% Main Extension
%--------------------------------------------------------------------------------%


%% link_shortener(+Msg) is semidet.
%
% This link shortener extension will first parse a message to determine whether
% or not this message contains a link. If a link exists, it will determine to
% whether it is http or https. It will accordingly attempt to open the URL and
% extract a title from the from the link. If the link is 100 or more characters
% in length, then the link will be sent to tinyurl for shortening. The shortened
% URL will be sent to the channel publicly for general consumption.
%
% This predicate will only succeed if Recip is identical to Chan

link_shortener(Msg) :-
  Msg = msg(_Prefix, "PRIVMSG", [Chan], M),
  connection(_Nick, _Pass, Chans, _Hostname, _Servername, _Realname),
  member(Chan, Chans), !,
  has_link(_, L, M, _),
  atom_codes(Link, L),
  length(L, N),
  (
     % shorten link if 100 or more characters in length
     (
        N >= 100
     ->
        make_tiny(Link, Chan)
     ;
        get_time(T1),
        url_get_title(Link, T),
        get_time(T2) ->
        (  T = []
        -> true
        ;  D is T2 - T1,
	   format(string(Title), "~s   (~2fs)",
	     [clean_sequence $ unescape_title $ T, D]),
	   send_msg(priv_msg, Title, Chan)
        )
     )
  ).


%--------------------------------------------------------------------------------%
% Link Shortening
%--------------------------------------------------------------------------------%


% tiny-url api
tiny_form("http://tinyurl.com/api-create.php?url=").


%% make_tiny(+Link, +Chan) is semidet.
%
% Attempt to extract the title of the link. If a title is extracted, then send the
% link to tinyurl to shorten the link if the link is determined to be valid.

make_tiny(Link, Chan) :-
  thread_create(make_tiny_(Link, Chan), Id, []),
  tiny_form(F),
  get_time(T1),
  visit_url(string_concat(F) $ Link, T),
  get_time(T2),
  D is T2 - T1,
  format(string(Tiny), "~s   (~2fs)", [T, D]),
  thread_join(Id, _Status),
  send_msg(priv_msg, Tiny, Chan).

  
make_tiny_(Link, Chan) :-
  get_time(T1),
  url_get_title(Link, T),
  get_time(T2),
  (  T = []
  -> true
  ;  D is T2 - T1,
     format(string(Title), "~s   (~2fs)",
       [clean_sequence $ unescape_title $ T, D]),
     send_msg(priv_msg, Title, Chan)
  ).


