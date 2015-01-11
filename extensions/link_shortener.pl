%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                                %
%                                                                                %
% Author: Ebrahim Azarisooreh                                                    %
% E-mail: ebrahim.azarisooreh@gmail.com                                          %
% IRC Nick: eazar001                                                             %
% Title: Yes-Bot Link Shortener Extension                                        %
% Descripton: Tinyurl API Link Shortener for the Yes-Bot IRC Bot                 %
%                                                                                %
%                                                                                %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- module(link_shortener, [link_shortener/1]).

:- use_module(dispatch).
:- use_module(parser).
:- use_module(library(http/http_open)).
:- use_module(library(http/http_ssl_plugin)).

%--------------------------------------------------------------------------------%
% link shortening
%--------------------------------------------------------------------------------%

% tiny-url api
tiny_form("http://tinyurl.com/api-create.php?url=").

%% make_tiny(+Link, -Title, -Tiny) is semidet.
%
% Attempt to extract the title of the link. If a title is extracted, then send the
% link to tinyurl to short the link if the link is determined to be valid and
% greater than or equal to 100 characters in length.

make_tiny(Link, Title, Tiny) :-
  visit_url(Link, Reply),
  (
     % host exists, but resource not grabbed by program
     Reply = none,
     Title = [], !
  ;
     % host exists, and the html resource has been grabbed
     get_title(Title, Reply, _), !
  ;
     % host exists, but content grabbed is not html
     Title = []
  ),
  tiny_form(F),
  string_concat(F, Link, Full),
  visit_url(Full, Tiny).


%% visit_url(+Link, -Reply) is semidet.
%
% Visit link and extract the reply and status code.
% XXX TODO : include the status code and response header
% XXX For appropriate usage of the content-type etc.
% XXX these will aid in making things much more efficient

visit_url(Link, Reply) :-
  catch((http_open(Link, Stream, []),
	 read_stream_to_codes(Stream, Reply),
	 close(Stream)), E, N = no_error),
  (
     % no problems here, succeed here
     E = N, !
  ;
     % could not connect, fail here
     E = error(socket_error('Host not found'), _), !, fail
  ;
     % can connect, but insufficient reply, so succeed
     Reply = none
  ).


%--------------------------------------------------------------------------------%
% extension
%--------------------------------------------------------------------------------%

% XXX TODO : add support for https
% XXX after implementing https, make sure that links that are
% XXX inappropriately marked as http are converted to the https
% variant by running a validation test with that variant, if and
% only if the initial http variant wasn't producing a socket error
% for the domain name.

% XXX NOTE : http_open('http://www.yahoo.com', IN, []),
% XXX read_stream_to_codes(In, Codes), format('~s', [Codes]), close(In).
% XXX is an example of an http link that should be https, reason :
% XXX ERROR: Unknown error term: ssl_error(ssl_verify)
% XXX hence here it is obvious that we should transform the prefix to
% XXX https rather than the dubious http.

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
  core:connection(_, _, Ch, _, _, _),
  atom_string(Ch, Chan),
  has_link(http, L, M, _),
  atom_codes(Link, L),
  length(L, N),
  (
     % shorten link if 100 or more characters in length
     N >= 100 ->
       make_tiny(Link, Title, Tiny),
       (
          Title = [] ->
            true
          ;
            send_msg(priv_msg, Title, Chan)
       ),
       send_msg(priv_msg, Tiny, Chan)
     ;

       visit_url(Link, Reply),
       get_title(Title, Reply, _),
       send_msg(priv_msg, Title, Chan)
  ),
  core:get_irc_stream(Stream),
  flush_output(Stream).


