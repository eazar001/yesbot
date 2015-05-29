
:- module(isup, [isup/1]).

:- use_module(library(http/json)).
:- use_module(library(http/http_open)).
:- use_module(dispatch).
:- use_module(parser).
:- use_module(submodules/web).
:- use_module(submodules/html).
:- use_module(submodules/utils).


target("##prolog", "yesbot").


isup(Msg) :-
  Msg = msg(_Prefix, "PRIVMSG", _, Rest),
  append(`?isup `, Q0, Rest),
  atom_codes(Q1, Q0),
  normalize_space(string(Query), Q1),
  format(string(Site), "http://isitup.org/~s.json", [Query]),
  setup_call_cleanup(
    http_open(Site, Stream, [timeout(20)]),		     
    json_read_dict(Stream, Dict),
    close(Stream)    
  ),
  decode(Msg, Dict.status_code, Dict.response_code, Dict.response_time).


decode(Msg, Status, Resp, Time) :-
  determine_recipient(isup, Msg, Rec),
  (
     Status = 1,
     send_msg(priv_msg, "Website is alive.", Rec)
  ;
     Status = 2,
     send_msg(priv_msg, "Website appears down.", Rec)
  ;
     Status = 3,
     send_msg(priv_msg, "Domain was not valid.", Rec)
  ),
  Status < 3,
  format(string(Report), 'Response code: ~a, Response time: ~a s', [Resp, Time]),
  send_msg(priv_msg, Report, Rec).


