
:- module(isup, [isup/1]).

:- use_module(library(http/http_json)).
:- use_module(library(http/http_open)).
:- use_module(dispatch).
:- use_module(parser).
:- use_module(submodules/web).
:- use_module(submodules/html).


target("##prolog", "yesbot").
api(`http://isitup.org/`).

isup(Msg) :-
  thread_create(ignore(isup_(Msg)), _Id, [detached(true)]).


isup_(Msg) :-
  target(Chan, Bot),
  Msg = msg(Prefix, "PRIVMSG", [Target], Rest),
  (
     Target = Chan
  ;
     Target = Bot
  ),
  append(`?isup `, Q, Rest),
  atom_codes(Query, Q),
  normalize_space(codes(Qcodes), Query),
  api(Fst),
  append(Qcodes, Diff, Snd),
  append(Fst, Snd, Thrd),
  Diff = `.json`,
  string_codes(Site, Thrd),
  setup_call_cleanup(
    http_open(Site, Stream, []),		     
    json:json_read(Stream, json(List)),
    close(Stream)    
  ),
  memberchk(response_code=Resp, List),
  memberchk(status_code=Status, List),
  memberchk(response_time=Time, List),
  decode(Prefix, Target, Status, Resp, Time).


decode(Prefix, Target, Status, Resp, Time) :-
  (
     target(Target, _),
     Sender = Target, !
  ;
     target(_, Target),
     prefix_id(Prefix, Sender, _, _)
  ),
  (
     Status = 1,
     send_msg(priv_msg, "Website is alive.", Sender)
  ;
     Status = 2,
     send_msg(priv_msg, "Website appears down.", Sender)
  ;
     Status = 3,
     send_msg(priv_msg, "Domain was not valid.", Sender)
  ),
  format(string(Report), 'Response code: ~a, Response time: ~a s', [Resp, Time]),
  send_msg(priv_msg, Report, Sender).


