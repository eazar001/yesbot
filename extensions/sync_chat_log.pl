%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                                %
%                                                                                %
% Author: Ebrahim Azarisooreh                                                    %
% E-mail: ebrahim.azarisooreh@gmail.com                                          %
% IRC Nick: eazar001                                                             %
% Title: Yes-Bot chat-logging extension                                          %
% Description: A chat-logging extension for the Yes-Bot IRC Bot                  %
%                                                                                %
%                                                                                %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- module(sync_chat_log, [sync_chat_log/1]).

:- use_module(parser).
:- use_module(info).
:- dynamic known/3.


%% sync_chat_log(+Msg) is semidet.
%
% Attempt to parse a reply from the server and determine whether or not the
% current line is a valid line to store in an irc chat transcript. If it is, the
% user's nick is ascertained along with the recipient and the corresponding
% message. The message will also be time-stamped. The entire day's worth of chat
% logs will be stored in one file. The next day's chat transript will spawn the
% existence of a new text file.
%
% This will succeed only if Recip and Chan are identical.

sync_chat_log(Msg) :-
  Msg = msg(Prefix, "PRIVMSG", [Chan], Log),
  connection(_Nick, _Pass, Chans, _Hostname, _Servername, _Realname),
  member(Chan, Chans), !,
  prefix_id(Prefix, Nick, _, _),
  (  exists_directory('extensions/chat-logs')
  -> true
  ;  make_directory('extensions/chat-logs')
  ),
  get_time(Time),
  stamp_date_time(Time, Date, local),
  write_chat_line(Date, Nick, Chan, Log).


%% write_chat_line(+Date, +Nick, +Chan, +Log) is det.
%
% Write chat logs to the correct files using the correct naming scheme that
% reflects the point in time observed. If the current day is not known then cut
% and assert the current day. Afterwards, open a file for appending logs to. If
% the day is known, then compared the stored day to the ascertained day. If they
% match, proceed with the normal course of action. If they do not, assert the
% newly ascertained day and retract the old one. Open a new file for appending
% the new day's logs to.

write_chat_line(Date, Nick, Chan, Log) :-
  Format_time = (\Format^Fname^
    (format_time(atom(Fname), Format, Date, posix))),
  atom_concat(Chan, Format_time $ '-%d-%b-%Y.txt', Filename),
  Stamp = Format_time $ '%T',
  date_time_value(day, Date, Current_Day),
  working_directory(_Working, 'extensions/chat-logs'),
  (
     \+known(_, _, _),
     asserta(known(yes, Current_Day, Filename)), !
  ;
     known(yes, Stored_Day, _),
     (
        Current_Day = Stored_Day
     ->
        true
     ;
        retractall(known(_, _, _)),
        asserta(known(yes, Current_Day, Filename))
     )
  ),
  known(yes, _, Filename),
  setup_call_cleanup(
    open(Filename, append, Fstream, []),
    (
       (  get_action(Log, Action)
       -> format(Fstream, '~a *~s ~s~n', [Stamp, Nick, Action])
       ;  format(Fstream, '~a <~s> ~s~n', [Stamp, Nick, Log])
       ),
       flush_output(Fstream),
       working_directory(_Return, '../../')
    ),
    close(Fstream)
  ).


%% get_action(+Log, -Action) is semidet.
%
% True if first and last element of code list is decimal code 1. This signifies
% an action, which is part of CTCP.

get_action([1|Log], Action) :-
  append(`ACTION`, Action, selectchk(1) $ Log).


