%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                                %
%                                                                                %
% Author: Ebrahim Azarisooreh                                                    %
% E-mail: ebrahim.azarisooreh@gmail.com                                          %
% IRC Nick: eazar001                                                             %
% Title: Yes-Bot chat-logging extension                                          %
% Descripton: A chat-logging extension for the Yes-Bot IRC Bot                   %
%                                                                                %
%                                                                                %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- module(chat_log, [chat_log/4]).

:- use_module(parser).
:- dynamic known/3.


%% chat_log(+Rest, +Nick, +Recip, +Chan) is semidet.
%
% Attempt to parse a reply from the server and determine whether or not the
% current line is a valid line to store in an irc chat transcript. If it is, the
% user's nick is ascertained along with the recipient and the corresponding
% message. The message will also be time-stamped. The entire day's worth of chat
% logs will be stored in one file. The next day's chat transript will spawn the
% existence of a new text file.
%
% This will succeed only if Nick and Recip are identical.

chat_log(Rest, Nick, Chan, Chan) :-
  chat_log(Rest, Log),
  get_time(Time),
  stamp_date_time(Time, Date, local),
  write_chat_line(Date, Nick, Log).


%% write_chat_line(+Date, +Nick, +Log) is det.
%
% Write chat logs to the correct files using the correct naming scheme that
% reflects the point in time observed. If the current day is not known then cut
% and assert the current day. Afterwards, open a file for appending logs to. If
% the day is known, then compared the stored day to the ascertained day. If they
% match, proceed with the normal course of action. If they do not, assert the
% newly ascertained day and retract the old one. Open a new file for appending
% the new day's logs to.

write_chat_line(Date, Nick, Log) :-
  format_time(atom(Filename), '%d-%b-%Y.txt', Date, posix),
  format_time(atom(Stamp), '%T', Date, posix),
  date_time_value(day, Date, Current_Day),
  working_directory(_Working, 'extensions/chat-logs'),
  (
     \+known(_, _, _),
     asserta(known(yes, Current_Day, Filename)), !
  ;
     known(yes, Stored_Day, Filename),
     Current_Day = Stored_Day ->
       true
     ;
       retractall(known(_, _, _)),
       asserta(known(yes, Current_Day, Filename))
  ),
  known(yes, _, Filename),
  open(Filename, append, Fstream, []),
  format(Fstream, '~a <~s> ~s~n', [Stamp, Nick, Log]),
  flush_output(Fstream),
  close(Fstream),
  working_directory(_Return, '../../').
