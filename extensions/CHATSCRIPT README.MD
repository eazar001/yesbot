The ChatScript Extension
========================

Use:

Users can start the bot interacting from chatscript with 

?chatty

and end (not working right now) with 

?chatty bye

Installing
----------

1. add chatty to your active extensions in your settings.db file

2. install ChatScript from chatscript.sourceforge.net

3. copy filesYesbot.txt from the source tree to the chatscript root directory

4. copy the entire ChatScript/RAWDATA/YESBOT directory into /RAWDATA in the chatscript install

5. IMPORTANT - Change authorizedip.txt to L_2938y423hsz   where the trash after L_ is known only
to you. Otherwise the chatscript server will let anyone exec on your machine.

6. start chatscript in local mode, enter a user name, and enter

:build 0

:build yesbot

after the bot builds 

7.stop local chatscript and run

C:\docs\chatscript\chatscript.exe port=5678 userlog serverctrlz

on windows or

LinuxChatScript64 port=5678 userlog serverctrlz

on linux  (use the 32 bit vesion if you're on 32 bit)

8. start yesbot normally

