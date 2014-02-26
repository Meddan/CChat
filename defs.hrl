% This record defines the structure of the 
% client process. 
% 
% It contains the following fields: 
%
% gui: it stores the name (or Pid) of the GUI process.
%
-record(cl_st, {gui, nick, server, connected}).
    
% This record defines the structure of the 
% server process. 
% 
-record (channel, {name = "", users=[]}).
-record (server_st, {users=[], channels=[]}). %, messagepids=[]
