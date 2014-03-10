-module(client).
-export([loop/2, initial_state/2]).

-include_lib("./defs.hrl").

%%%%%%%%%%%%%%%
%%%% Connect remote
%%%%%%%%%%%%%%%
loop(St, {connect, {_Server, Machine}}) ->
    RemServer = {list_to_atom(_Server), list_to_atom(Machine)}, 
    RemMachine = list_to_atom(Machine),
    case catch net_adm:ping(Machine) of
        pong ->
            case catch (genserver:request(RemServer, {connect, {St#cl_st.nick, self()}})) of
                {'EXIT', Reason} -> % There is no server like this
                    {{error, server_not_reached, "Could not reach server!"}, St};
                ok_connected -> % Connected
                    {ok, St#cl_st{server = RemServer, connected = true, machine = RemMachine}};
                {error, user_already_connected} -> % Could not connect
                    {{error, user_already_connected, "User with that name already connected"}, St}
            end;
        pang ->
            {{error, server_not_reached, "Could not reach server!"}, St}
    end;

%%%%%%%%%%%%%%%
%%%% Connect
%%%%%%%%%%%%%%%
loop(St, {connect, _Server}) ->
    case catch (genserver:request(list_to_atom(_Server), {connect, {St#cl_st.nick, self()}})) of
        {'EXIT', Reason} -> % There is no server like this
            {{error, server_not_reached, "Could not reach server!"}, St};
        ok_connected -> % Connected
            {ok, St#cl_st{server = list_to_atom(_Server), connected = true, machine = false}};
        {error, user_already_connected} -> % Could not connect
            {{error, user_already_connected, "User with that name already connected"}, St}
    end;

%%%%%%%%%%%%%%%
%%%% Disconnect
%%%%%%%%%%%%%%%
loop(St, disconnect) ->
    case St#cl_st.connected of
        true -> % if connected
            case catch genserver:request(St#cl_st.server, {disconnect, {St#cl_st.nick, self()}}) of
                {'EXIT', Reason} -> % Server could not be reached
                    {{error, server_not_reached, "Could not reach server"}, St};
                ok -> % Disconnected
                    {ok, St#cl_st{connected = false}};
                {error, leave_channels_first} -> % User hasn't left the channels first
                    {{error, leave_channels_first, "Leave channels before disconnecting"}, St}
            end;
        false -> % if not connected
            {{error, user_not_connected, "User not connected"}, St}
    end;




%%%%%%%%%%%%%%
%%% Join
%%%%%%%%%%%%%%
loop(St,{join,_Channel}) ->
    case genserver:request(St#cl_st.server, {join, {St#cl_st.nick, self()}, _Channel}) of
        ok -> % Join went ok
            {ok, St};
        {error, user_already_joined} -> % User has already joined thic channel before
            {{error, user_already_joined, "You've already joined this channel!"}, St}
    end;

%%%%%%%%%%%%%%%
%%%% Leave
%%%%%%%%%%%%%%%
loop(St, {leave, _Channel}) ->
    case St#cl_st.machine of
        false ->
            case genserver:request({list_to_atom(_Channel)}, {leave, {St#cl_st.nick, self()}}) of 
                ok -> % User is in channel
                    {ok, St};
                {error, user_not_joined} -> % User is not in channel
                    {{error, user_not_joined, "You have not joined this channel!"}, St}
            end;
        _remote ->
            case genserver:request({list_to_atom(_Channel), St#cl_st.machine}, {leave, {St#cl_st.nick, self()}}) of 
                ok -> % User is in channel
                    {ok, St};
                {error, user_not_joined} -> % User is not in channel
                    {{error, user_not_joined, "You have not joined this channel!"}, St}
            end
    end;

%%%%%%%%%%%%%%%%%%%%%
%%% Sending messages
%%%%%%%%%%%%%%%%%%%%%
loop(St, {msg_from_GUI, _Channel, _Msg}) ->
    case St#cl_st.machine of
        false ->
            case genserver:request({list_to_atom(_Channel)}, {message, {St#cl_st.nick, self()}, _Msg}) of
                ok -> % User has joined channel in which it writes
                    {ok, St};
                {error, user_not_joined} -> % User has not joined channel
                    {{error, user_not_joined, "You have not joined this channel!"}, St}
            end;
        _remote ->
            case genserver:request({list_to_atom(_Channel), St#cl_st.machine}, {message, {St#cl_st.nick, self()}, _Msg}) of
                ok -> % User has joined channel in which it writes
                    {ok, St};
                {error, user_not_joined} -> % User has not joined channel
                    {{error, user_not_joined, "You have not joined this channel!"}, St}
            end
    end;

%%%%%%%%%%%%%%
%%% WhoIam
%%%%%%%%%%%%%%
loop(St, whoiam) ->
    {St#cl_st.nick, St} ;

%%%%%%%%%%
%%% Nick
%%%%%%%%%%
loop(St,{nick,_Nick}) ->
    if
        not St#cl_st.connected ->
            {ok, St#cl_st{nick = _Nick}} ;
        true ->
            {{error, nick_change_error, "You can't change nickname while connected!"}, St}
    end;

%%%%%%%%%%%%%
%%% Debug
%%%%%%%%%%%%%
loop(St, debug) ->
    {St, St} ;

%%%%%%%%%%%%%%%%%%%%%
%%%% Incoming message
%%%%%%%%%%%%%%%%%%%%%
loop(St , _MsgFromClient) ->
    {Channel, Name, Msg} = decompose_msg(_MsgFromClient),
    gen_server:call(list_to_atom(St#cl_st.gui), {msg_to_GUI, Channel, Name++"> "++Msg}),
    {ok, St}.


% This function will take a message from the client and
% decomposed in the parts needed to tell the GUI to display
% it in the right chat room.
decompose_msg({message_from_server, Channel, Nick, Token}) ->
    {atom_to_list(Channel), Nick, Token}.


initial_state(Nick, GUIName) ->
    #cl_st { gui = GUIName, nick = Nick, connected = false }.
