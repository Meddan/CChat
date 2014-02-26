-module(client).
-export([loop/2, initial_state/2]).

-include_lib("./defs.hrl").

%%%%%%%%%%%%%%%
%%%% Connect
%%%%%%%%%%%%%%%
loop(St, {connect, _Server}) ->
    case catch (genserver:request(list_to_atom(_Server), {connect, {St#cl_st.nick, self()}})) of
        {'EXIT', Reason} -> % There is no server like this
            {{error, server_not_reached, "Could not reach server!"}, St};
        {exit, Ref, Reason} -> % Server crashed
            {'EXIT', "Server crashed"};
        ok_connected -> % Connected
            {ok, St#cl_st{server = _Server, connected = true}};
        {error, user_already_connected} -> % Could not connect
            {{error, user_already_connected, "User with that name already connected"}, St}
    end;

%%%%%%%%%%%%%%%
%%%% Disconnect
%%%%%%%%%%%%%%%
loop(St, disconnect) ->
    Ref = make_ref(),
    case catch genserver:request(list_to_atom(St#cl_st.server), {disconnect, {St#cl_st.nick, self()}}) of
        {'EXIT', Reason} -> % Server could not be reached
            {{error, server_not_reached, "Could not reach server"}, St};
        ok -> % Disconnected
            {ok, St#cl_st{connected = false}};
        {error, user_not_connected} -> % User was never connected
            {{error, user_not_connected, "User not connected"}, St};
        {error, leave_channels_first} -> % User hasn't left the channels first
            {{error, leave_channels_first, "Leave channels before disconnecting"}, St}
    end;


%%%%%%%%%%%%%%
%%% Join
%%%%%%%%%%%%%%
loop(St,{join,_Channel}) ->
    Ref = make_ref(),
    case genserver:request(list_to_atom(St#cl_st.server), {join, {St#cl_st.nick, self()}, _Channel}) of
        ok -> % Join went ok
            {ok, St};
        {error, user_already_joined} -> % User has already joined thic channel before
            {{error, user_already_joined, "You've already joined this channel!"}, St}
    end;

%%%%%%%%%%%%%%%
%%%% Leave
%%%%%%%%%%%%%%%
loop(St, {leave, _Channel}) ->
    Ref = make_ref(),
    case genserver:request( list_to_atom(St#cl_st.server), {leave, {St#cl_st.nick, self()}, _Channel}) of 
        ok ->
            {ok, St};
        {error, user_not_joined} ->
            {{error, user_not_joined, "You have not joined this channel!"}, St}
    end;

%%%%%%%%%%%%%%%%%%%%%
%%% Sending messages
%%%%%%%%%%%%%%%%%%%%%
loop(St, {msg_from_GUI, _Channel, _Msg}) ->
    Ref = make_ref(),
    case genserver:request(list_to_atom(St#cl_st.server), {message, {St#cl_st.nick, self()}, _Channel, _Msg}) of
        ok ->
            {ok, St};
        {error, user_not_joined} ->
            {{error, user_not_joined, "You have not joined this channel!"}, St}
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
    This = self(),
    io:fwrite("Receiving message ~w~n", [This]),
    {Channel, Name, Msg} = decompose_msg(_MsgFromClient),
    gen_server:call(list_to_atom(St#cl_st.gui), {msg_to_GUI, Channel, Name++"> "++Msg}),
    io:format("Should be done writing.\n"),
    {ok, St}.


% This function will take a message from the client and
% decomposed in the parts needed to tell the GUI to display
% it in the right chat room.
decompose_msg({message_from_server, Channel, Nick, Token}) ->
    io:format("message_from_server \n"),
    io:format("message, nick: ~n~w", [Nick]),
    io:format("message, channel: ~n~w", [Channel]),
    {Channel, Nick, Token}.


initial_state(Nick, GUIName) ->
    #cl_st { gui = GUIName, nick = Nick, connected = false }.
