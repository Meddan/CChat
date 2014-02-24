-module(client).
-export([loop/2, initial_state/2]).

-include_lib("./defs.hrl").

%%%%%%%%%%%%%%%
%%%% Connect
%%%%%%%%%%%%%%%
loop(St, {connect, _Server}) ->
    Ref = make_ref(),
    NewSt = St#cl_st{messagePID = spawn(fun() -> receive_message(St) end)}, % Spawn help process to listen for messages
    case catch (list_to_atom(_Server) ! {request, self(), Ref, {connect, {NewSt#cl_st.nick, self(), NewSt#cl_st.messagePID}}}) of
        {'EXIT', Reason} -> % There is no server like this
            NewSt#cl_st.messagePID ! stop,
            {{error, server_not_reached, "Could not connect to server!"}, NewSt};
        _Else ->
            receive
                {exit, Ref, Reason} -> % Server crashed
                    NewSt#cl_st.messagePID ! stop,
                    {'EXIT', "Server crashed"};
                {result, Ref, ok_connected} -> % Connected
                    {ok, NewSt#cl_st{server = _Server, connected = true}};
                {result, Ref, {error, user_already_connected}} -> % Could not connect
                    NewSt#cl_st.messagePID ! stop,
                    {{error, user_already_connected, "User with that name already connected"}, NewSt}
            end
    end;

%%%%%%%%%%%%%%%
%%%% Disconnect
%%%%%%%%%%%%%%%
loop(St, disconnect) ->
    Ref = make_ref(),
    case catch list_to_atom(St#cl_st.server) ! {request, self(), Ref, {disconnect, {St#cl_st.nick, self()}}} of
        {'EXIT', Reason} -> % Server could not be reached
            St#cl_st.messagePID ! stop,
            {{error, server_not_reached, "Could not reach server"}, St};
        _Else ->
            receive
                {result, Ref, ok} -> % Disconnected
                    St#cl_st.messagePID ! stop,
                    {ok, St#cl_st{connected = false}};
                {result, Ref, {error, user_not_connected}} -> % User was never connected
                    {{error, user_not_connected, "User not connected"}, St};
                {result, Ref, {error, leave_channels_first}} -> % User hasn't left the channels first
                    {{error, leave_channels_first, "Leave channels before disconnecting"}, St}
            end
    end;


%%%%%%%%%%%%%%
%%% Join
%%%%%%%%%%%%%%
loop(St,{join,_Channel}) ->
    Ref = make_ref(),
    list_to_atom(St#cl_st.server) ! {request, self(), Ref, {join, {St#cl_st.nick, self()}, _Channel}},
    receive
        {result, Ref, ok} -> % Join went ok
            {ok, St};
        {result, Ref, {error, user_already_joined}} -> % User has already joined thic channel before
            {{error, user_already_joined, "You've already joined this channel!"}, St}
    end;

%%%%%%%%%%%%%%%
%%%% Leave
%%%%%%%%%%%%%%%
loop(St, {leave, _Channel}) ->
    Ref = make_ref(),
    list_to_atom(St#cl_st.server) ! {request, self(), Ref, {leave, {St#cl_st.nick, self()}, _Channel}},
    receive
        {result, Ref, ok} ->
            {ok, St};
        {result, Ref, {error, user_not_joined}} ->
            {{error, user_not_joined, "You have not joined this channel!"}, St}
    end;

%%%%%%%%%%%%%%%%%%%%%
%%% Sending messages
%%%%%%%%%%%%%%%%%%%%%
loop(St, {msg_from_GUI, _Channel, _Msg}) ->
    Ref = make_ref(),
    list_to_atom(St#cl_st.server) ! {request, self(), Ref, {message, {St#cl_st.nick, self()}, _Channel, _Msg}},
    receive
        {result, Ref, ok} ->
            {ok, St};
        {result, Ref, {error, user_not_joined}} ->
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
loop(St = #cl_st { gui = GUIName }, _MsgFromClient) ->
    {Channel, Name, Msg} = decompose_msg(_MsgFromClient),
    gen_server:call(list_to_atom(GUIName), {msg_to_GUI, Channel, Name++"> "++Msg}),
    {ok, St}.

receive_message(St) ->
    receive
        {message, UserID, ChannelName, Token} ->
            if
                UserID /= St#cl_st.nick ->
                    gen_server:call(list_to_atom(St#cl_st.gui), {msg_to_GUI, ChannelName, UserID++"> "++Token}),
                    receive_message(St);
                true ->
                    receive_message(St)
            end;
        stop ->
            ok
    end.


% This function will take a message from the client and
% decomposed in the parts needed to tell the GUI to display
% it in the right chat room.
decompose_msg({message_from_server, Nick, Channel, Token}) ->
    io:format("message_from_server"),
    {Channel, Nick, Token}.


initial_state(Nick, GUIName) ->
    #cl_st { gui = GUIName, nick = Nick, connected = false }.
