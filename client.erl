-module(client).
-export([loop/2, initial_state/2]).

-include_lib("./defs.hrl").

%%%%%%%%%%%%%%%
%%%% Connect
%%%%%%%%%%%%%%%
loop(St, {connect, _Server}) ->
    Ref = make_ref(),
    case catch (list_to_atom(_Server) ! {request, self(), Ref, {connect, {St#cl_st.nick, self()}}}) of
        {'EXIT', Reason} -> % There is no server like this
            {{error, server_not_reached, "Could not connect to server!"}, St};
        _Else ->
            receive
                {exit, Ref, Reason} -> % Server crashed
                    {'EXIT', "Server crashed"};
                {result, Ref, ok_connected} -> % Connected
                    {ok, St#cl_st{server = _Server}};
                {result, Ref, {error, user_already_connected}} -> % Could not connect
                    {{error, user_already_connected, "User already connected"}, St}
            end
    end;

%%%%%%%%%%%%%%%
%%%% Disconnect
%%%%%%%%%%%%%%%
loop(St, disconnect) ->
    Ref = make_ref(),
    case catch list_to_atom(St#cl_st.server) ! {request, self(), Ref, {disconnect, {St#cl_st.nick, self()}}} of
        {'EXIT', Reason} -> % Server could not be reached
            {{error, server_not_reached, "Could not reach server"}, St};
        _Else ->
            receive
                {result, Ref, ok} -> % Disconnected
                    {ok, St};
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
     {ok, St} ;

%%%%%%%%%%%%%%%%%%%%%
%%% Sending messages
%%%%%%%%%%%%%%%%%%%%%
loop(St, {msg_from_GUI, _Channel, _Msg}) ->
     {ok, St} ;


%%%%%%%%%%%%%%
%%% WhoIam
%%%%%%%%%%%%%%
loop(St, whoiam) ->
    {St#cl_st.nick, St} ;

%%%%%%%%%%
%%% Nick
%%%%%%%%%%
loop(St,{nick,_Nick}) ->
    {ok, St#cl_st{nick = _Nick}} ;

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


% This function will take a message from the client and
% decomposed in the parts needed to tell the GUI to display
% it in the right chat room.
decompose_msg(_MsgFromClient) ->
    {"", "", ""}.


initial_state(Nick, GUIName) ->
    #cl_st { gui = GUIName, nick = Nick }.
