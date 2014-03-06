-module(server).
-export([loop/2, initial_state/1, request/2]).

-include_lib("./defs.hrl").

loop(St, _Msg) ->
	request(St, _Msg).

%
% User connecting to the server
%
request(State, {connect, {UserID,UserPID}}) -> %,MessagePID
	ConnectedPIDs = lists:map(fun ({_, V}) -> V end, State#server_st.users),
	ConnectedIDs = lists:map(fun ({X, _}) -> X end, State#server_st.users),
	UserConnected = lists:member(UserID, ConnectedIDs) or lists:member(UserPID,ConnectedPIDs),
	if
		not UserConnected ->
			NewUserList = lists:append([{UserID,UserPID}],State#server_st.users),
			{ok_connected, State#server_st{ users = NewUserList} };
		true ->
			{{error, user_already_connected}, State}
	end;

%
% User disconnecting from the server
%
request(State, {disconnect, {UserID,UserPID}}) ->
	ConnectedPIDs = lists:map(fun ({_, V}) -> V end, State#server_st.users),
	ConnectedIDs = lists:map(fun ({X, _}) -> X end, State#server_st.users),
	UserConnected = lists:member(UserID, ConnectedIDs) and lists:member(UserPID,ConnectedPIDs),
	if
		not UserConnected ->	%if the user isn't connected.
			{{error, user_not_connected}, State};
		true ->
			case lists:member(true, [genserver:request(Chan, {user_exist, {UserID,UserPID}}) || Chan <- State#server_st.channels]) of
				true -> 
					{{error, leave_channels_first}, State};
				false ->
					NewUserList = lists:delete({UserID,UserPID}, State#server_st.users),
					{ok, State#server_st{users = NewUserList}}

			end
	end;

%
% User joining a channel
% Currently only represents the case where a user joins a channel that doesn't exist already.
%
request(State, {join, {UserID,UserPID}, ChannelName}) ->
	ChannelAtom = list_to_atom(ChannelName),
	ListOfChannels = State#server_st.channels,
	ChannelToJoin = lists:member(ChannelAtom, ListOfChannels),

	case ChannelToJoin of
		%Channel doesn't exist
		false ->
			%Create new channel and add the user to it.
			genserver:start(ChannelAtom, channel:initial_state(ChannelAtom, {UserID,UserPID}), fun channel:loop/2 ),
			NewChannelList = lists:append([ChannelAtom], ListOfChannels),
			%Add the channel to the list of channels and return it.
			{ok, State#server_st{channels = NewChannelList}};
		% Channel exists.
		_asd ->
			case genserver:request(ChannelAtom, {join, {UserID,UserPID}}) of
				%User is not a member of the channel
				ok ->
					{ok, State};

				% If the user is already a member of the channel.
				_else -> {{error,user_already_joined}, State}
			end
	end.
	
%Creates the initial state of the server
initial_state(_Server) ->
    #server_st{users=[], channels = []}. %, messagepids=[]
