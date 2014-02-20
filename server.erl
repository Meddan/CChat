-module(server).
-export([loop/2, initial_state/1, request/2]).

-include_lib("./defs.hrl").

loop(St, _Msg) ->
	request(St, _Msg).
%
% User connecting to the server
%
request(State, {connect, {UserID,UserPID}}) ->
	UserConnected = lists:member({UserID,UserPID}, State#server_st.users),
	if
		not UserConnected ->
			{ok_connected, State#server_st{users = lists:append([{UserID,UserPID}], State#server_st.users)}};
		true ->
			{{error, user_already_connected}, State}
	end;
	

%
% User disconnecting from the server
%
request(State, {disconnect, {UserID,UserPID}}) ->
	UserConnected = lists:member({UserID,UserPID}, State#server_st.users),
	if
		not UserConnected ->
			{{error, user_not_connected}, State};
		true ->
			{ok, State#server_st{users = lists:delete([{UserID,UserPID}], State#server_st.users)}}
	end;

%
% User joining a channel
% Currently only represents the case where a user joins a channel that doesn't exist already.
%
request(State, {join, {UserID,UserPID}, ChannelName}) ->
	% Check if channel exists
	ChannelToJoin = lists:keysearch(ChannelName,1,State#server_st.channels),
	case ChannelToJoin of
		% Channel exists.
		{value, Tuple} ->
			case lists:member(UserID, Tuple) of
				%User is not a member of the channel
				false ->
				%Add the user to the channel
				ChangedChannel = Tuple#channel{users = lists:append(Tuple#channel.users, [UserID])},
				%Remove the old reference of the channel and add a new one.
				{ok, State#server_st{channels = lists:append(lists:delete(Tuple, State#server_st.channels), [ChangedChannel])}};

				% If the user is already a member of the channel.
				true -> {{error,user_already_joined}, State}
			end;
		%Channel doesn't exist
		false ->
			%Create new channel and add the user to it.
			{ok, State#server_st{channels = lists:append(State#server_st.channels, NewChannel = #channel{name = ChannelName, users = [UserID]})}}
	end;
%
% User sending a message to a channel
%
request(State, {message, {UserID,UserPID}, ChannelName, Token}) ->
	{value, Channel} = lists:keysearch(ChannelName,1,State#server_st.channels),
	ListOfUsers = Channel#channel.users,
	[Pid ! {message, UserID, ChannelName, Token} || Pid <- ListOfUsers],
	{ok, State}.



initial_state(_Server) ->
    #server_st{users=[], channels = []}.
