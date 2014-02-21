-module(server).
-export([loop/2, initial_state/1, request/2]).

-include_lib("./defs.hrl").

loop(St, _Msg) ->
	request(St, _Msg).
%
% User connecting to the server
%
request(State, {connect, {UserID,UserPID}}) ->
	ConnectedPIDs = lists:map(fun ({_, V}) -> V end, State#server_st.users),
	ConnectedIDs = lists:map(fun ({X, _}) -> X end, State#server_st.users),
	UserConnected = lists:member(UserID, ConnectedIDs) or lists:member(UserPID,ConnectedPIDs),
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
	ConnectedPIDs = lists:map(fun ({_, V}) -> V end, State#server_st.users),
	ConnectedIDs = lists:map(fun ({X, _}) -> X end, State#server_st.users),
	UserConnected = lists:member(UserID, ConnectedIDs) and lists:member(UserPID,ConnectedPIDs),
	if
		not UserConnected ->
			{{error, user_not_connected}, State};
		true ->
			NewUserList = lists:delete({UserID,UserPID}, State#server_st.users),
			{ok, State#server_st{users = NewUserList}}
	end;

%
% User joining a channel
% Currently only represents the case where a user joins a channel that doesn't exist already.
%
request(State, {join, {UserID,UserPID}, ChannelName}) ->
	% Check if channel exists
	io:format("request join \n"),
	ListOfChannels = State#server_st.channels,
	io:format("created list of channels \n"),
	ChannelToJoin = lists:keyfind(ChannelName,#channel.name, ListOfChannels),
	io:format("created channel to join \n"),
	case ChannelToJoin of
		%Channel doesn't exist
		false ->
			io:format("CHANNEL DOES NOT EXIST \n"),
			%Create new channel and add the user to it.
			NewChannel = #channel{name = ChannelName, users = [UserID]},
			io:format(NewChannel#channel.name),
			NewChannelList = lists:append([NewChannel], ListOfChannels),
			%Add the channel to the list of channels and return it.
			{ok, State#server_st{channels = NewChannelList}};
		% Channel exists.
		_asd ->
			io:format("CHANNEL EXISTS \n"),
			case lists:member(UserID, ChannelToJoin#channel.users) of
				%User is not a member of the channel
				false ->
					io:format("USER NOT IN CHANNEL \n"),
					%Add the user to the channel
					ChangedChannel = ChannelToJoin#channel{users = lists:append(ChannelToJoin#channel.users, [UserID])},
					%Remove the old reference of the channel and add a new one.
					{ok, State#server_st{channels = lists:append(lists:delete(ChannelToJoin, State#server_st.channels), [ChangedChannel])}};

				% If the user is already a member of the channel.
				true -> io:format("USER IN CHANNEL \n"), {{error,user_already_joined}, State}
			end
		
	end;
%
% User leaving a channel.
%
request(State, {leave, {UserID, UserPID}, ChannelName}) ->
	ListOfChannels = State#server_st.channels,
	ChannelToLeave = lists:keyfind(ChannelName,#channel.name, ListOfChannels),
	case lists:member(UserID, ChannelToLeave#channel.users) of 
		false ->
			{{error, user_not_joined}, State};
		true ->
			ChangedChannel = ChannelToJoin#channel{users = lists:delete([UserID], ChannelToJoin#channel.users)},
			{ok, State#server_st{channels = lists:append(lists:delete(ChannelToJoin, State#server_st.channels), [ChangedChannel])}}
	end;

%
% User sending a message to a channel
%
request(State, {message, {UserID,UserPID}, ChannelName, Token}) ->
	{value, Channel} = lists:keysearch(ChannelName,1,State#server_st.channels),
	ListOfUsers = Channel#channel.users,
	UserPIDs = lists:map(fun ({_, V}) -> V end, ListOfUsers),
	[Pid ! {message, UserID, ChannelName, Token} || Pid <- UserPIDs],
	{ok, State}.



initial_state(_Server) ->
    #server_st{users=[], channels = []}.
