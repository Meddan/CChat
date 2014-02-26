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
			%NewMessageList = lists:append([{UserID,MessagePID}], State#server_st.messagepids),
			{ok_connected, State#server_st{ users = NewUserList} }; %, messagepids = NewMessageList
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
			UserInChannel = [ [lists:member({UserID,UserPID}, Chan#channel.users)] || Chan <- State#server_st.channels],
			case catch lists:member([true], UserInChannel) of
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
	ListOfChannels = State#server_st.channels,
	ChannelToJoin = lists:keyfind(ChannelName,#channel.name, ListOfChannels),
	case ChannelToJoin of
		%Channel doesn't exist
		false ->
			%Create new channel and add the user to it.
			NewChannel = #channel{name = ChannelName, users = [{UserID,UserPID}] },
			io:format(NewChannel#channel.name),
			NewChannelList = lists:append([NewChannel], ListOfChannels),
			%Add the channel to the list of channels and return it.
			{ok, State#server_st{channels = NewChannelList}};
		% Channel exists.
		_asd ->
			case lists:member({UserID,UserPID}, ChannelToJoin#channel.users) of
				%User is not a member of the channel
				false ->
					%Add the user to the channel
					ChangedChannel = ChannelToJoin#channel{users = lists:append(ChannelToJoin#channel.users, [{UserID,UserPID}])},
					%Remove the old reference of the channel and add a new one.
					{ok, State#server_st{channels = lists:append(lists:delete(ChannelToJoin, State#server_st.channels), [ChangedChannel])}};

				% If the user is already a member of the channel.
				true -> {{error,user_already_joined}, State}
			end
		
	end;
%
% User leaving a channel.
%
request(State, {leave, {UserID, UserPID}, ChannelName}) ->
	ListOfChannels = State#server_st.channels,
	ChannelToLeave = lists:keyfind(ChannelName,#channel.name, ListOfChannels),
	case lists:member({UserID,UserPID}, ChannelToLeave#channel.users) of 
		false ->
			{{error, user_not_joined}, State};
		true ->
			ChangedChannel = ChannelToLeave#channel{users = lists:delete({UserID,UserPID}, ChannelToLeave#channel.users)},
			{ok, State#server_st{channels = lists:append(lists:delete(ChannelToLeave, State#server_st.channels), [ChangedChannel])}}
	end;

%
% User sending a message to a channel
%
request(State, {message, {UserID,UserPID}, ChannelName, Token}) ->
	ListOfChannels = State#server_st.channels,
	ChannelToMessage = lists:keyfind(ChannelName,#channel.name, ListOfChannels),
	case catch lists:member({UserID,UserPID}, ChannelToMessage#channel.users) of
		false ->
			{{error, user_not_joined}, State};
		true -> 
			ListOfUsers = ChannelToMessage#channel.users,
			UserIDs = lists:map(fun ({X, _}) -> X end, ListOfUsers),
			UserPIDs = lists:map(fun ({_, V}) -> V end, ListOfUsers),

			[genserver:request(Pid, {message_from_server, ChannelName, UserID, Token}) || Pid <- UserPIDs, Pid /= UserPID],
			{ok, State}
	end.
	

initial_state(_Server) ->
    #server_st{users=[], channels = []}. %, messagepids=[]
