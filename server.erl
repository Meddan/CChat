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
			io:format("user is connected \n"),
			UserInChannel = [ [lists:member({UserID,UserPID}, Chan#channel.users)] || Chan <- State#server_st.channels],
			[io:fwrite("~n we have: ~w ~n", Bool) || Bool <- UserInChannel],
			io:fwrite("~n listmember: ~w~n", [lists:member([true], UserInChannel)] ),
			case catch lists:member([true], UserInChannel) of
				true -> 
					io:format("errors :D:D:D"),
					{{error, leave_channels_first}, State};
				false ->
					io:format("user not in a channel"),
					NewUserList = lists:delete({UserID,UserPID}, State#server_st.users),
					{ok, State#server_st{users = NewUserList}}

			end		
			%UserPIDtoDelete = lists:keysearch(UserID, 1, State#server_st.messagepids),
			%NewMessageList = lists:delete(UserPIDtoDelete, State#server_st.users),
			%, messagepids = NewMessageList
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
			NewChannel = #channel{name = ChannelName, users = [{UserID,UserPID}] },
			io:format(NewChannel#channel.name),
			NewChannelList = lists:append([NewChannel], ListOfChannels),
			%Add the channel to the list of channels and return it.
			{ok, State#server_st{channels = NewChannelList}};
		% Channel exists.
		_asd ->
			io:format("CHANNEL EXISTS \n"),
			case lists:member({UserID,UserPID}, ChannelToJoin#channel.users) of
				%User is not a member of the channel
				false ->
					io:format("USER NOT IN CHANNEL \n"),
					%Add the user to the channel
					ChangedChannel = ChannelToJoin#channel{users = lists:append(ChannelToJoin#channel.users, [{UserID,UserPID}])},
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
	case lists:member({UserID,UserPID}, ChannelToLeave#channel.users) of 
		false ->
			io:format("USER TRIED LEAVING CHANNEL THEY HAVEN'T JOINED \n"),
			{{error, user_not_joined}, State};
		true ->
			io:format("USER LEAVING CHANNEL \n"),
			ChangedChannel = ChannelToLeave#channel{users = lists:delete({UserID,UserPID}, ChannelToLeave#channel.users)},
			io:format("USER REMOVED FROM CHANNEL \n"),
			{ok, State#server_st{channels = lists:append(lists:delete(ChannelToLeave, State#server_st.channels), [ChangedChannel])}}
	end;

%
% User sending a message to a channel
%
request(State, {message, {UserID,UserPID}, ChannelName, Token}) ->
	ListOfChannels = State#server_st.channels,
	io:format("Gonna send the message \n"),
	ChannelToMessage = lists:keyfind(ChannelName,#channel.name, ListOfChannels),
	case catch lists:member({UserID,UserPID}, ChannelToMessage#channel.users) of
		false ->
			io:format("ASHDJAFHJKAFNKNASFKNSFKNSAKFNKJASFNKJSNFKJNSFKJSNFKJANSFKANSFKJNSJKNSFJKNASKFJNSAKFJN"),
			{{error, user_not_joined}, State};
		true -> io:format("keysearch ok in message \n"),
			ListOfUsers = ChannelToMessage#channel.users,
			io:format("list of users created \n"),
			UserIDs = lists:map(fun ({X, _}) -> X end, ListOfUsers),
	%HelperPIDs = [],

			UserPIDs = lists:map(fun ({_, V}) -> V end, ListOfUsers),
			io:format("list of pids created \n"),
			io:fwrite("~npid: ~w~n", [UserPID]),
			io:format("~nchannelname: ~w~n", [ChannelName]),
			io:format("~nuserid: ~w~n", [UserID]),
			io:format("~ntoken: ~w~n", [Token]),
			io:format("~nUserPIDS: ~w~n", [UserPIDs]),

			[genserver:request(Pid, {message_from_server, ChannelName, UserID, Token}) || Pid <- UserPIDs, Pid /= UserPID],
	

			io:format("messages sent \n"),
			{ok, State}
	end.
	

initial_state(_Server) ->
    #server_st{users=[], channels = []}. %, messagepids=[]
