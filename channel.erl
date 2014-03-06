-module (channel).
-export ([loop/2, request/2, initial_state/2]).
-include_lib("./defs.hrl").

loop(St, _Msg) ->
	request(St, _Msg).

request(State, {join, {UserID, UserPID}}) -> 
	case lists:member({UserID,UserPID}, State#channel.users) of
				%User is not a member of the channel
				false ->
					%Add the user to the channel
					ChangedState = State#channel{users = lists:append(State#channel.users, [{UserID,UserPID}])},
					%Remove the old reference of the channel and add a new one.
					{ok, ChangedState};
				% If the user is already a member of the channel.
				true -> {{error,user_already_joined}, State}
	end;

request(State, {leave, {UserID, UserPID}}) ->
	case lists:member({UserID,UserPID}, State#channel.users) of 
		false ->
			{{error, user_not_joined}, State};
		true ->
			ChangedState = State#channel{users = lists:delete({UserID,UserPID}, State#channel.users)},
			{ok, ChangedState}
	end;

request(State, {message, {UserID,UserPID}, Token}) ->
	case catch lists:member({UserID,UserPID}, State#channel.users) of
		false ->
			{{error, user_not_joined}, State};
		true -> 
			spawn( fun() ->
			ListOfUsers = lists:delete({UserID,UserPID}, State#channel.users ),
			UserPIDs = lists:map(fun ({_, V}) -> V end, ListOfUsers),
			lists:foreach(
				fun(PID) ->
					spawn( fun() -> genserver:request(PID, {message_from_server, State#channel.name, UserID, Token}) end) 
				end, UserPIDs) end),
			{ok, State}
	end;
request(State, {user_exist, {UserID, UserPID}}) ->
	{lists:member({UserID,UserPID}, State#channel.users), State}.
	
initial_state(ChannelName, {UserID,UserPID}) -> 
	#channel{name = list_to_atom(ChannelName), users = [{UserID,UserPID}]}.
