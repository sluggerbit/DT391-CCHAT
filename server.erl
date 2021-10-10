-module(server).
-export([start/1,stop/1]).
-import(proplists, [lookup/2]).

-record(server_st, {
    server, % atom of the chat server
    channels, % list of all channels PIDs
    nicks % All connected user's nicks
}).

initial_state(ServerAtom) ->
    #server_st {
        server = ServerAtom,
        channels = [],
        nicks = []
    }.

-record(channel_st, { % atom of the channel server
    channel, 
    members 
}).

new_channel(ChannelAtom, Member) ->
    #channel_st {
        channel = ChannelAtom,
        members = [Member]
    }.

% Start a new server process with the given name
% Do not change the signature of this function.
start(ServerAtom) ->
    % TODO Implement function
    % - Spawn a new process which waits for a message, handles it, then loops infinitely
    % - Register this process to ServerAtom
    % - Return the process ID
    Pid = genserver:start(ServerAtom, initial_state(ServerAtom), fun messagehandler/2),
    Pid.
    
   
messagehandler(St, Data) -> 
    case Data of 
    {nick, OldNick, NewNick} -> case lists:member(NewNick, St#server_st.nicks) of
        true -> {reply, nick_taken, St};
        _ -> {reply, ok, St#server_st{nicks = [NewNick | lists:delete(OldNick, St#server_st.nicks)]}}
        end; 
    stopChannels ->
        lists:foreach(fun stopChannel/1, St#server_st.channels),
        {reply, ok, St#server_st{channels = []}};
        
    {checkChannel, Channel} -> 
        case proplists:lookup(Channel, St#server_st.channels) of
            {Channel, _} -> {reply, true, St};
            _ -> {reply, false, St}
        end;        
    {join, {Client, Nick}, Channel} -> 
        case proplists:lookup(Channel, St#server_st.channels) of
            {Channel, Pid} -> try genserver:request(Pid, {join, Client}) of
                {error, user_already_joined, ErrorMsg} -> {reply, {error, user_already_joined, ErrorMsg}, St};
                _ -> {reply, Pid, St#server_st{nicks = [Nick | St#server_st.nicks]}}
                catch 
                    {'EXIT', Reason} -> {'EXIT', Reason}
                end;
            _ -> NewPid = spawn(fun() -> genserver:loop(new_channel(Channel, Client), fun channelhandler/2) end),
                {reply, NewPid, #server_st{channels = [{Channel, NewPid} | St#server_st.channels], nicks = [Nick | St#server_st.nicks]}}         
         end;
    {leave, Client, Channel} -> 
        case proplists:lookup(Channel, St#server_st.channels) of
            {Channel, Pid} -> try genserver:request(Pid, {leave, Client}) of
                Result -> {reply, Result, St} % här
                    catch 
                        % {'EXIT', Reason} -> {'EXIT', Reason}
                        {'EXIT', Reason} -> {reply, {error, server_not_reached, Reason}, St}
                    end;
            _  -> {reply, {error, user_not_joined, "Channel does not exist in server."}, St}
        end;
    _  -> {reply, {error, user_not_joined, "Not a valid server request"}, St}
    end.       
    
channelhandler(ChannelState, Data) ->
    case Data of
        {join, Client} -> 
            case lists:member(Client, ChannelState#channel_st.members) of
                true -> {reply, {error, user_already_joined, "tried to join while already a member in channel"}, ChannelState};
                _ -> {reply, ChannelState#channel_st.channel, #channel_st{channel = ChannelState#channel_st.channel, members = [Client | ChannelState#channel_st.members]}}
            end;
        {leave, Client} -> 
            case lists:member(Client, ChannelState#channel_st.members) of
                true -> {reply, ChannelState#channel_st.channel, #channel_st{channel = ChannelState#channel_st.channel, members = lists:delete(Client, ChannelState#channel_st.members)}};
                _ -> {reply, {error, user_not_joined, "tried to leave a channel while not a member of that channel"}, ChannelState} 
            end;
        {message, Client, Nick, Msg} -> 
            case lists:member(Client, ChannelState#channel_st.members) of
                true ->  spawnMsgRec(ChannelState, Msg, Nick, lists:delete(Client, ChannelState#channel_st.members));
                _ -> {reply, {error, user_not_joined ,"user not a part of channel"}, ChannelState}
            end;
        _ -> {reply, {error, server_not_reached, "not a valid channel request"}, ChannelState} 
    end.      
         
spawnMsgRec(St, Msg, Nick, [Member | Members]) -> spawnMsg(St, Member, Nick, Msg), spawnMsgRec(St, Msg, Nick, Members);
spawnMsgRec(St, _, _, []) -> {reply, ok, St}.

spawnMsg(St, Client, Nick, Msg) -> spawn(fun() -> sendAMsg(St, Client, Nick, Msg) end).

sendAMsg(St, Client, Nick, Msg) -> case genserver:request(Client, {message_receive, St#channel_st.channel, Nick, Msg}) of
    ok -> exit(normal);
    _ -> exit("not normal")
end.

stopChannel({_, Pid}) -> spawn(fun() -> Pid ! stop , ok end). 

% Stop the server process registered to the given name,
% together with any other associated processes
stop(ServerAtom) -> 
    % TODO Implement function
    % Return ok
    genserver:request(ServerAtom, stopChannels),
    % lists:foreach(fun , St#server_st.channels),
    genserver:stop(ServerAtom),
    ok.
    
