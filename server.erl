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
    
   
messagehandler(State, Data) -> 
    case Data of         
    {join, Client, Channel} -> 
        case proplists:lookup(Channel, State#server_st.channels) of
            {Channel, Pid} -> try genserver:request(Pid, {join, Client}) of
                Result -> {reply, Pid, State}
                catch 
                    {'EXIT', Reason} -> {'EXIT', Reason}
                end;
            _ -> NewPid = spawn(fun() -> genserver:loop(new_channel(Channel, Client), fun channelhandler/2) end),
                {reply, NewPid, #server_st{
                    server = State#server_st.server,
                    channels = [{Channel, NewPid} | State#server_st.channels],
                    nicks = State#server_st.nicks}}             
         end;
    {leave, Client, Channel} -> 
        case proplists:lookup(Channel, State#server_st.channels) of
            {Channel, Pid} -> try genserver:request(Pid, {leave, Client}) of
                Result -> {reply, Result, State}
                    catch 
                        %{'EXIT', Reason} -> {'EXIT', Reason}
                        {'EXIT', Reason} -> {reply, {error, server_not_reached, Reason}, State}
                    end;
            _  -> {reply, {error, user_not_joined, "Channel does not exist in server."}, State}
        end;
    _  -> {reply, {error, user_not_joined, "Not a valid server request"}, State}
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
                true -> spawn(fun() -> genserver:request(Client , {message_receive, ChannelState#channel_st.channel, Nick, Msg}) end),
                    {reply, ok, ChannelState};
                _ -> {reply, {error, user_not_joined ,"user not a part of channel"}, ChannelState}
                end;
        _ -> {reply, {error, server_not_reached, "not a valid channel request"}, ChannelState} 
    end.



sendMsgToClients(St, Msg, Nick, [Member | Members]) -> 
    case genserver:request(Member, {message_receive, St#channel_st.channel, Nick, Msg}) of
        ok -> sendMsgToClients(St, Msg, Nick, Members);
        _ -> {reply, {error, server_not_reached, "no reply from server"}, St}
    end;
    % {message_receive, Channel, Nick, Msg}
sendMsgToClients(St, _, _, []) -> 
    {reply, ok, St}.


% Stop the server process registered to the given name,
% together with any other associated processes
stop(ServerAtom) -> 
    % TODO Implement function
    % Return ok
    genserver:stop(ServerAtom),
    ok.
    
