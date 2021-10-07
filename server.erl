-module(server).
-export([start/1,stop/1]).
%-import(genserver, [start/3, stop/1]).

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
    % Record = 1,
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
        % case lists:member(Channel, State#server_st.channels) of
        %    true -> try genserver:request(Channel, {join, Client}) of
        %                Result -> {reply, Result, State}
        %            catch 
        %                {'EXIT', Reason} -> {'EXIT', Reason}
        %            end;
        %    _ -> Pid = spawn(fun() -> genserver:loop(new_channel(Channel, Client), fun channelhandler/2) end),
        %        {reply, ok, #server_st{
        %                server = State#server_st.server, 
        %                channels = [Pid | State#server_st.channels], 
        %                nicks = State#server_st.nicks}}             
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
            %_ -> {'EXIT', {user_not_joined, "Channel does not exist in server."}}
            % {reply, {error, user_not_joined, Reason}, State}
        %case lists:member(Channel, State#server_st.channels) of
        %    true -> try genserver:request(Channel, {leave, Client}) of
        %        Result -> {reply, Result, State}
        %        catch 
        %            {'EXIT', Reason} -> {'EXIT', Reason} 
        %        end;
        %    false -> {'EXIT', "Channel does not exist in server."}
        end;
    %_ -> {'EXIT', "Not a valid server request"}
    _  -> {reply, {error, user_not_joined, "Not a valid server request"}, State}
    end.    
    
    
            
    % NewClient = #server_st{
    %        server = State#server_st.server, 
    %        channels = [#channel_st{
    %                channel = N#channel_st.channel, 
    %                members = [Client|N#channel_st.members]} || N <- State#server_st.channels], 
    %        nicks = State#server_st.nicks},
         
         %[element(1, N) || N <- State#server_st.channels]
         
    % ChannelState = new_channel(Channel, Client),
    %     NewClientAndChannel = #server_st{
    %         server = State#server_st.server, 
    %         channels = [#channel_st{channel = Channel, members = [Client|#channel_st.members]} | State#server_st.channels], 
    %         nicks = State#server_st.nicks},
    
    
    
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
        {message, Client, Msg} -> 
            case lists:member(Client, ChannelState#channel_st.members) of
                true -> sendMsgToClients(Msg, ChannelState#channel_st.members),
                        {reply, ok, ChannelState};
                _ -> {reply, {error, user_not_joined ,"user not a part of channel"}, ChannelState}
                end;
        _ -> {reply, {error, server_not_reached, "not a valid channel request"}, ChannelState} 
    end.


sendMsgToClients(Msg, [Member | Members]) -> 
    genserver:request(Member, {message, self(), Msg}), 
    sendMsgToClients(Msg, Members);
sendMsgToClients(_, []) -> 
    ok.
    
   %for(0,_) -> 
   %[]; 
   
  % for(N,Term) when N > 0 -> 
  % io:fwrite("Hello~n"), 
  % [Term|for(N-1,Term)]. 

% Stop the server process registered to the given name,
% together with any other associated processes
stop(ServerAtom) -> 
    % TODO Implement function
    % Return ok
    genserver:stop(ServerAtom),
    ok.
    
