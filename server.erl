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
%    gui : start(ServerAtom),
%    gui : init(ServerAtom, shire),
    Pid.
    
messagehandler(State, Data) -> 
    case Data of         
    {join, Client, Channel} -> 
         case lists:member(Channel, State#server_st.channels) of
            true -> Ref = make_ref(),
                Channel ! {request, self(), Ref, {join, Client}}, 
                receive
                    {exit, Ref, Reason} -> {'EXIT', Reason};
                    {result, Ref, R} -> {reply, R, State}
                end;
            false -> genserver:start(Channel, new_channel(Channel, Client), fun channelhandler/2), 
                     {reply, Channel, #server_st{
                        server = State#server_st.server, 
                        channels = [Channel | State#server_st.channels], 
                        nicks = State#server_st.nicks}}
         end;
    {leave, Client, Channel} -> 
        case lists:member(Channel, State#server_st.channels) of
            true -> Ref = make_ref(),
                Channel ! {request, self(), Ref, {leave, Client}},
                    receive
                        {exit, Ref, Reason} -> {'EXIT', Reason};
                        {result, Ref, R} -> {reply, R, State}
                    end;
            false -> {'EXIT', "Channel does not exist in server."}
        end;
            _ -> {reply, "Hej", State}
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
        _ -> {reply, "Hej", ChannelState}
    end.



% Stop the server process registered to the given name,
% together with any other associated processes
stop(ServerAtom) -> 
    % TODO Implement function
    % Return ok
    genserver:stop(ServerAtom),
    ok.
    
