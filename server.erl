-module(server).
-export([start/1,stop/1]).
-import(proplists, [lookup/2]).

% -------------------- RECORDS ------------------------------

-record(server_st, {
    server, % atom of the chat server
    channels, % list of all channels in tuples: {channel atom, pid} 
    nicks % All connected user's nicks
}).

initial_state(ServerAtom) ->
    #server_st {
        server = ServerAtom,
        channels = [],
        nicks = []
    }.

-record(channel_st, { 
    channel, % atom of the channel server
    members % list of all members of channel
}).


new_channel(ChannelAtom, Member) ->
    #channel_st {
        channel = ChannelAtom,
        members = [Member]
    }.
% -------------------- SERVER FUNCTIONS ------------------------------      

% Starts a new server process with the given name
% Do not change the signature of this function.
start(ServerAtom) ->
    % - Spawns a new process which waits for a message, handles it, then loops infinitely
    % - Registers this process to ServerAtom
    % - Returns the process ID
    Pid = genserver:start(ServerAtom, initial_state(ServerAtom), fun messagehandler/2),
    Pid.  
    
% Server loop
%   - takes 2 params : server state, request message
%   - returns a triple: reply atom, response message content, new server state 
messagehandler(St, Data) -> 
    case Data of 
    
    % Checks if nick already exists in server
    {nick, OldNick, NewNick} -> case lists:member(NewNick, St#server_st.nicks) of
        true -> {reply, nick_taken, St};
        _ -> {reply, ok, St#server_st{nicks = [NewNick | lists:delete(OldNick, St#server_st.nicks)]}}
        end; 
        
    % Sends a stop message to all channels in server
    stopChannels ->
        lists:foreach(fun stopChannel/1, St#server_st.channels),
        {reply, ok, St#server_st{channels = []}};
        
    % Checks if channel exists in server    
    {checkChannel, Channel} -> 
        case proplists:lookup(Channel, St#server_st.channels) of
            {Channel, _} -> {reply, true, St};
            _ -> {reply, false, St}
        end;       
         
    % Join channel
    {join, {Client, Nick}, Channel} -> 
        % Checks if channel already exists
        case proplists:lookup(Channel, St#server_st.channels) of
            {Channel, Pid} -> try genserver:request(Pid, {join, Client}) of
                {error, user_already_joined, ErrorMsg} -> {reply, {error, user_already_joined, ErrorMsg}, St};
                _ -> {reply, Pid, St#server_st{nicks = [Nick | St#server_st.nicks]}}
                catch 
                    {'EXIT', Reason} -> {'EXIT', Reason}
                end;
            % If no channel exists, spawns a new channel process
            _ -> NewPid = spawn(fun() -> genserver:loop(new_channel(Channel, Client), fun channelhandler/2) end),
                {reply, NewPid, #server_st{channels = [{Channel, NewPid} | St#server_st.channels], nicks = [Nick | St#server_st.nicks]}}         
         end;
         
    % Leave channel
    {leave, Client, Channel} -> 
        % Checks if channel already exists
        case proplists:lookup(Channel, St#server_st.channels) of
            % If it exists, requests /leave from that channel
            {Channel, Pid} -> try genserver:request(Pid, {leave, Client}) of
                Result -> {reply, Result, St}
                    catch 
                        {'EXIT', Reason} -> {reply, {error, server_not_reached, Reason}, St}
                    end;
            _  -> {reply, user_not_joined, St}
        end;
    _  -> {reply, server_not_reached, St}
    end.       
    
% -------------------- CHANNEL FUNCTIONS ------------------------------

% Channel loop
% - takes 2 params : channel state, request message
% - returns a triple: reply atom, response message content, new channel state 
channelhandler(ChannelState, Data) ->
    case Data of
        % Join channel
        {join, Client} -> 
            % Checks if client is a member of the channel
            case lists:member(Client, ChannelState#channel_st.members) of
                true -> {reply, user_already_joined, ChannelState};
                % Adds client to channels member list
                _ -> {reply, ChannelState#channel_st.channel, #channel_st{channel = ChannelState#channel_st.channel, members = [Client | ChannelState#channel_st.members]}}
            end;
        % Leave channel
        {leave, Client} -> 
            % Checks if client is a member of the channel
            case lists:member(Client, ChannelState#channel_st.members) of
                % Removes client from channels member list
                true -> {reply, ChannelState#channel_st.channel, #channel_st{channel = ChannelState#channel_st.channel, members = lists:delete(Client, ChannelState#channel_st.members)}};
                _ -> user_not_joined 
            end;
        % Message channel
        {message, Client, Nick, Msg} -> 
            % Checks if client is a member of the channel
            case lists:member(Client, ChannelState#channel_st.members) of
                % Sends message to all channel members
                true ->  spawnMsgRec(ChannelState, Msg, Nick, lists:delete(Client, ChannelState#channel_st.members));
                _ -> {reply, user_not_joined, ChannelState}
            end;
        _ -> {reply, server_not_reached, ChannelState} 
    end.      
    
            % Message channel helper
            % - takes 4 params : channel state, message content, nickname, list of channel members
            % - returns triple : reply atom, ok atom, channel state
            % Recursively spawns a new message process for sending a message to all client          
            spawnMsgRec(St, Msg, Nick, [Member | Members]) -> spawnMsg(St, Member, Nick, Msg), spawnMsgRec(St, Msg, Nick, Members);
            % End of recursion
            spawnMsgRec(St, _, _, []) -> {reply, ok, St}.
                % Spawns a new process ending when message is sent 
                spawnMsg(St, Client, Nick, Msg) -> spawn(fun() -> sendAMsg(St, Client, Nick, Msg) end).
                % Sends a message to the clients gui forwarding function
                sendAMsg(St, Client, Nick, Msg) -> case genserver:request(Client, {message_receive, St#channel_st.channel, Nick, Msg}) of
                    % Nothing more for the process if nothing's wrong
                    ok -> exit(normal);
                    % Tells us if something went wrong with the process
                    _ -> exit("Something went wrong while sending messages to clients")
                end.

% -------------------- STOP FUNCTIONS ------------------------------

% Send a stop message to the PID of a channel
stopChannel({_, Pid}) -> spawn(fun() -> Pid ! stop , ok end). 

% Stop the server process registered to the given name,
% together with any other associated processes
stop(ServerAtom) -> 
    % Stop channels
    genserver:request(ServerAtom, stopChannels),
    % Stop server
    genserver:stop(ServerAtom),
    ok.
    
