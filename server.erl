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
        _ -> {reply, "Hej", State}
    end.    


%    {join, Channel} -> 
%        if sets: is_element(Channel, set),
%            messagehandler()

            
% messagehandler(State, Data).

%startChannel(ChannelName) ->
%    PidChannel = genserver:start(ChannelName),
%    PidChannel.

%channelhandler() ->
    
%    ok.

% Stop the server process registered to the given name,
% together with any other associated processes
stop(ServerAtom) -> 
    % TODO Implement function
    % Return ok
    genserver:stop(ServerAtom),
    ok.
    
