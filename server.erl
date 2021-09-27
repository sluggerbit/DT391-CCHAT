-module(server).
-export([start/1,stop/1]).
%-import(genserver, [start/3, stop/1]).

% Start a new server process with the given name
% Do not change the signature of this function.
start(ServerAtom) ->
    % TODO Implement function
    % - Spawn a new process which waits for a message, handles it, then loops infinitely
    % - Register this process to ServerAtom
    % - Return the process ID
    gui : start(ServerAtom),
    Pid = genserver:start(ServerAtom, 0, messagehandler()),
    Pid.
messagehandler() -> 
    receive Msg -> 
    io:format("received: ~p~n", [Msg]) end,
    messagehandler().

% Stop the server process registered to the given name,
% together with any other associated processes
stop(ServerAtom) -> 
    % TODO Implement function
    % Return ok
    genserver:stop(ServerAtom),
    ok.
    
