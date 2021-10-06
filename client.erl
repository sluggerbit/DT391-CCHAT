-module(client).
-export([handle/2, initial_state/3]).

% This record defines the structure of the state of a client.
% Add whatever other fields you need.
-record(client_st, {
    gui, % atom of the GUI process
    nick, % nick/username of the client
    server % atom of the chat server
}).

% Return an initial state record. This is called from GUI.
% Do not change the signature of this function.
initial_state(Nick, GUIAtom, ServerAtom) ->
    #client_st {
        gui = GUIAtom,
        nick = Nick,
        server = ServerAtom
    }.




%clientloop(State, Data) ->
%    receive 
%        {request, Data} ->  
%            handle(State, Data)
%    end.


% ah nu fattar jag vad du menar tror jag, om det bara hade gällt typ ett "ping" så hade man inte behövt ha receive i klienten
% dvs att det kommer direkt tillbaka till samma Ref
% jo verkar som dom startar en gemensam process där för gui och client, för client:handle/2 hanterar det den tar emot och 
% använder client state som state, så vi behöver nog inte mer server där om inte vi ska ha en per channel, fast behövs nog inte maybe idk 


% om du kollar där nere vid handle msg


% handle/2 handles each kind of request from GUI
% Parameters:
%   - the current state of the client (St)
%   - request data from GUI
% Must return a tuple {reply, Data, NewState}, where:
%   - Data is what is sent to GUI, either the atom `ok` or a tuple {error, Atom, "Error message"}
%   - NewState is the updated state of the client

% Join channel
handle(St, {join, Channel}) ->
    try genserver:request(St#client_st.server, {join, self(), Channel}) of
        Result -> {reply, ok, St}
    catch
       {'EXIT', Reason} -> {reply, {error, server_not_reached, Reason}, St}
    end;
        
% Leave channel
handle(St, {leave, Channel}) ->
    try genserver:request(St#client_st.server, {leave, self(), Channel}) of
        Result -> {reply, ok, St}
    catch
       {'EXIT', Reason} -> {reply, {error, server_not_reached, Reason}, St}
    end;
    
% Sending message (from GUI, to channel)
handle(St, {message_send, Channel, Msg}) ->
    try genserver:request(Channel, {message, self(), Msg}) of
        Result -> {reply, ok, St}
    catch
       {'EXIT', Reason} -> {reply, {error, server_not_reached, Reason}, St}
    end;

            
% This case is only relevant for the distinction assignment!
% Change nick (no check, local only)
handle(St, {nick, NewNick}) ->
    {reply, ok, St#client_st{nick = NewNick}};

% ---------------------------------------------------------------------------
% The cases below do not need to be changed...
% But you should understand how they work!

% Get current nick
handle(St, whoami) ->
    {reply, St#client_st.nick, St} ;

% Incoming message (from channel, to GUI)
handle(St = #client_st{gui = GUI}, {message_receive, Channel, Nick, Msg}) ->
    gen_server:call(GUI, {message_receive, Channel, Nick++"> "++Msg}),
    {reply, ok, St} ;

% Quit client via GUI
handle(St, quit) ->
    % Any cleanup should happen here, but this is optional
    {reply, ok, St} ;

% Catch-all for any unhandled requests
handle(St, Data) ->
    {reply, {error, not_implemented, "Client does not handle this command"}, St} .
