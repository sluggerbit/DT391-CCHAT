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




clientloop(State, Data) ->
    receive 
        {request, Data} ->  
            handle(State, Data)
    end.


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
    % TODO: Implement this function
    % {reply, ok, St} ;
    {reply, {error, not_implemented, "join not implemented"}, St} ;
%  fun () -> request(ClientName, {join, Channel})

% Leave channel
handle(St, {leave, Channel}) ->
    % TODO: Implement this function
    % {reply, ok, St} ;
    {reply, {error, not_implemented, "leave not implemented"}, St} ;

% Sending message (from GUI, to channel)
handle(St, {message_send, Channel, Msg}) ->
    % TODO: Implement this function
Ref = make_ref(),
receive case Channel ! {request, self(), Ref, "Hej"} of  % ska inte client ha en request funktion?
     {exit, Ref, Reason} -> {reply, {error, not_implemented, Reason}, St}; % vid varje case-fall ska det vara ett semikolon
    {result, Ref, R} -> {reply, ok, St} % förutom det sista som inte ska ha något
end.   % ska sluta med end, punkt för att det är slutet på funktionen


% ger upp lite och äter, du kan sitta mer om du vill, lämnar det uppe, säg till om du avslutar



    % Varför får vi channel i argumenten?
    % så man kan skicka direkt till channels på servern antar jag
    
    % ahh ändrade till channels från server nu, aha ok
    % får inte syntaxen att fungera riktigt den BORDE fungera såhär
    
    %    error
    %else
    % {reply, ok, St} ;
    %{reply, {error, not_implemented, "message sending not implemented"}, St} ;

             
%   Här har jag påbörjat, jag modellerade efter hur de hanterade nytt nick precis nedanför lite men har
%   inte fått syntaxen att gå ihop, vi ska förändra klientens status på något sätt (eller kanalens?)             
             
% Här tror jag vi kan skicka ett meddelande till servern, vänta på svar, om ok, returnera ok till guit, om server säger att det sket sig, returnera fel
% tror inte vi behöver ändra klienten state, bara om vi ändrar nick, yes
% låter rimligt, men frågan är också hur vi svarar genom att ändra klientens status bara
% svaret är reply, {ok, state} ah ok så oförändrat bara felhantering

% This case is only relevant for the distinction assignment!
% Change nick (no check, local only)
handle(St, {nick, NewNick}) ->
    {reply, ok, St#client_st{nick = NewNick}} ;

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
