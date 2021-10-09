-module(client).
-export([handle/2, initial_state/3]).
-import(proplists, [lookup/2]).

% This record defines the structure of the state of a client.
% Add whatever other fields you need.
-record(client_st, {
    gui, % atom of the GUI process
    nick, % nick/username of the client
    server, % atom of the chat server
    channelList
}).

% Return an initial state record. This is called from GUI.
% Do not change the signature of this function.
initial_state(Nick, GUIAtom, ServerAtom) ->
    #client_st {
        gui = GUIAtom,
        nick = Nick,
        server = ServerAtom,
        channelList = []
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
    case catch genserver:request(St#client_st.server, {join, self(), Channel}) of
        {'EXIT', Reason} -> {reply, {error, server_not_reached, Reason}, St};
        {error, user_already_joined, ErrorMsg} -> {reply, {error, user_already_joined, ErrorMsg}, St};
        Result -> {reply, ok, St#client_st{channelList = [{Channel, Result} | St#client_st.channelList]}}
   end;   
 
% Join channel
%handle(St, {join, Channel}) ->
%    try genserver:request(St#client_st.server, {join, self(), Channel}) of        
%        Result -> {reply, ok, St#client_st{channelList = [{Channel, Result} | St#client_st.channelList]}}
%    catch 
%        exit:{'EXIT', Reason} -> {reply, {error, server_not_reached, Reason}, St};
%        error:{error, user_already_joined, ErrorMsg} -> {reply, {error, user_already_joined, ErrorMsg}, St}
%    end;    
 
% Leave channel
handle(St, {leave, Channel}) ->
    case catch genserver:request(St#client_st.server, {leave, self(), Channel}) of
        {'EXIT', _} -> {reply, ok, St#client_st{channelList = proplists:delete(Channel, St#client_st.channelList)}};
        %{'EXIT', Reason} -> {reply, {error, server_not_reached, Reason}, St};
        {error, user_not_joined, ErrorMsg} -> {reply, {error, user_not_joined, ErrorMsg}, St};
        Result -> {reply, ok, St#client_st{channelList = proplists:delete(Result, St#client_st.channelList)}}
    end;
    
% Sending message (from GUI, to channel)
handle(St, {message_send, Channel, Msg}) ->
    case catch genserver:request(St#client_st.server, {checkChannel, Channel}) of
            true -> case proplists:lookup(Channel, St#client_st.channelList) of
                    {Channel, Pid} ->   case catch genserver:request(Pid, {message, self(), St#client_st.nick, Msg}) of
                                            ok -> {reply, ok, St};
                                            {error, user_not_joined, ErrorText} -> {reply, {error, user_not_joined, ErrorText}, St};
                                            {error, server_not_reached, ErrorText} -> {reply, {error, server_not_reached, ErrorText}, St};
                                            Result -> {reply, {error, server_not_reached, Result}, St}
                                        end;  
                    _ ->  {reply, {error, user_not_joined, "User has not joined channel yet"}, St}
                    end;
            %{'EXIT', _} -> {reply, {error, server_not_reached, "Server timed"}, St};        
            {'EXIT', _} -> case proplists:lookup(Channel, St#client_st.channelList) of
                                            {Channel, Pid} ->   case catch genserver:request(Pid, {message, self(), St#client_st.nick, Msg}) of
                                                                    {'EXIT', Reason} -> {reply, {error, server_not_reached, Reason}, St};
                                                                    Result -> {reply, Result, St}
                                                                end;
                                                _ ->  {reply, {error, user_not_joined, "User has not joined channel yet"}, St}
                                        end;    
            false -> {reply, {error, server_not_reached, "No such channel exists"}, St}
    end;
    
%
            
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
handle(St, _) ->
    {reply, {error, not_implemented, "Client does not handle this command"}, St} .
