-module(client).
-export([handle/2, initial_state/3]).
-import(proplists, [lookup/2]).

% -------------------- RECORDS ------------------------------      

% This record defines the structure of the state of a client.
% Add whatever other fields you need.
-record(client_st, {
    gui, % atom of the GUI process
    nick, % nick/username of the client
    server, % atom of the chat server
    channelList % list of channels client is a member of
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
    
% -------------------- HANDLE FUNCTIONS ------------------------------          

% Join channel
handle(St, {join, Channel}) ->
    % Tries to join channel
    case catch genserver:request(St#client_st.server, {join, {self(), St#client_st.nick}, Channel}) of
        %--------- ERRORS  
        % Server is unreachable
        {'EXIT', Reason} -> {reply, {error, server_not_reached, Reason}, St};
        % Client is already a member of channel
        user_already_joined -> {reply, {error, user_already_joined, "Client is already a member of the channel"}, St};
        
        %--------- SUCCESS  
        % Succesfully joined
        Result -> {reply, ok, St#client_st{channelList = [{Channel, Result} | St#client_st.channelList]}}
   end;   
 
% Leave channel
handle(St, {leave, Channel}) ->
    % Tries to leave channel
    case catch genserver:request(St#client_st.server, {leave, self(), Channel}) of
        %--------- ERRORS    
        % Client is not a member of channel
        user_not_joined -> {reply, {error, user_not_joined, "Client is not a member of this channel."}, St};
        % Channel is unreachable
        {error, server_not_reached, Reason} -> {reply, {error, server_not_reached, Reason}, St};
            
        %--------- SUCCESS
        % Server is unreachable, successfully leaves channel
        {'EXIT', _} -> {reply, ok, St#client_st{channelList = proplists:delete(Channel, St#client_st.channelList)}};
        % Successfully leaves channel
        Result -> {reply, ok, St#client_st{channelList = proplists:delete(Result, St#client_st.channelList)}}
    end;
    
% Sending message (from GUI, to channel)
handle(St, {message_send, Channel, Msg}) ->
    % Checks if channel exists
    case catch genserver:request(St#client_st.server, {checkChannel, Channel}) of
            % Channel has not yet been created
            false -> {reply, {error, server_not_reached, "No such channel exists"}, St};
            % Server is unreachable, and/or channel exists
            _ -> case proplists:lookup(Channel, St#client_st.channelList) of
                                            % Tries to send a message to channel
                                            {Channel, Pid} ->   case catch genserver:request(Pid, {message, self(), St#client_st.nick, Msg}) of
                                                                    %--------- SUCCESS
                                                                    % Succesfully sent a message
                                                                    ok -> {reply, ok, St};
                                                                    
                                                                    %--------- ERRORS 
                                                                    % Channel process is unreachable
                                                                    {'EXIT', Reason} -> {reply, {error, server_not_reached, Reason}, St};
                                                                    % Client not a part of channels member list
                                                                    user_not_joined -> {reply, {error, user_not_joined, "User is not part of channel"}, St};
                                                                    % Sent an invalid request to channel
                                                                    server_not_reached -> {reply, server_not_reached, "not a valid channel request"};
                                                                    % Error code catch-all
                                                                    Result -> {reply, Result, St}
                                                                end;
                                                _ ->  {reply, {error, user_not_joined, "User has not joined channel yet"}, St}
                                        end
    end;
            
% This case is only relevant for the distinction assignment!
% Change nick (no check, local only)
handle(St, {nick, NewNick}) -> 
% Tries to change nick
case genserver:request(St#client_st.server, {nick, St#client_st.nick, NewNick}) of 
        %--------- SUCCESS
        % Successfully changed nick
        ok -> {reply, ok, St#client_st{nick = NewNick}};
        
        %--------- ERRORS
        % Nick already taken
        nick_taken -> {reply, {error, nick_taken, "Nick already taken."}, St};
        % Server is unreachable
        {'EXIT', Reason} -> {reply, {error, server_not_reached, Reason}, St}
    end;

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
