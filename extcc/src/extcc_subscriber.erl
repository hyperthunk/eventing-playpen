%% -----------------------------------------------------------------------------
%%
%% Phonostroma: extcc_subsriber
%%
%% -----------------------------------------------------------------------------
%% Copyright (c) 2010 Tim Watson (watson.timothy@gmail.com)
%%
%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%% THE SOFTWARE.
%% -----------------------------------------------------------------------------
%% @doc Defines a high level interface for event subscription providers.
%%
%% The idea behind THIS server is that the callback module provides
%% functions to open and close channels and to convert channel inputs into
%% event records compatible with our specific API.
%%
%% The user module should export the following callback functions:
%%
%%   open_channel(Listener, Options) Asks the user module to open a channel
%%          Listener = Name | {Name, Node} | {global, GlobalName} | pid()
%%              Name = atom()
%%          Options = term()
%%
%%     ==> {ok, ChannelRef}
%%         {ok, ChannelRef, State}
%%         ignore
%%         {stop, Reason}
%%              ChannelRef = term() e.g., a means for the callback module to
%%                                  uniquely identify the channel (can be the module name)
%%              State = term() i.e., state associated with the channel
%%              Reason = term() i.e., describes the reason the operation failed
%%
%%   close_channel(ChannelRef, State) Lets the user module clean up a specific channel
%%              ChannelRef = term() e.g., a means for the callback module to
%%                                  uniquely identify the channel (can be the module name)
%%              State = term() i.e., state associated with the channel
%%
%%     ==> ok || ignore
%%
%%   transpose_event(RawInput, State) Transposes the supplied RawInput into an event record
%%              RawInput = term() i.e., some raw input received directly from the channel
%%              State = term() i.e., state associated with the channel
%%
%%    ==> {ok, Transposed}
%%        {ok, Transposed, State}
%%        {error, Reason, State}
%%              Transposed = #'extcc.event'{} i.e., the transposed event
%%              Reason = term() i.e., describes the reason the operation failed
%%              State = term() i.e., state associated with the channel
%%
%%   terminate(Reason, State) Let the user module clean up
%%        always called when this server terminates
%%
%%    ==> ok
%%              Reason = term() i.e., describes the reason the operation failed
%%              State = term() i.e., state associated with the channel
%%
%%
%% The work flow (of the server) can be described as follows:
%%
%%   User module                   Channel          Server
%%   -----------                                    -------
%%     start            ----->                      start
%%                                                  loop
%%     register         ----->                      register
%%     open_channel     <-----                       .
%%                      ----->     (starts)
%%
%%                                Server ! Term      .
%%     transpose_event  <-----                       .
%%                      ----->                       .
%%     stop             ----->                       .
%%     terminate        <-----                       .
%%
%%
%% Once opened, a channel is responsible for publishing events by sending
%% messages (terms) to the listener (i.e., the server). The server maintains the
%% state of each channel and co-ordinates with the user module(s) to transpose
%% the raw inputs (received as erlang messages) into event records and subsequently
%% publishes them to an intermediary, defined during startup by the supplying the
%% {callback, {Mod,Func}} configuration tuple, where Mod is the module and Func
%% the function to be called to publish transposed events.
%% -----------------------------------------------------------------------------

-module(extcc_subscriber).

-ifdef(TEST).
-compile(export_all).
-endif.

%% Client API Exports
-export([start/1]).

-export([behaviour_info/1]).

behaviour_info(callbacks) ->
    [{open_channel, 2},
     {close_channel, 2},
     {transpose_event, 2},
     {terminate, 2}];
behaviour_info(_Other) ->
    undefined.

%% -----------------------------------------------------------------------------
%% Starts a subscription server.
%% start(Options)
%%    Options ::= [{callback, {Mod, Func}} | {subscribers, [SubscriberDef]}]
%%    SubscriberDef ::= {subscriber, Module}
%%
%% Returns: {ok, Pid} |
%%          {error, {already_started, Pid}} |
%%          {error, Reason}
%%              Reason ::= {config, ConfigError::term()}
%% -----------------------------------------------------------------------------
-spec(start/1 :: ([{callback, {atom(), atom()}} |
                   {subscribers, [{subscriber, atom()}]}]) -> {ok, pid()} |
                                                              {error, {already_started, pid()}} |
                                                              {error, {config, term()}}).
start([]) ->
    {error, {config, no_callback}};
start(_Options) ->
    ok.
