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
%% API compatible event records.
%%
%% The user module should export the following callback functions:
%%
%%   open_channel(Listener, Options)
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
%%   close_channel(ChannelRef, Options)
%%              ChannelRef = term() e.g., a means for the callback module to
%%                                  uniquely identify the channel (can be the module name)
%%              Options = term()
%%
%%     ==> ok || ignore
%%
%%   transpose_event(Input TODO: word this property and finish off the api, State)
%%
%%    ==> {noreply, State}
%%        {noreply, State, Timeout}
%%        {stop, Reason, State}
%%              Reason = normal | shutdown | Term, terminate(State) is called
%%
%%   terminate(Reason, State) Let the user module clean up
%%        always called when server terminates
%%
%%    ==> ok
%%
%%
%% The work flow (of the server) can be described as follows:
%%
%%   User module                          Generic
%%   -----------                          -------
%%     start            ----->             start
%%     init             <-----              .
%%
%%                                         loop
%%     handle_call      <-----              .
%%                      ----->             reply
%%
%%     handle_cast      <-----              .
%%
%%     handle_info      <-----              .
%%
%%     terminate        <-----              .
%%
%%                      ----->             reply
%%
%%
%% -----------------------------------------------------------------------------

-module(extcc_subscriber).

-export([behaviour_info/1]).

behaviour_info(callbacks) ->
    [{start, 2},
     {stop, 1}];
behaviour_info(_Other) ->
    undefined.

-ifdef(TEST).
-compile(export_all).
-endif.
