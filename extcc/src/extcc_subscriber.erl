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
%% @doc Defines a high level interface for event subscriptions.
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
%%   close_channel(ChannelRef, State) Lets the user module clean up a specific channel.
%%
%% This is a less aggressive shutdown mechanism than terminate, which lets the
%% subscriber module know that the channel service is no longer required but the
%% server itself is not actually shutting down.
%%
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
%%        {ok, Transposed, NewState}
%%        {error, Reason, NewState}
%%              Transposed = #'extcc.event'{} i.e., the transposed event
%%              Reason = term() i.e., describes the reason the operation failed
%%              NewState = term() i.e., state associated with the channel
%%
%%   terminate(ChannelRef, Reason, State) Let the user module clean up.
%%
%% An aggresive shutdown model, this function is always called when this server terminates.
%% Calls to terminate should return in a timely fashion as the server might not be able to
%% restart until all (user) subscriber modules have fully terminated.
%%
%%    ==> ok
%%              ChannelRef = term() e.g., a means for the callback module to
%%                                  uniquely identify the channel (can be the module name)
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
%%     unregister       ----->                       .
%%     close_channel    <-----                       .
%%                  or
%%     stop             ----->                       .
%%     terminate        <-----                       .
%%
%%
%% Once opened, a channel is responsible for publishing events by sending
%% messages (terms) to the listener (i.e., the server). The server maintains the
%% state of each channel and co-ordinates with the user module(s) to transpose
%% the raw inputs (received as erlang messages) into event records and subsequently
%% publishes them to an intermediary, using the gen_event mechanism. The event
%% manager is registered by the atom 'extcc.subscription.manager' and is
%% registered locally by default. You can override this behaviour during startup
%% by providing either the atom global (to globally register the default handler)
%% or by providing a custom event handler name, in which case a pre-started
%% event manager assuming the supplied name is assumed.
%%
%% -----------------------------------------------------------------------------

-module(extcc_subscriber).

-ifdef(TEST).
-compile(export_all).
-endif.

%%
%% What Am I?
%%

-behavior(gen_server).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([behaviour_info/1]).

-export([start_link/0,
         start/1,
         stop/0,
         stop/1]).

-export([dump/1,
        which_subscribers/0,
         register_subscriber/1,
         register_subscriber/2]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

%% ------------------------------------------------------------------
%% Prelude
%% ------------------------------------------------------------------

-include("../include/extcc.hrl").

-define(E_NODRIVER, {error, {config, no_broadcast_driver}}).
-define(E_BADDRIVER, {error, {config, bad_broadcast_driver}}).

-type(callback_option() :: atom() | {atom(), term()}).

-define(SERVER, ?MODULE).

-record(state, {
    listener                  :: pid() | atom() | {atom(), atom()} | {global, atom()},
    options       = []        :: [{atom(), term()}],
    subscribers   = []        :: [#'extcc.subscriber'{}],
    terminations  = []        :: [term()]
}).

%% ------------------------------------------------------------------
%% Custom Behaviour Definition
%% ------------------------------------------------------------------

behaviour_info(callbacks) ->
  [{open_channel, 2},
   {close_channel, 2},
   {transpose_event, 2},
   {terminate, 3}];
behaviour_info(_Other) ->
  undefined.

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).
  
%% -----------------------------------------------------------------------------
%% @doc Starts a subscription server.
%% start(Options)
%%    Options ::= [{broadcast_driver, CallbackOpts} |
%%                 {subscribers, [SubscriberDef]} |
%%                 {options, [term()]}]
%%      CallbackOpts ::= Module | {Module, Id::term()}
%%    SubscriberDef ::= Module
%%    Module ::= atom()
%%
%% Returns: {ok, Pid} |
%%          {error, {already_started, Pid}} |
%%          {error, Reason}
%%              Reason ::= {config, ConfigError::term()}
%%              ConfigError ::= no_broadcast_driver | unknown
%% -----------------------------------------------------------------------------
-spec(start/1 :: ([{broadcast_driver, [callback_option()]} |
                   {subscribers, [atom()]} |
                   {options, [term()]}]) ->
                        {ok, pid()} |   
                        {error, {already_started, pid()}} |
                        {error, {config, term()}} |
                        {error, {startup, term()}}).
start(Options) ->
  %% does the caller supply me with a gen_event callback module to be registered?
  %% how do I know whether I should start a subscription manager myself or not?
  do_start(broadcast_driver, proplists:get_value(broadcast_driver, Options), Options).

%% @hidden
do_start(broadcast_driver, {Mod, InitArgs}, Options) when is_atom(Mod) andalso is_list(InitArgs) ->
  case gen_event:add_handler(?SUBSCRIPTION_EV_MGR, Mod, InitArgs) of
    ok -> 
      do_start(server, Options);
    {'EXIT', Reason} ->
      {error, {startup, Reason}};
    StartupFailure ->
      {error, {startup, StartupFailure}}
  end;
do_start(broadcast_driver, undefined, _) -> ?E_NODRIVER;
do_start(broadcast_driver, _, _) -> ?E_BADDRIVER.

%% @hidden
do_start(server, Options) ->
  case gen_server:start(?MODULE, Options, gen_server_options(Options)) of
    {ok,Server}=Started ->
      startup_subscriptions(Server, proplists:get_value(subscriptions, Options)),
      Started;
    Other ->
      {error, {startup, {"gen_server startup failed", Other}}}
  end.
  
%% @hidden
startup_subscriptions(Server, [{_, _}=Subscription|Rest]) ->
  register_subscriber(Server, Subscription),
  startup_subscriptions(Server, Rest);
startup_subscriptions(_, _) ->
  ok.

%% -----------------------------------------------------------------------------
%% @doc Stops a subscription server.
%% Returns: term() i.e., server status
%% -----------------------------------------------------------------------------
-spec(stop/0 :: () -> term()).
stop() ->
  stop(?SERVER).

%% -----------------------------------------------------------------------------
%% @doc Stops a subscription server.
%% stop(Server)
%%    Server ::= Name || pid()
%%      Name ::= atom()
%% Returns: term() i.e., server status
%% -----------------------------------------------------------------------------
-spec(stop/1 :: (atom() | pid()) -> term()).
stop(Name) when is_atom(Name) ->
    gen_server:cast(Name, stop);
stop(Pid) when is_pid(Pid) ->
    gen_server:cast(Pid, stop).

dump(Server) ->
  gen_server:call(Server, dump).

%% -----------------------------------------------------------------------------
%% @doc Returns a list of all registered subscription plugins
%% Returns: [module()] i.e., a list of registered subscription callback modules
%% -----------------------------------------------------------------------------
which_subscribers() ->
  gen_server:call(?SERVER, {get_config, subscribers}).

%% -----------------------------------------------------------------------------
%% @doc Registers a subscriber callback module.
%% register_subscriber(Spec)
%%    Spec ::= {Module, InitArgs}
%%      Module ::= atom()
%%      InitArgs ::= term()
%%
%% Returns: {ok, Subscriber} |
%%          {stopping, e_subscription_failed} |
%%          ignored
%%              Subscriber ::= #'extcc.subscriber'{}
%% -----------------------------------------------------------------------------
register_subscriber({_Mod, _InitArgs}=Spec) ->
  register_subscriber(?SERVER, Spec).

%% -----------------------------------------------------------------------------
%% @doc Registers a subscriber callback module against the supplied server.
%% register_subscriber(Server, Spec)
%%    Server ::= pid() | RegisteredName::atom()
%%    Spec ::= {Module, InitArgs}
%%      Module ::= atom()
%%      InitArgs ::= term()
%%
%% Returns: {ok, Subscriber} |
%%          {stopping, e_subscription_failed} |
%%          ignored
%%              Subscriber ::= #'extcc.subscriber'{}
%% -----------------------------------------------------------------------------
register_subscriber(Server, Spec) ->
  gen_server:call(Server, {register, Spec}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Args) ->
  Listener = proplists:get_value(listener, Args, self()),
  {ok, #state{ options=Args, listener=Listener }}.

handle_call(dump, _, State) ->
  ct:pal("state = [~p] ~n", [State]),
  {reply, ok, State};
handle_call({get_config, Key}, _,
    #state{ options=Opts }=State) ->
  {reply, proplists:get_value(Key, Opts), State};
handle_call({register, {Mod, InitArgs}}, _,
    #state{ subscribers=Subs, listener=Listener }=State) ->
  case Mod:open_channel(Listener, InitArgs) of
    {ok, ChannelRef} ->
      Subscription =
      #'extcc.subscriber'{ channel=ChannelRef, mod=Mod, init=InitArgs},
      {reply, {ok, Subscription},
        State#state{ subscribers=[Subscription|Subs] }};
    {ok, ChannelRef2, ChannelState} ->
      StatefulSubscription =
      #'extcc.subscriber'{ channel=ChannelRef2, state=ChannelState,
                            mod=Mod, init=InitArgs},
      {reply, {ok, StatefulSubscription},
        State#state{ subscribers=[StatefulSubscription|Subs] }};
    ignore -> 
      {reply, ignored, State};
    {stop, Reason} ->
      {stop, Reason, {stopping, e_subscription_failed}, State}
  end;
handle_call(_Request, _From, State) ->
  {noreply, ok, State}.

handle_cast(stop, State) ->
  ct:pal("asked to stop in state ~p~n", [State]),
  {stop, normal, State#state{ terminations=terminate_all_channels(State) }};
handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info({ChannelRef, Event}, #state{ subscribers=Subs }=State) ->
  case find_subscriber(ChannelRef, Subs) of
    {error, not_found} ->
      {stop, {error, {no_channel, ChannelRef, Event}}, State};
    {error, not_unique_cref} -> %% TODO: where are the tests for this!? :/
      {stop, {error, {not_unique_cref, ChannelRef, Event, State}}, State};
    #'extcc.subscriber'{ mod=Mod, state=S } ->
      case Mod:transpose_event(Event, S) of
        {ok, Transposed} ->
          publish(Transposed),
          {noreply, State};
        {ok, Transposed, NewState} ->
          publish(Transposed),
          {noreply, NewState};
        {error, Reason, NewState} ->
          {stop, {error, {conversion_failed, {ChannelRef, Event, Reason}}, NewState}}
      end
  end;
handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

terminate_all_channels(#state{ subscribers=Subs }) ->
  lists:map(fun terminate_channel/1, Subs).

terminate_channel(#'extcc.subscriber'{ channel=Chan, mod=Mod, state=State }) ->
  Mod:terminate(Chan, normal, State).

publish(Event) ->
  gen_event:notify(?SUBSCRIPTION_EV_MGR, Event).

find_subscriber(ChannelRef, Subscribers) ->
  P = fun(#'extcc.subscriber'{ channel=Chan }) -> Chan == ChannelRef end,
  case extcc_util:find(P, Subscribers) of
    [_|_] -> {error, not_unique_cref};
    [] -> {error, not_found};
    #'extcc.subscriber'{}=Subscriber -> Subscriber
  end.

%% @hidden
gen_server_options(Options) ->
  lists:filter(fun({debug, _}) -> true;
                  ({timeout, _}) -> true;
                  ({spawn_opt, _}) -> true;
                  (_) -> false
               end, Options).
