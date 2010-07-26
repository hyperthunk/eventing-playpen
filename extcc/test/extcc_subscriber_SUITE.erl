%% -----------------------------------------------------------------------------
%%
%% Phonostroma: extcc_subsriber_SUITE
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

-module(extcc_subscriber_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("hamcrest/include/hamcrest.hrl").
-include_lib("proper/include/proper.hrl").
-include("../include/test.hrl").
-include("../include/extcc.hrl").

-compile(export_all).

-define(COLLECTOR(TC), list_to_atom(atom_to_list(TC) ++ "_collector")).

all() -> [starting_subscription_server_without_callback_handler_should_fail,
          starting_subscription_server_with_valid_callback_should_not_fail,
          subscription_server_should_track_registered_subscription_handlers,
          late_registration_via_api,
          late_registration_with_channel_state,
          registration_can_be_combined_with_subscription,
          iam_the_broadcast_driver,
          inbound_events_are_transposed_then_broadcast].

init_per_testcase(TestCase, Config) ->
    %% TODO: replace this with libtest/emock
    Pid = spawn(?MODULE, collector_loop, [[]]),
    register(?COLLECTOR(?MODULE), Pid),
    ct:pal("spawned pid [~p] to collect for testcase [~p]~n", [Pid, TestCase]),
    gen_event:start({local, ?SUBSCRIPTION_EV_MGR}),
    [{collector, Pid}|Config].

end_per_testcase(TestCase, Config) ->
  gen_event:stop(?SUBSCRIPTION_EV_MGR),
  ct:pal("cleaning up for testcase [~p]~n", [TestCase]),
  case lists:keyfind(collector, 1, Config) of
    {collector, Pid} ->
      ct:pal("killing collector [~p]~n", [Pid]),
      Pid ! {stop, self()},
      receive
        {stopping, State} ->
            ct:pal("observed collector exiting with state ~p~n", [State])
      after 10000 ->
        case erlang:is_process_alive(Pid) of
          true ->
            ct:pal("~p Timed out waiting for response from collector proc~n", [self()]);
          _ ->
            ct:pal("collector process has died!!!")
        end
      end,
      catch( unregister(?COLLECTOR(?MODULE)) ),
      exit(Pid, normal),
      ok;
    _ -> ok
  end.

starting_subscription_server_without_callback_handler_should_fail(_) ->
  Startup = extcc_subscriber:start([]),
  ?assertThat(Startup, is(equal_to({error, {config, no_broadcast_driver}}))).

starting_subscription_server_with_valid_callback_should_not_fail(Config) ->
  Pid = proplists:get_value(collector, Config),
  {ok, Server} = extcc_subscriber:start([
    {broadcast_driver, {?MODULE, [Pid]}}]),
  ?assertThat(Server, isalive()),
  %% assuming the service has started, kill it - if not this line isn't needed
  extcc_subscriber:stop(Server).
  
subscription_server_should_track_registered_subscription_handlers(Config) ->
  Pid = proplists:get_value(collector, Config),
  {ok, Server} = extcc_subscriber:start([
    {broadcast_driver, {?MODULE, [Pid]}},
    {subscribers, [?MODULE]}]),
  register(extcc_subscriber, Server),
  F = fun() -> extcc_subscriber:which_subscribers() end,
  ?assertThat(F(), is(equal_to([?MODULE])), force_stop(Server)).
  
late_registration_via_api(Config) ->
  Pid = proplists:get_value(collector, Config),
  {ok, Server} = extcc_subscriber:start([{broadcast_driver, {?MODULE, []}}]),
  register(extcc_subscriber, Server), Ctx = self(),
  ?BLOCK_UNTIL_DONE(Ctx, fun() -> extcc_subscriber:register_subscriber({?MODULE, [Pid]}) end),
  ?assertThat({open_channel, [Server,Pid]}, was_received_by(Pid), force_stop(Server)).

late_registration_with_channel_state(Config) ->
  Pid = proplists:get_value(collector, Config),
  {ok, Server} = extcc_subscriber:start([{broadcast_driver, {?MODULE, []}}]),
  register(extcc_subscriber, Server), Ctx = self(),
  ?BLOCK_UNTIL_DONE(Ctx, fun() -> extcc_subscriber:register_subscriber({?MODULE, [Pid, {state, []}]}) end),
  ?assertThat({open_channel, [Server,Pid, {state, []}]}, was_received_by(Pid), force_stop(Server)).

registration_can_be_combined_with_subscription(Config) ->
  Pid = proplists:get_value(collector, Config),
  {ok, Server} = extcc_subscriber:start([
    {broadcast_driver, {?MODULE, [Pid]}},
    {subscriptions, [{?MODULE, [Pid]}]}]),
  register(extcc_subscriber, Server), Ctx = self(),
  ?assertThat({open_channel, [Server,Pid]}, was_received_by(Pid), force_stop(Server)).

iam_the_broadcast_driver(Config) ->
  Pid = proplists:get_value(collector, Config),
  {ok, Server} = extcc_subscriber:start([
    {broadcast_driver, {?MODULE, [Pid]}},
    {subscriptions, [{?MODULE, [Pid]}]}]),
  register(extcc_subscriber, Server), Ctx = self(),
  ?assertThat({init, [Pid]}, was_received_by(Pid), force_stop(Server)).

inbound_events_are_transposed_then_broadcast(Config) ->
  Pid = proplists:get_value(collector, Config),
  {ok, Server} = extcc_subscriber:start([
    {broadcast_driver, {?MODULE, [Pid]}},
    {subscriptions, [{?MODULE, [Pid]}]}]),
  register(extcc_subscriber, Server), Ctx = self(),
  %% events are delivered using "standard' message passing
  Server ! {?MODULE, {raw_data, self(), ?MODULE}},
  ?WAIT_FOR_COLLECTOR(resume),
  ExpectedEvent = #'extcc.event'{ body={raw_data, self(), ?MODULE} },
  ?assertThat({handle_event, ExpectedEvent}, was_received_by(Pid), force_stop(Server)).  

%% TODO: need to provide a PUBLICATION hook so we can register *this* module as a callback
%% TODO: need a way to propagate state in the subscription manager (already exists?) so we can call the collector with each event

force_stop(Server) ->
  fun() ->
    catch( unregister(extcc_subscriber) ),
    ct:pal("stopping extcc_subscriber...~ndone: ~p~n",
      [extcc_subscriber:stop(Server)])
  end.

init(InitArgs) ->
  CPid = ?COLLECTOR(?MODULE),
  CPid ! {init, InitArgs},
  {ok, InitArgs}.

open_channel(Ln, [Pid|_]=State) when is_pid(Pid) ->
  ct:pal("opening channel against listener ~p~n", [Ln]),
  Pid ! {open_channel, [Ln|State]},
  open_channel(Ln, []);
open_channel(_, _) ->
  {ok, ?MODULE}.

close_channel(_,_) ->
  ignore.

transpose_event(RawInput, State) ->
  {ok, #'extcc.event'{ body=RawInput }, State}.

terminate(_Reason, _State) -> ok.

handle_event(Event, State) ->
  CPid = ?COLLECTOR(?MODULE),
  CPid ! {handle_event, Event},
  {ok, State}.

was_received_by(CollectorPid) ->
  Matcher = match_mfa(?MODULE, check_state, [CollectorPid]),
  ct:pal("generating matcher: ~p", [Matcher]),
  Matcher.

check_state(CollectorPid, Expected) ->
  StatusMsg = {status, self()},
  ct:pal("sending ~p to ~p", [StatusMsg, CollectorPid]),
  CollectorPid ! StatusMsg,
  ct:pal("awaiting response from ~p", [CollectorPid]),
  receive
    {response, State} ->
      ct:pal("checking result", []),
      Result = lists:member(Expected, State),
      ct:pal("checking ~p against collector status ~p: ~p", [Expected, State, Result]),
      Result;
    Other ->
      ct:pal("check_state received unexpected message passing [~p]", Other),
      false
  after 20000 ->
    ct:pal("Timed out waiting for response from ~p, where is_process_alive(~p) == ~p",
      [CollectorPid, CollectorPid, is_process_alive(CollectorPid)])
  end.

collector_loop(State) ->
  ct:pal("collector looping...~n", []),
  State1 =
  receive
    {status, Sender} ->
      ct:pal("sending collector status to ~p~n", [Sender]),
      Sender ! {response, State},
      State; 
    {stop, Sender} ->
      ct:pal("collector stopping...~n", []),
      Sender ! {stopping, State},
      exit(normal);
    {handle_event, #'extcc.event'{ body={raw_data, Pid, _} }}=Ev ->
      ct:pal("received event, restarting test case process ~p~n", [Pid]),
      Pid ! resume, [Ev|State];
    Other ->
      ct:pal("anon sent ~p~n", [Other]),
      [Other|State]
  after 10000 ->
    ct:pal("timeout in collector loop... stopping~n", [])
  end,
  collector_loop(State1).
