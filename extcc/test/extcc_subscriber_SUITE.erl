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
          late_registration_via_api].

init_per_testcase(TestCase, Config) ->
    %% TODO: replace this with libtest/emock
    Pid = spawn(?MODULE, collector_loop, [[]]),
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
            ct:pal("Collector exiting with state ~p~n", [State])
      after 10000 ->
        case erlang:is_process_alive(Pid) of
          true ->
            ct:pal("~p Timed out waiting for response from collector proc~n", [self()]);
          _ ->
            ct:pal("collector process has died!!!")
        end
      end,
      exit(Pid, normal),
      ok;
    _ -> ok
  end.

starting_subscription_server_without_callback_handler_should_fail(_) ->
  Startup = extcc_subscriber:start([]),
  ?assertThat(Startup, is(equal_to({error, {config, no_broadcast_driver}}))).

starting_subscription_server_with_valid_callback_should_not_fail(Config) ->
  Pid = proplists:get_value(collector, Config),
  CallbackOptions = [Pid],
  ct:pal("starting subscription manager", []),
  {ok, Server} = extcc_subscriber:start([
    {broadcast_driver, {?MODULE, [Pid]}}]),
  ?assertThat(Server, isalive()),
  %% assuming the service has started, kill it - if not this line isn't needed
  extcc_subscriber:stop(Server).
  
subscription_server_should_track_registered_subscription_handlers(Config) ->
  Pid = proplists:get_value(collector, Config),
  CallbackOptions = [Pid],
  ct:pal("starting subscription manager", []),
  {ok, Server} = extcc_subscriber:start([
    {broadcast_driver, {?MODULE, [Pid]}},
    {subscribers, [?MODULE]}]),
  register(extcc_subscriber, Server),
  F = fun() -> extcc_subscriber:which_subscribers() end,
  ?assertThat(F(), is(equal_to([?MODULE])), force_stop(Server)).
  
late_registration_via_api(Config) ->
  Pid = proplists:get_value(collector, Config),
  ct:pal("starting subscription manager", []),
  {ok, Server} = extcc_subscriber:start([{broadcast_driver, {?MODULE, []}}]),
  register(extcc_subscriber, Server), Ctx = self(),
  ?BLOCK_UNTIL_DONE(Ctx, fun() -> extcc_subscriber:register_subscriber({?MODULE, [Pid]}) end),
  ct:pal("applying message passing assertions", []),
  ?assertThat(open_channel, was_received_by(Pid), force_stop(Server)).

%% TODO: need to provide a PUBLICATION hook so we can register *this* module as a callback
%% TODO: need a way to propagate state in the subscription manager (already exists?) so we can call the collector with each event

force_stop(Server) ->
  fun() ->
    catch( unregister(extcc_subscriber) ),
    extcc_subscriber:stop(Server)
  end.

init(InitArgs) ->
  ct:pal("gen_event handler init/1 called", []),
  {ok, InitArgs}.

open_channel(Ln, [Pid|_]) when is_pid(Pid) ->
  ct:pal("opening channel against listener ~p~n", [Ln]),
  Pid ! open_channel,
  open_channel(Ln, []);
open_channel(_, _) ->
  {ok, ?MODULE}.

close_channel(_,_) ->
  ignore.

transpose_event(RawInput, State) ->
  {ok, #'extcc.event'{ body=RawInput }, State}.

terminate(_Reason, _State) -> ok.

handle_event(Event, State) ->
  {ok, State}.

was_received_by(CollectorPid) ->
  Matcher = match_mfa(?MODULE, check_state, [CollectorPid]),
  ct:pal("generating matcher: ~p", [Matcher]),
  Matcher.

check_state(CollectorPid, Expected) ->
  %% {response, State} needs to be gotten via recieve!!!
  %% CollectorPid ! {status, self()},
  %% ?WAIT_FOR(CollectorPid, {status, self()}, {response, State}, infinite),
  StatusMsg = {status, self()},
  ct:pal("sending ~p to ~p", [StatusMsg, CollectorPid]),
  CollectorPid ! StatusMsg,
  receive
    {response, State} ->
      ct:pal("status response from collector: ~p", [State]),
      lists:member(State, Expected);
    Other ->
      ct:pal("check_state received unexpected message passing [~p]", Other),
      false
  end.

collector_loop(State) ->
  ct:pal("collector looping...~n", []),
  State1 = receive
    {Cmd, Sender} ->
      ct:pal("collector ~p received ~p from ~p~n", [self(), Cmd, Sender]),
      case Cmd of
          status ->
            ct:pal("sending collector status to ~p~n", [Sender]),
            Sender ! {response, State};
          stop ->
            ct:pal("collector stopping...~n", []),
            Sender ! {stopping, State},
            exit(normal)
      end,
      State;
    Other ->
      ct:pal("anon sent ~p~n", [Other]),
      [Other|State]
  after 10000 ->
    ct:pal("timeout in collector loop... stopping~n", [])
  end,
  collector_loop(State1).
