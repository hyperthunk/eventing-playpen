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
          starting_subscription_server_with_valid_callback_should_not_fail].

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
      after 10000
        -> ct:pal("Timed out waiting for response from collector proc~n", [])
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
    {broadcast_driver, {?MODULE, [Pid]}},
    {subscribers, [?MODULE]}]),
  ?assertThat(Server, isalive()).
  %%register(extcc_subscriber, Server),
  %%?assertThat(extcc_subscriber:which_subscribers(),
  %%  is(equal_to({subscribers, [?MODULE]}))).

%% TODO: need to provide a PUBLICATION hook so we can register *this* module as a callback
%% TODO: need a way to propagate state in the subscription manager (already exists?) so we can call the collector with each event

init(InitArgs) ->
  ct:pal("gen_event handler init/1 called", []),
  {ok, InitArgs}.

open_channel(_,_) ->
  {ok, ?MODULE}.

close_channel(_,_) ->
  ignore.

transpose_event(RawInput, State) ->
  {ok, #'extcc.event'{ body=RawInput }, State}.

terminate(Reason, State) -> ok.

handle_event(Event, State) ->
  {ok, State}.

collector_loop(State) ->
  ct:pal("collector looping...~n", []),
  receive
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
      end;
    Other ->
      ct:pal("anon sent ~p~n", [Other]),
      collector_loop([Other|State])
  after 10000 ->
    ct:pal("timeout in collector loop... stopping~n", [])
  end.
