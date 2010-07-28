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

-module(extcc_util_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("hamcrest/include/hamcrest.hrl").
-include_lib("proper/include/proper.hrl").
-include("../include/test.hrl").
-include("../include/extcc.hrl").

-compile(export_all).

-import(extcc_util, [find/2]).

all() -> ?CT_REGISTER_TESTS(?MODULE).

find_should_unpack_head_when_result_is_single_elem_list(_) ->
  P = ?FORALL(XS, list(integer(0, 10)), 
        ?IMPLIES(length(XS) > 0,
        begin
          ?assertThat(find(fun(X) -> X > 10 end, [15|XS]), is(equal_to(15)))
        end)),
	?EQC(P).
  
find_should_return_complete_results_when_more_than_one_element_is_found(_) ->
  P = ?FORALL(XS, list(integer(0, 10)),
        ?IMPLIES(length(XS) > 1,
          begin
            P = fun(X) -> X >= 0 andalso X =< 10 end,
            ?assertThat(length(find(P, XS)), is(equal_to(length(XS))))
          end)),
	?EQC(P).
  
%%find_behaves_like_filter_for_empty_list(_) ->
%%  ?assertThat(find(fun(X) -> false end, []), isempty()).
