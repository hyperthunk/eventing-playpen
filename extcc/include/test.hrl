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

-define(CT_REGISTER_TESTS(Mod),
	All = [ FName || {FName, _} <- lists:filter(
      fun ({module_info,_}) -> false ;
          ({all,_}) -> false ;
          ({init_per_suite,1}) -> false ;
          ({end_per_suite,1}) -> false ;
          ({_,1}) -> true ;
          ({_,_}) -> false
      end,
      Mod:module_info(exports)
    )
  ],
  ct:pal("registering ~p~n", [All]),
  All).
  
-define(EQC(P),
  case code:lib_dir(eqc, include) of
    {error, bad_name} ->
      proper:check(P);
    _ ->
      eqc:check(P)
  end).

-define(WAIT_FOR_COLLECTOR(Term),
  begin
    ?TDEBUG("suspending test cast process on selective receive...~n", []),
    receive Term -> ok end,
    ?TDEBUG("test cast process resuming...~n", [])
  end).

-define(BLOCK_UNTIL_DONE(Sender, Code),
	F = fun() -> Code(), Sender ! done end,
	F(), receive done -> ok end).

-define(TDEBUG(Pattern, Args),
  ct:pal("~p: " ++ Pattern, [self()|Args])).
