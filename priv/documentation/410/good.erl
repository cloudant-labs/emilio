-module(good).
-behavior(gen_server).


-export([
    start_link/0,
    do_thing/1
]).

-export([
    init/1,
    terminate/2,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    code_change/3
]).


start_link() ->
    gen_server:start_link(?MODULE, [], []).


do_thing(Pid) ->
    gen_server:call(Pid, do_thing).


init(_Arg) ->
    {ok, undefined}.


terminate(_Reason, _State) ->
    ok.


handle_call(_Msg, _From, State) ->
    {reply, ok, State}.


handle_cast(_Msg, State) ->
    {noreply, State}.


handle_info(_Msg, State) ->
    {noreply, State}.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
