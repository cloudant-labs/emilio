-module(good).
-behavior(application).
-behavior(gen_server).


-export([
    init/1,
    terminate/2,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    code_change/3
]).

-export([
    start/2,
    stop/1
]).


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


start(_, _) ->
    sup_mod:start_link().


stop(_) ->
    ok.
