
-module(bad).
-behavior(gen_server).

-export([
    terminate/2,
    handle_call/3,
    code_change/3,
    handle_cast/2,
    init/1,
    handle_info/2
]).


terminate(_Reason, _State) ->
    ok.


handle_call(_Msg, _From, State) ->
    {reply, ok, State}.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


handle_cast(_Msg, State) ->
    {noreply, State}.


init(_Arg) ->
    {ok, undefined}.


handle_info(_Msg, State) ->
    {noreply, State}.
