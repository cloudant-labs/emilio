-module(bad).
-behavior(gen_server).


-export([
    init/1,
    terminate/2,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    code_change/3
]).


init(_Arg) ->
    {ok, undefined}.


terminate(_Reason, _State) ->
    ok.


handle_call(Msg, _From, State) ->
    NewState = do_private_helper(Msg),
    {reply, ok, NewState}.


do_private_helper(State) ->
    State.


handle_cast(_Msg, State) ->
    {noreply, State}.


handle_info(_Msg, State) ->
    {noreply, State}.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
