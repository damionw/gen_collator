%% Module declaration
-module(gen_collator_server).

%% Behaviour
-behaviour(gen_collator).

%% Exports
-export([start_link/0, start_link/1, start_link/2]).
-export([start_registered/0, start_registered/1, start_registered/2]).
-export([close/0, close/1]).
-export([stop/0, stop/1]).
-export([push/3, push/2]).
-export([raise/1, raise/2]).

%% gen_collator interface exports
-export([handle_init/1, handle_delivery/2, handle_shutdown/1, handle_completion/1, handle_failure/2]).

%%======================================================
%%               Record Definitions
%%======================================================
% Provides the Testing context
-record(
    server_state, {
        client
    }
).

%% Lager compilation settings
-compile([{parse_transform, lager_transform}]).

%%======================================================
%%               Registered Client Interface
%%======================================================
start_registered() ->
    start_registered(0, infinity).

start_registered(Limit) ->
    start_registered(0, Limit).

start_registered(Floor, Limit) ->
    gen_collator:start_registered(?MODULE, [self(), Floor, Limit]).

push(Order, Payload) ->
    push(?MODULE, Order, Payload).

raise(Error) ->
    raise(?MODULE, Error).

close() ->
    close(?MODULE).

stop() ->
    stop(?MODULE).

%%======================================================
%%          Instance-based Client Interface
%%======================================================
start_link() ->
    start_link(0, infinity).

start_link(Limit) ->
    start_link(0, Limit).

start_link(Floor, Limit) ->
    gen_collator:start_link(?MODULE, [self(), Floor, Limit]).

push(Instance, Order, Payload) ->
    gen_collator:push(Instance, Order, Payload).

raise(Instance, Error) ->
    gen_collator:raise(Instance, Error).

close(Instance) ->
    gen_collator:close(Instance).

stop(Instance) ->
    gen_collator:stop(Instance).

%%======================================================
%%               Simple_Server Implementation
%%======================================================
handle_init([Client, Floor, Limit]) ->
    lager:info("Instance of ~s started on node ~s Pid=~p with floor=~p limit=~p", [?MODULE, node(), self(), Floor, Limit]),

    State = #server_state{
        client = Client
    },

    {ok, Floor, Limit, State}.

handle_delivery(Payload, State=#server_state{}) ->
    State#server_state.client ! {data, Payload},
    {ok, State}.

handle_shutdown(State=#server_state{}) ->
    {ok, State}.

handle_completion(State=#server_state{}) ->
    State#server_state.client ! {finished},
    {close, State}.

handle_failure(Exception, State=#server_state{}) ->
    State#server_state.client ! {Exception},
    {close, State}.
