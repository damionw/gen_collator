%% Module declaration
-module(file_collator).

%% Behaviour
-behaviour(gen_collator).

%% Exports
-export([start/3, close/1, stop/1, push/3, raise/2]).

%% gen_collator interface exports
-export([handle_init/1, handle_delivery/2, handle_shutdown/1, handle_completion/1, handle_failure/2]).

%%======================================================
%%               Record Definitions
%%======================================================
% Provides the Testing context
-record(
    server_state, {
        filehandle,
        client
    }
).

%% Lager compilation settings
-compile([{parse_transform, lager_transform}]).

%%======================================================
%%          Instance-based Client Interface
%%======================================================
start(Filename, Start, Finish) ->
    gen_collator:start_link(?MODULE, [self(), Filename, Start, Finish]).

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
handle_init([Client, Filename, Floor, Limit]) ->
    lager:info("Instance of ~s started on node ~s Pid=~p writing to ~p with floor=~p limit=~p", [?MODULE, node(), self(), Filename, Floor, Limit]),

    {ok, Filehandle} = file:open(Filename, [raw, write]),

    State = #server_state{
        filehandle = Filehandle,
        client = Client
    },

    {ok, Floor, Limit, State}.

handle_delivery(Payload, State=#server_state{}) ->
    Filehandle = State#server_state.filehandle,

    case file:write(Filehandle, io_lib:format("~p~n", [Payload])) of
        ok ->
            true;

        {error, Reason} ->
            raise(self(), Reason)
    end,

    State#server_state.client ! {data, Payload},
    {ok, State}.

handle_shutdown(State=#server_state{}) when (State#server_state.filehandle /= undefined) ->
    Filehandle = State#server_state.filehandle,
    file:close(Filehandle),

    NewState = State#server_state{
        filehandle = undefined
    },

    {ok, NewState};

handle_shutdown(State=#server_state{}) ->
    {ok, State}.

handle_completion(State=#server_state{}) ->
    State#server_state.client ! {finished},
    {close, State}.

handle_failure(Exception, State=#server_state{}) ->
    State#server_state.client ! {Exception},
    {close, State}.
