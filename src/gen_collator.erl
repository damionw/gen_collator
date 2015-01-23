%% Copyright 2013 Damion K. Wilson
%% 
%% Module declaration
-module(gen_collator).

%% Client side invokable methods
-export([behaviour_info/1, service_start/2, service_loop/2]).

%% Generic Collator methods
-export([start_link/1, start_link/2]).
-export([start_registered/1, start_registered/2, start_registered/3]).
-export([stop/1]).
-export([push/3, close/1, raise/2]).

%% Includes
-include_lib("lager/include/lager.hrl").

%%======================================================
%%               Compilation Options
%%======================================================
%% Lager compilation settings
-compile([{parse_transform, lager_transform}]).

%%======================================================
%%               Record Definitions
%%======================================================
% Provides the Testing context
-record(
    behaviour_state, {
        lastvalue = infinity,
        limit = infinity,
        closed = false,
        module_name,
        floor = 0,
        exception,
        store
    }
).

%%======================================================
%%               Behaviour Interface
%%======================================================
behaviour_info(callbacks) ->
    [
        {handle_init, 1},
        {handle_shutdown, 1},
        {handle_delivery, 2},
        {handle_completion, 1},
        {handle_failure, 2}
    ];

behaviour_info(_Other) ->
    undefined.

%%======================================================
%%               Service Creation
%%======================================================
start_registered(ModuleName) ->
    start_registered(ModuleName, []).

start_registered(ModuleName, Arguments) ->
    start_registered(ModuleName, ModuleName, Arguments).

start_registered(RegisterName, ModuleName, Arguments) ->
    {ok, Pid} = start_link(ModuleName, Arguments),
    register(RegisterName, Pid),
    {ok, Pid}.

start_link(ModuleName) ->
    start_link(ModuleName, []).

start_link(ModuleName, Arguments) ->
    Pid = spawn_link(?MODULE, service_start, [ModuleName, Arguments]),
    {ok, Pid}.

%%======================================================
%%               Client Interface
%%======================================================
raise(Instance, Error) ->
    send_signal(Instance, {'$_exception', [Error]}).

close(Instance) ->
    send_signal(Instance, {'$_close', []}).

stop(Instance) ->
    send_signal(Instance, {'$_stop', []}).

push(Instance, Order, Payload) ->
    send_signal(Instance, {'$_push', [self(), Order, Payload]}).

%%======================================================
%%               Simple_Server Implementation
%%======================================================
service_start(ModuleName, Arguments) ->
    {ok, Floor, Limit, ServiceState} = apply(ModuleName, handle_init, [Arguments]),

    BehaviourState = #behaviour_state{
        module_name = ModuleName,
        store = orddict:new(),
        floor = Floor,
        limit = Limit
    },

    service_loop(BehaviourState, ServiceState).

service_loop(BehaviourState=#behaviour_state{}, ServiceState) ->
    ModuleName = BehaviourState#behaviour_state.module_name,

    {NewBehaviourState, NewServiceState} = receive
        {'$_push', [_From, Order, Payload]} ->
            _BehaviourState = flush(record(Order, Payload, BehaviourState)),
            {_BehaviourState, ServiceState};

        {'$_deliver', [Payload]} ->
            {ok, _ServiceState} = apply(ModuleName, handle_delivery, [Payload, ServiceState]),
            {BehaviourState, _ServiceState};

        {'$_completion', []} ->
            {Option, _ServiceState} = apply(ModuleName, handle_completion, [ServiceState]),

            case Option of
                close ->
                    send_stop();
                _ ->
                    true
            end,

            {BehaviourState, _ServiceState};

        {'$_exception', [Details]} ->
            {_Option, _ServiceState} = apply(ModuleName, handle_failure, [Details, ServiceState]),
            send_stop(),
            {BehaviourState, _ServiceState};

        {'$_close', [Exception]} ->
            _BehaviourState = flush(BehaviourState#behaviour_state{closed=true, exception=Exception}),
            {_BehaviourState, ServiceState};

        {'$_close', []} ->
            _BehaviourState = flush(BehaviourState#behaviour_state{closed=true}),
            {_BehaviourState, ServiceState};

        {'$_stop', _} ->
            {ok, _ServiceState} = apply(ModuleName, handle_shutdown, [ServiceState]),
            erlang:exit(normal),
            {BehaviourState, _ServiceState}
    end,

    service_loop(NewBehaviourState, NewServiceState).

%%======================================================
%%         Unordered data submission
%%======================================================
record(Order, Payload, BehaviourState=#behaviour_state{}) ->
    Floor = BehaviourState#behaviour_state.floor,
    record(Order, Payload, Floor, BehaviourState).

record(Order, _, Floor, BehaviourState=#behaviour_state{}) when (Order < Floor) ->
    lager:warning("Ignoring duplicate of processed sequence# ~p under floor ~p", [Order, Floor]),
    BehaviourState;

record(Order, Payload, Floor, BehaviourState=#behaviour_state{}) when (Order == Floor) ->
    lager:debug("Got sequence# ~p in order = ~p", [Order, Payload]),

    % Forward the ordered payload
    send_delivery(Payload),

    % If the series lastvalue has been emitted, then send a finished message
    check_order(Order, BehaviourState),

    NewState = BehaviourState#behaviour_state{
        floor = Order + 1
    },

    NewState;

record(Order, Payload, Floor, BehaviourState=#behaviour_state{}) when (Order > Floor) ->
    Store = BehaviourState#behaviour_state.store,

    case orddict:is_key(Order, Store) of
        % Handle out of order duplicates
        true ->
            lager:warning("Ignoring out of order duplicate sequence# ~p", [Order]),
            BehaviourState;

        _ ->
            lager:debug("Buffering sequence# ~p = ~p", [Order, Payload]),

            BehaviourState#behaviour_state{
                store = orddict:store(Order, Payload, Store)
            }
    end.

%%======================================================
%%          Order sequence generation
%%======================================================
flush(BehaviourState=#behaviour_state{}) ->
    Floor = BehaviourState#behaviour_state.floor,
    Store = BehaviourState#behaviour_state.store,
    flush(get_sequence(Floor, Store), BehaviourState).

flush([], BehaviourState=#behaviour_state{}) when (BehaviourState#behaviour_state.closed == true) ->
    Exception = BehaviourState#behaviour_state.exception,
    Store = BehaviourState#behaviour_state.store,
    Limit = BehaviourState#behaviour_state.limit,
    Floor = BehaviourState#behaviour_state.floor,

    case Exception of
        undefined ->
            RemainingItems = orddict:size(Store),

            if
                RemainingItems > 0 ->
                    send_incomplete();

                (Limit /= infinity) and (Floor =< Limit) ->
                    send_incomplete();

                true ->
                    send_finished()
            end;

        _ ->
            send_exception(Exception)
    end,

    BehaviourState;

flush([], BehaviourState=#behaviour_state{}) ->
    BehaviourState;

flush([Order|Remaining], BehaviourState=#behaviour_state{}) ->
    Store = BehaviourState#behaviour_state.store,

    {ok, Payload} = orddict:find(Order, Store),

    % Send next value
    send_delivery(Payload),

    % If the series lastvalue has been reached, then send a finished message
    check_order(Order, BehaviourState),

    NewBehaviourState = BehaviourState#behaviour_state{
        store = orddict:erase(Order, Store),
        floor = Order + 1
    },

    flush(Remaining, NewBehaviourState).

%%======================================================
%%                  Limit Checking
%%======================================================
% If the series lastvalue has been reached, then send a finished message
check_order(Order, BehaviourState=#behaviour_state{}) when (Order >= BehaviourState#behaviour_state.limit) ->
    LastValue = BehaviourState#behaviour_state.limit,
    lager:debug("Automatic close on emitting expected final value ~p", [LastValue]),
    send_finished(),
    ok;

check_order(_Order, _BehaviourState=#behaviour_state{}) ->
    ok.

%%======================================================
%%                Message generation
%%======================================================
send_delivery(Payload) ->
    send_signal({'$_deliver', [Payload]}).

send_finished() ->
    send_signal({'$_completion', []}).

send_exception(Details) ->
    send_signal({'$_exception', [Details]}).

send_incomplete() ->
    send_exception(incomplete).

send_stop() ->
    stop(self()).

send_signal(Message) ->
    send_signal(self(), Message).

send_signal(Instance, Message) ->
    case is_alive(Instance) of
        true ->
            Instance ! Message,
            ok;
        _ ->
            lager:warning("Instance ~p doesn't exist", [Instance]),
            error
    end.

%%======================================================
%%              Internal lookup functions
%%======================================================
is_alive(Instance) ->
    Pid = case is_pid(Instance) of
        true ->
            Instance;
        _ ->
            whereis(Instance)
    end,

    case Pid of
        undefined ->
            false;
        _ ->
            is_process_alive(Pid)
    end.

get_sequence(Floor, Store) ->
    get_sequence(Floor, Store, []).

get_sequence(Floor, Store, Accumulator) ->
    case orddict:is_key(Floor, Store) of
        true ->
            get_sequence(Floor + 1, Store, Accumulator ++ [Floor]);

        _ ->
            Accumulator
    end.
