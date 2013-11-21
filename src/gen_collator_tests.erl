%% Module declaration
-module(gen_collator_tests).

-ifdef(TEST).

-compile(export_all).

%% Include eunit definitions
-include_lib("eunit/include/eunit.hrl").

%%======================================================
%%                     Tests
%%======================================================
basic_test_() ->
    [
        {"Basic Registered Test", {timeout, 1200, [fun execute_basic_registered/0]}},
        {"Basic Test", {timeout, 1200, [fun execute_basic/0]}},
        {"Explicit Close", {timeout, 1200, [fun execute_explicit_close/0]}},
        {"Explicit Raise", {timeout, 1200, [fun execute_raise/0]}},
        {"Test Incomplete", {timeout, 1200, [fun execute_incomplete/0]}},
        {"Test File collation", {timeout, 1200, [fun execute_file_collation/0]}}
    ].

execute_basic_registered() ->
    Floor = 4,
    Limit = 27,

    gen_collator_server:start_registered(Floor, Limit),

    [gen_collator_server:push(Value, Value) || Value <- get_test_range(Floor, Limit)],

    HandlerFunction = fun(LoopingReference, Collection) ->
        receive
            {data, Message} ->
                LoopingReference(LoopingReference, Collection ++ [Message]);
            {finished} ->
                Collection
        end
    end,

    Expected = lists:seq(Floor, Limit),
    Results = HandlerFunction(HandlerFunction, []),
    ?assertEqual(Expected, Results).

execute_basic() ->
    Floor = 22,
    Limit = 32,

    {ok, Instance} = gen_collator_server:start_link(Floor, Limit),

    [gen_collator_server:push(Instance, Value, Value) || Value <- get_test_range(Floor, Limit)],

    HandlerFunction = fun(LoopingReference, Collection) ->
        receive
            {data, Message} ->
                LoopingReference(LoopingReference, Collection ++ [Message]);
            {finished} ->
                Collection
        end
    end,

    Expected = lists:seq(Floor, Limit),
    Results = HandlerFunction(HandlerFunction, []),
    ?assertEqual(Expected, Results).

execute_explicit_close() ->
    Floor = 2,
    Limit = 10,

    {ok, Instance} = gen_collator_server:start_link(Floor, infinity),

    [gen_collator_server:push(Instance, Value, Value) || Value <- get_test_range(Floor, Limit)],

    HandlerFunction = fun(LoopingReference, Collection) ->
        receive
            {data, Message} ->
                NewCollection = Collection ++ [Message],

                case Message of
                    Limit ->
                        gen_collator_server:close(Instance);
                    _ ->
                        true
                end,

                LoopingReference(LoopingReference, NewCollection);

            {finished} ->
                Collection
        end
    end,

    Expected = lists:seq(Floor, Limit),
    Results = HandlerFunction(HandlerFunction, []),
    ?assertEqual(Expected, Results).

execute_incomplete() ->
    Floor = 2,
    Limit = 10,

    {ok, Instance} = gen_collator_server:start_link(Floor, Limit + 2),

    [gen_collator_server:push(Instance, Value, Value) || Value <- get_test_range(Floor, Limit)],

    HandlerFunction = fun(LoopingReference, Collection) ->
        receive
            {data, Message} ->
                NewCollection = Collection ++ [Message],
                LoopingReference(LoopingReference, NewCollection);

            {finished} ->
                Collection;

            {incomplete} ->
                incomplete
        after
            1000 ->
                gen_collator_server:close(Instance),
                LoopingReference(LoopingReference, Collection)
        end
    end,

    Expected = incomplete,
    Results = HandlerFunction(HandlerFunction, []),
    ?assertEqual(Expected, Results).

execute_file_collation() ->
    Filename = "file_collation.log",
    Floor = 0,
    Limit = 9,

    {ok, Instance} = file_collator:start(Filename, Floor, Limit),

    [file_collator:push(Instance, Value, Value) || Value <- get_test_range(Floor, Limit)],
    file_collator:close(Instance),

    Expected = lists:seq(Floor, Limit),

    Results = case file:open(Filename, [read]) of
        {ok, Filehandle} ->
            GetFileContents = fun(LoopingReference, Accumulator) ->
                case file:read_line(Filehandle) of
                    {ok, []} ->
                        LoopingReference(LoopingReference, Accumulator);

                    {ok, Data} ->
                        Fixed = case lists:last(Data) of
                            10 ->
                                lists:sublist(Data, length(Data) - 1);
                            _ ->
                                Data
                        end,

                        LoopingReference(LoopingReference, Accumulator ++ [list_to_integer(Fixed)]);
                    _ ->
                        Accumulator
                end
            end,

            Value = GetFileContents(GetFileContents, []),
            file:close(Filehandle),
            Value;

        _ ->
            undefined
    end,

    ?assertEqual(Expected, Results).

execute_raise() ->
    ExceptionDetail = myexception,
    ExceptionValue = 5,
    Floor = 0,
    Limit = 9,

    {ok, Instance} = gen_collator_server:start_link(Floor, Limit),

    HandlerFunction = fun(LoopingReference, Collection) ->
        receive
            {data, Message} ->
                LoopingReference(LoopingReference, Collection ++ [Message]);

            {finished} ->
                Collection;

            {Error} ->
                throw(Error),
                Collection
        after
            100 ->
                Collection
        end
    end,

    ?assertThrow(
        ExceptionDetail,

        lists:foldl(
            fun(NextValue, Accumulator) ->
                % Push data (or error) to collation stream
                case NextValue of
                    ExceptionValue ->
                        gen_collator_server:raise(Instance, ExceptionDetail);

                    _ ->
                        gen_collator_server:push(Instance, NextValue, NextValue)
                end,

                % Handle any replies from the collator
                HandlerFunction(HandlerFunction, Accumulator)
            end,

            [],
            get_test_range(Floor, Limit)
        )
    ).

get_test_range(Limit) ->
    get_test_range(0, Limit).

get_test_range(Floor, Limit) ->
    {A1,A2,A3} = now(),

    random:seed(A1, A2, A3),

    Shuffled = lists:foldl(
        fun(Value, Accumulator) ->
            Index = random:uniform(length(Accumulator) + 1) - 1,
            {Left, Right} = lists:split(Index, Accumulator),
            Left ++ [Value] ++ Right
        end,

        [],
        lists:seq(Floor, Limit)
    ),

    % lager:info("Providing series ~p", [Shuffled]),

    Shuffled.

-endif.
