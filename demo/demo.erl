%% Copyright 2013 Damion K. Wilson
%% 
#! /usr/bin/env escript

%% Compile this script before running it
-mode(compile).

%% Includes
-include_lib("$PROJECT_ROOT/include/escript_utils.hrl").

%% Lager compilation settings
-compile([{parse_transform, lager_transform}]).

% main() ->
%     main(init:get_plain_arguments()).
 
main([]) ->
    command_line_options:showhelp(get_options_specification()),
    halt(0);

main({ok, Options, []}) ->
    main(Options, []);

main({ok, Options, Arguments}) ->
    main(Options, Arguments);

main({error, Details}) ->
    lager:error("Error in command line ~p", [Details]);

main(Arguments) ->
    update_script_paths(),
    main(get_command_line_options(Arguments)).

main(Options, _Arguments) ->
    lager:start(),
    lager:set_loglevel(lager_console_backend, get_logging_level(Options)),

    Floor = proplists:get_value(floor, Options),

    Limit = case proplists:get_value(limit, Options) of
        infinity ->
            12;

        _Value ->
            _Value
    end,

    WantIncomplete = 0,

    gen_collator_server:start_registered(Floor, Limit + WantIncomplete),

    gen_collator_server:push(12, 12),

    % gen_collator_server:raise(my_demo_exception),

    [gen_collator_server:push(Value, Value) || Value <- get_test_range(Floor, Limit)],

    Results = consumer(),

    lager:info("RESULTS = ~p", [Results]).

consumer() ->
    CollationHandler = fun(LoopingReference, Accumulator) ->
        receive
            {data, Message} ->
                lager:info("Received ~p", [Message]),
                LoopingReference(LoopingReference, Accumulator ++ [Message]);

            {finished} ->
                lager:info("Finished"),
                Accumulator;

            {incomplete} ->
                incomplete;

            {Error} ->
                lager:info("ERROR ~p", [Error]),
                Error
        after
            1000 ->
                gen_collator_server:close(),
                LoopingReference(LoopingReference, Accumulator)
        end
    end,

    CollationHandler(CollationHandler, []).

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

    lager:info("Providing series ~p", [Shuffled]),

    Shuffled.
