-define(DEFAULT_DEBUG_LEVEL, "info").

update_script_paths() ->
    RootPath = filename:absname(filename:dirname(escript:script_name()) ++ "/.."),
    code:add_pathz(RootPath ++ "/ebin"),

    os:putenv(
        "PATH",

        string:join(
            [
                RootPath ++ "/src",
                RootPath ++ "/ebin",
                os:getenv("PATH")
            ],
            ":"
        )
    ),

    {ok, {}}.

%%==============================================================
%%                Command Line Option Parsing
%%==============================================================
get_options_specification() ->
    [
        {floor, undefined, "floor", {integer, 0}, "Starting point for ordering"},
        {limit, undefined, "limit", {integer, infinity}, "Ordering limit (auto completion)"},
        {logging_level, undefined, "logging", {string, ?DEFAULT_DEBUG_LEVEL}, "Logging level"}
    ].

get_command_line_options(Args) ->
    %% Fetch the command line options & defaults
    OptionsResult = getopt:parse(get_options_specification(), Args),

    case OptionsResult of
        {ok, {Options, Arguments}} ->
            {ok, Options, Arguments};

        {error, {Reason, Data}} ->
            io_lib:format("Error: ~s ~p~n~n", [Reason, Data]),
            showhelp(),
            {error, {Reason, Data}}
    end.

get_logging_level(Options) ->
    list_to_atom(proplists:get_value(logging_level, Options)).

%%==============================================================
%%                         Help
%%==============================================================
showhelp() ->
    getopt:usage(get_options_specification(), atom_to_list(?MODULE)).
