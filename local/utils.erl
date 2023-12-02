-module(utils).
-export([get_timeout/0, get_send_strategy/0, get_acceptors/0, get_proposers/0, get_random_color/0]).
-define(TIMEOUT, 2000).
-define(DELAY, 2000).
-define(RED, {255, 0, 0}).
-define(BLUE, {0, 0, 255}).
-define(GREEN, {0, 255, 0}).
-define(NUM_PROP, 3).
-define(NUM_ACC, 5).

% {Integer, source}
get_timeout() ->
    T = os:getenv("timeout"),
    case T of
        false -> {?TIMEOUT, program};
        _ -> {list_to_integer(T), shell}
    end.

% {Integer, source}
get_delay() ->
    D = os:getenv("delay"),
    case D of
        false -> {?DELAY, program};
        _ -> {list_to_integer(D), shell}
    end.

% {Bool, source}
get_drop() ->
    P = rand:uniform(100),
    D = os:getenv("drop"),
    case D of
        false -> {false, program};
        _ -> {P =< list_to_integer(D), shell}
    end.

% Bool
is_normal_send() ->
    case {os:getenv("acceptor"), os:getenv("proposer")} of
        {false, false} -> false;
        _ -> true
    end.

% normal | {delayed, Delay} | dropped | normal | Error
get_send_strategy() ->
    case {is_normal_send(), get_delay(), get_drop(), get_timeout()} of
        % is acceptor vs proposer measurement
        {true, _, _,_} ->
            normal;
        % is delay measurement
        {_, {Delay, shell}, _, _} ->
            {delayed, Delay};
        % is timeout measurement
        {_, {Delay, program}, _, {_, shell}} ->
            {delayed, Delay};
        % is drop measurement did dropped
        {_, {_, program}, {true, shell}, _} ->
            dropped;
        % is drop measurement didn't dropped
        {_, {_, program}, {false, shell},_} ->
            normal;
        % is normal send
        {_, {_, program}, {_, program},_} ->
            normal;
        _ ->
            error(combination_not_found)
    end.

get_unique_id() ->
    Timestamp = os:timestamp(),
    lists:flatten(io_lib:format("~w", [Timestamp])).

get_random_color() ->
    case rand:uniform(3) of
        1 -> ?RED;
        2 -> ?BLUE;
        _ -> ?GREEN
    end.

get_proposers() ->
    Val = os:getenv("proposer"),
    N =
        case Val of
            false -> ?NUM_PROP;
            _ -> Val
        end,
    [get_unique_id() || _ <- lists:seq(1, list_to_integer(N))].

get_acceptors() ->
    Val = os:getenv("acceptor"),
    N =
        case Val of
            false -> ?NUM_ACC;
            _ -> Val
        end,
    [get_unique_id() || _ <- lists:seq(1, list_to_integer(N))].
