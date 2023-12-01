-module(paxy).
-export([start/1, measure/0, stop/0, stop/1]).

-define(RED, {255,0,0}).
-define(BLUE, {0,0,255}).
-define(GREEN, {0,255,0}).

-define(NUM_PROP, 3).
-define(NUM_ACC, 5).

generate_unique_id() ->
    Timestamp = os:timestamp(),
    lists:flatten(io_lib:format("~w", [Timestamp])).

random_color()->
  case rand:uniform(4) of
    1 -> ?RED;
    2 -> ?BLUE;
    _ -> ?GREEN 
  end
.

get_proposers() ->
    Val = os:getenv("proposer"),
    case Val of
        false -> ?NUM_PROP;
        _ -> [generate_unique_id() || _ <- lists:seq(1, list_to_integer(Val))]
    end.

get_acceptors() ->
    Val = os:getenv("acceptor"),
    case Val of
        false -> ?NUM_ACC;
        _ -> [generate_unique_id() || _ <- lists:seq(1, list_to_integer(Val))]
    end.

measure() ->
  AcceptorNames = get_acceptors(),
  AccRegister = [ list_to_atom(X) || X <- AcceptorNames],
  ProposerNames = [{X,random_color()} || X <- get_proposers()],
  Sleep = [100 || _ <- ProposerNames],
  PropInfo = [ {list_to_atom(X), Color} || {X,Color} <- ProposerNames],
  register(gui, spawn(fun() -> gui:start(AcceptorNames, ProposerNames) end)),
  gui ! {reqState, self()},
  receive
    {reqState, State} ->
      {AccIds, PropIds} = State,
      start_acceptors(AccIds, AccRegister),
      spawn(fun() -> 
        Begin = erlang:monotonic_time(),
        start_proposers(PropIds, PropInfo, AccRegister, Sleep, self()),
        wait_proposers(length(PropIds)),
        End = erlang:monotonic_time(),
        Elapsed = erlang:convert_time_unit(End-Begin, native, millisecond),
        io:format("[Paxy] Total elapsed time: ~w ms~n", [Elapsed])
      end)
  end.

start(Sleep) ->
  AcceptorNames = ["Homer", "Marge", "Bart", "Lisa", "Maggie"],
  %AccRegister = [homer, marge, bart, lisa, maggie],
  ProposerNames = [{"Fry", ?RED}, {"Bender", ?GREEN}, {"Leela", ?BLUE}],
  AccRegister = [{X, 'paxy-acc'} ||X <- [homer, marge, bart, lisa, maggie]],
  %ProposerRegister =  [{{Name, "paxy-pro"},Color} || {Name, Color} <- ProposerNames],
  PropInfo = [{fry, ?RED}, {bender, ?GREEN}, {leela, ?BLUE}],
  register(gui, spawn(fun() -> gui:start(AcceptorNames, ProposerNames) end)),
  gui ! {reqState, self()},
  receive
    {reqState, State} ->
      {AccIds, PropIds} = State,
      spawn('paxy-acc', fun() -> 
        start_acceptors(AccIds, AccRegister) 
      end),
      spawn('paxy-pro', fun() -> 
        Begin = erlang:monotonic_time(),
        start_proposers(PropIds, PropInfo, AccRegister, Sleep, self()),
        wait_proposers(length(PropIds)),
        End = erlang:monotonic_time(),
        Elapsed = erlang:convert_time_unit(End-Begin, native, millisecond),
        io:format("[Paxy] Total elapsed time: ~w ms~n", [Elapsed])
      end)
  end.
    
start_acceptors(AccIds, AccReg) ->
  case AccIds of
    [] ->
      ok;
    [AccId|Rest] ->
      [{Name, _}|RegNameRest] = AccReg,
      register(Name, acceptor:start(Name, AccId)),
      start_acceptors(Rest, RegNameRest)
  end.

start_proposers(PropIds, PropInfo, Acceptors, Sleep, Main) ->
  case PropIds of
    [] ->
      ok;
    [PropId|Rest] ->
      [{RegName, Colour} |RestInfo] = PropInfo,
      [FirstSleep|RestSleep] = Sleep,
      proposer:start(RegName, Colour, Acceptors, FirstSleep, PropId, Main),	
      %register({RegName,RegNode} ,proposer:start(RegName, Colour, Acceptors, FirstSleep, PropId, Main)),	
      start_proposers(Rest, RestInfo, Acceptors, RestSleep, Main)
  end.

wait_proposers(0) ->
  ok;
wait_proposers(N) ->
  receive
    done ->
      wait_proposers(N-1)
  end.

stop() ->
  stop(homer),
  stop(marge),
  stop(bart),
  stop(lisa),
  stop(maggie),
  stop(gui).

stop(Name) ->
  case whereis(Name) of
    undefined ->
      ok;
    Pid ->
      Pid ! stop
  end.
