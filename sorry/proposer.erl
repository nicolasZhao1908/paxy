-module(proposer).
-export([start/6]).
-define(BACKOFF, 10).
-define(TIMEOUT, 2000).

start(Name, Proposal, Acceptors, Sleep, PanelId, Main) ->
  spawn(fun() -> init(Name, Proposal, Acceptors, Sleep, PanelId, Main) end).

init(Name, Proposal, Acceptors, Sleep, PanelId, Main) ->
  timer:sleep(Sleep),
  Begin = erlang:monotonic_time(),
  Round = order:first(Name),
  {Decision, LastRound} = round(Name, ?BACKOFF, Round, Proposal, Acceptors, PanelId),
  End = erlang:monotonic_time(),
  Elapsed = erlang:convert_time_unit(End-Begin, native, millisecond),
  io:format("[Proposer ~w] DECIDED ~w in round ~w after ~w ms~n", 
             [Name, Decision, LastRound, Elapsed]),
  Main ! done,
  PanelId ! stop.

round(Name, Backoff, Round, Proposal, Acceptors, PanelId) ->
  io:format("[Proposer ~w] Phase 1: round ~w proposal ~w~n", 
             [Name, Round, Proposal]),
  % Update gui
  PanelId ! {updateProp, "Round: " ++ io_lib:format("~p", [Round]), Proposal},
  case ballot(Name, Round, Proposal, Acceptors, PanelId) of
    {ok, Value} ->
      {Value, Round};
    abort ->
      timer:sleep(rand:uniform(Backoff)),
      Next = order:inc(Round),
      round(Name, (2*Backoff), Next, Proposal, Acceptors, PanelId)
  end.

ballot(Name, Round, Proposal, Acceptors, PanelId) ->
  prepare(Round, Acceptors),
  Quorum = (length(Acceptors) div 2) + 1,
  MaxVoted = order:null(),
  case collect(Quorum, Round, MaxVoted, Proposal, Quorum) of
    {accepted, Value} ->
      io:format("[Proposer ~w] Phase 2: round ~w proposal ~w (was ~w)~n", 
                 [Name, Round, Value, Proposal]),
      % update gui
      PanelId ! {updateProp, "Round: " ++ io_lib:format("~p", [Round]), Value},
      accept(Round, Value, Acceptors),
      case vote(Quorum, Round, Quorum) of
        ok ->
          {ok, Value};
        abort ->
          abort
      end;
    abort ->
      abort
  end.


collect(0, _, _, Proposal,_) ->
  {accepted, Proposal};

collect(_, _, _, _, 0) ->
  abort;

collect(N, Round, MaxVoted, Proposal, NumSorries) ->
  receive 
    {promise, Round, _, na} ->
      collect(N-1, Round, MaxVoted, Proposal, NumSorries);
    {promise, Round, Voted, Value} ->
      case order:gr(Voted, MaxVoted) of
        true ->
          collect(N-1, Round, Voted, Value, NumSorries);
        false ->
          collect(N-1, Round, MaxVoted, Proposal, NumSorries)
      end;
    {promise, _, _,  _} ->
      collect(N, Round, MaxVoted, Proposal, NumSorries);
    {sorry, {prepare, Round}} ->
      collect(N, Round, MaxVoted, Proposal, NumSorries-1);
    {sorry, _} ->
      collect(N, Round, MaxVoted, Proposal, NumSorries-1)
  after ?TIMEOUT ->
    abort
  end.

vote(_, _, 0) ->
  abort;
vote(0, _, _) ->
  ok;
vote(N, Round, NumSorries) ->
  receive
    {vote, Round} ->
      vote(N-1, Round, NumSorries);
    {vote, _} ->
      vote(N, Round, NumSorries);
    {sorry, {accept, Round}} ->
      vote(N, Round, NumSorries-1);
    {sorry, _} ->
      vote(N, Round, NumSorries-1)
  after ?TIMEOUT ->
    abort
  end.

prepare(Round, Acceptors) ->
  Fun = fun(Acceptor) -> 
    send(Acceptor, {prepare, self(), Round}) 
  end,
  lists:foreach(Fun, Acceptors).

accept(Round, Proposal, Acceptors) ->
  Fun = fun(Acceptor) -> 
    send(Acceptor, {accept, self(), Round, Proposal}) 
  end,
  lists:foreach(Fun, Acceptors).

send(Name, Message) ->
  Name ! Message.

