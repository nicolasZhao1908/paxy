-module(acceptor).
-export([start/2]).
-define(delay_promise, 8000).
-define(delay_vote, 8000).
-define(drop, 1).

start(Name, PanelId) ->
    spawn(fun() -> init(Name, PanelId) end).

init(Name, PanelId) ->
    Promised = order:null(),
    Voted = order:null(),
    Value = na,
    acceptor(Name, Promised, Voted, Value, PanelId).

getdelay() ->
   list_to_integer(os:getenv("delay_shell")).

send_or_drop(Proposer, message) ->
  P = rand:uniform(10),
  if P =< ?drop ->
        io:format("message dropped~n");
     true -> Proposer ! message, true
  end.

acceptor(Name, Promised, Voted, Value, PanelId) ->
  receive
    {prepare, Proposer, Round} ->
      case order:gr(Round, Promised) of
        true ->
            T = rand:uniform(getdelay()),
            timer:send_after(T, Proposer, {promise, Round, Voted, Value}),
          % Proposer ! {promise, Round, Voted, Value},               
      io:format("[Acceptor ~w] Phase 1: promised ~w voted ~w colour ~w delayed ~w~n",
                 [Name, Round, Voted, Value, T]),
          % Update gui
          Colour = case Value of na -> {0,0,0}; _ -> Value end,
          PanelId ! {updateAcc, "Voted: " ++ io_lib:format("~p", [Voted]), 
                     "Promised: " ++ io_lib:format("~p", [Round]), Colour},
          acceptor(Name, Round, Voted, Value, PanelId);
        false ->
          Proposer ! {sorry, {prepare, Round}},
          acceptor(Name, Promised, Voted, Value, PanelId)
      end;
    {accept, Proposer, Round, Proposal} ->
      case order:goe(Round, Promised) of
        true ->
          T = rand:uniform(getdelay()),
          timer:send_after(T, Proposer, {vote, Round}),
          % Proposer ! {vote, Round},
          case order:goe(Round, Voted) of
            true ->
      io:format("[Acceptor ~w] Phase 2: promised ~w voted ~w colour ~w delayed ~w~n",
                 [Name, Promised, Round, Proposal, T]),
              % Update gui
              PanelId ! {updateAcc, "Voted: " ++ io_lib:format("~p", [Round]), 
                         "Promised: " ++ io_lib:format("~p", [Promised]), Proposal},
              acceptor(Name, Promised, Round, Proposal, PanelId);
            false ->
              acceptor(Name, Promised, Voted, Value, PanelId)
          end;                            
        false ->
          Proposer ! {sorry, {accept, Round}},
          acceptor(Name, Promised, Voted, Value, PanelId)
      end;
    stop ->
      PanelId ! stop,
      ok
  end.
