-module(acceptor).
-export([start/2]).
-define(delay_promise, 2000).
-define(delay_accept, 2000).
-define(drop,1).


start(Name, PanelId) ->
  spawn(fun() -> init(Name, PanelId) end).
        
init(Name, PanelId) ->
  Promised = order:null(), 
  Voted = order:null(),
  Value = na,
  acceptor(Name, Promised, Voted, Value, PanelId).

getdelay() ->
  D = os:getenv("delay"),
  case D of
      false -> ?delay_promise;
      _ -> list_to_integer(D)
  end.
  

getdrop() ->
  P = rand:uniform(10),
  P =< list_to_integer(os:getenv("drop")).

send_or_drop(Proposer, Message) ->
  case getdrop() of
    true -> io:format("dropping message~n",[]);
    false -> Proposer ! Message
  end.


acceptor(Name, Promised, Voted, Value, PanelId) ->
  receive
    {prepare, Proposer, Round} ->
      case order:gr(Round, Promised) of
        true ->
          %Original -----------------------------
          Proposer ! {promise, Round, Voted, Value}, %- original

          %Experiment i.1)-----------------------------
          %T = rand:uniform(get_delay()),
          %timer:send_after(T,Proposer,{promise, Round, Voted, Value}),

          %Experiment iii)-----------------------------
          %send_or_drop(Proposer,{promise, Round, Voted, Value}),

      io:format("[Acceptor ~w] Phase 1: promised ~w voted ~w colour ~w~n",
        [Name, Round, Voted, Value]),
          % Update gui
          Colour = case Value of na -> {0,0,0}; _ -> Value end,
          PanelId ! {updateAcc, "Voted: " ++ io_lib:format("~p", [Voted]), 
                     "Promised: " ++ io_lib:format("~p", [Round]), Colour}, % Round
          acceptor(Name, Round, Voted, Value, PanelId); % Round
        false ->
          Proposer ! {sorry, {prepare, Round}},
          acceptor(Name, Promised, Voted, Value, PanelId)
      end;
    {accept, Proposer, Round, Proposal} ->
      case order:goe(Round, Promised) of %Promised
        true ->

          %Original -----------------------------
          Proposer ! {vote, Round}, % Proposer ! {vote, Round}

          %Experiment i.1) -----------------------------
          %T = rand:uniform(get_delay()),
          %timer:send_after(T,Proposer,{vote, Round}),

          % Experiment iii) -----------------------------
          %send_or_drop(Proposer,{vote, Round}),
          
          case order:goe(Round, Voted) of
            true ->
      io:format("[Acceptor ~w] Phase 2: promised ~w voted ~w colour ~w~n",
                 [Name, Promised, Round, Proposal]), % Name, Promised, Round, Proposal
              % Update gui
              PanelId ! {updateAcc, "Voted: " ++ io_lib:format("~p", [Round]), % Round
                         "Promised: " ++ io_lib:format("~p", [Promised]), Proposal}, %Proposal
              acceptor(Name, Promised, Round, Proposal, PanelId);
            false ->
              acceptor(Name, Promised, Voted, Value, PanelId) %Round vs Voted
          end;
        false ->
          Proposer ! {sorry, {accept, Round}}, %maybe optimization: Promised here
          acceptor(Name, Promised, Voted, Value, PanelId)
      end;
    stop ->
      PanelId ! stop,
      ok
  end.
