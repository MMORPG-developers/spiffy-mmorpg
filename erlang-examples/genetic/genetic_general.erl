-module(genetic_general).

-export([
    % Exported for testing
    % Exported for spawning
    manageBest/1,
    createColony/1,
    createOrganism/3,
    % Exported for use
    main/0
]).

%%%%%%%%%%%%%
% Constants %
%%%%%%%%%%%%%

-define(ACTIONS,
    [
    fight,
    mate,
    mutate
    ]).

-define(NUMBER_START_ACTIONS, 10000).
-define(COLONY_START_SIZE, 1500).
-define(NUMBER_COLONIES, 100).

-define(OTHER_COLONY_PROBABILITY, 0.2).

-define(TIMEOUT, 500).

%%%%%%%%
% Main %
%%%%%%%%

main() ->
    runColonyManager().

%%%%%%%%%%%%%%%%
% Best Manager %
%%%%%%%%%%%%%%%%

manageBest(BestData = {_BestOrganism, BestFitness, _BestActions}) ->
    receive
        % If the colony manager says the program is done, tell it the
        % final result.
        {ManagerPid, manager, {die}} ->
            % Wait for any slow organisms.
            timer:sleep(2 * ?TIMEOUT),
            ManagerPid ! {self(), best, BestData}
    ;
        Data = {_Organism, Fitness, _Actions} ->
            if
                % If we get a better organism, treat it as the best.
                Fitness >  BestFitness ->
                    manageBest(Data)
            ;
                % Otherwise, the old best is still the best.
                Fitness =< BestFitness ->
                    manageBest(BestData)
        end
    end.

%%%%%%%%%%%%%%%%%%
% Colony Manager %
%%%%%%%%%%%%%%%%%%

runColonyManager() ->
    {BestPid, ColonyPids} = setup(),
    manageColonies(BestPid, {ColonyPids, []}).

setup() ->
    % Set up the best manager with a random starting organism.
    RandomOrganism = genetic_specific:randomOrganism(),
    RandomFitness = genetic_specific:fitness(RandomOrganism),
    BestPid = spawn(?MODULE, manageBest, [{RandomOrganism,
                                            RandomFitness, 0}]),
    % Set up the colonies.
    ColonyPids = setupColonies(BestPid),
    {BestPid, ColonyPids}.

setupColonies(BestPid) ->
    % Create colonies and fill them with organisms.
    ColonyPids = createColonies(self(), ?NUMBER_COLONIES),
    ok = fillColonies(BestPid, ColonyPids),
    ColonyPids.

fillColonies(_BestPid, []) ->
    % If there are no colonies to fill, succeed.
    ok;
fillColonies(BestPid, [ColonyPid | ColonyPids]) ->
    % Otherwise, populate the first colony and recurse.
    ok = createOrganisms(BestPid, ColonyPid, ?COLONY_START_SIZE),
    fillColonies(BestPid, ColonyPids).

manageColonies(BestPid, {[], []}) ->
    % If all colonies have finished, ask the best manager for the best
    % organism.
    BestPid ! {self(), manager, {die}},
    receive
        {BestPid, best, Data = {Organism, Fitness, _Actions}} ->
            % Output the best organism and exit.
            io:format("The best organism was~n~w~nwith fitness ~w~n",
                        [Organism, Fitness]),
            Data
    end;
manageColonies(BestPid, ColonyPids = {_AvailablePids, BusyPids}) ->
    receive
        {ColonyPid, colony, Action} ->
            Busy = lists:member(ColonyPid, BusyPids),
            if
                Busy ->
                    managerHandleBusyAction(BestPid, ColonyPids,
                                            ColonyPid, Action)
            ;
                not Busy ->
                    managerHandleAction(BestPid, ColonyPids,
                                            ColonyPid, Action)
            end
    end.

managerHandleBusyAction(BestPid, ColonyPids, ColonyPid, Action) ->
    % Call the appropriate helper function.
    case Action of
        {available} ->
            managerHandleAvailable(BestPid, ColonyPids, ColonyPid)
    ;
        _SomethingElse ->
            % Otherwise, ignore the colony's request.
            ColonyPid ! {self(), manager, {denied}},
            manageColonies(BestPid, ColonyPids)
    end.

managerHandleAction(BestPid, ColonyPids, ColonyPid, Action) ->
    % Call the appropriate helper function.
    case Action of
        {die} ->
            managerHandleDie(BestPid, ColonyPids, ColonyPid)
    ;
        {get_colony_pid} ->
            managerHandleGetPid(BestPid, ColonyPids, ColonyPid)
    end.

managerHandleAvailable(BestPid, {AvailablePids, BusyPids}, ColonyPid) ->
    % Mark the colony as available.
    NewAvailablePids = [ColonyPid | AvailablePids],
    NewBusyPids = lists:delete(ColonyPid, BusyPids),
    NewColonyPids = {NewAvailablePids, NewBusyPids},
    manageColonies(BestPid, NewColonyPids).

managerHandleDie(BestPid, {AvailablePids, BusyPids}, ColonyPid) ->
    % When a colony finishes running, let it know it's died, then remove
    % its PID from the list.
    ColonyPid ! {self(), manager, {die}},
    NewAvailablePids = lists:delete(ColonyPid, AvailablePids),
    NewBusyPids = lists:delete(ColonyPid, BusyPids),
    NewColonyPids = {NewAvailablePids, NewBusyPids},
    manageColonies(BestPid, NewColonyPids).

managerHandleGetPid(BestPid, ColonyPids = {AvailablePids, BusyPids},
                    ColonyPid) ->
    % When a colony wants the pid of another colony, give it one and
    % remove both from the list.
    OtherColonyPid = randomElement(AvailablePids),
    if
        OtherColonyPid ==  ColonyPid ->
            % If the two colonies are the same, deny the colony's request.
            ColonyPid ! {self(), manager, {denied}},
            manageColonies(BestPid, ColonyPids)
    ;
        OtherColonyPid =/= ColonyPid ->
            % Otherwise, accept the request and mark both colonies as
            % busy.
            ColonyPid ! {self(), manager, {get_colony_pid,
                                            OtherColonyPid}},
            NewAvailablePids = AvailablePids --
                                [ColonyPid, OtherColonyPid],
            NewBusyPids = [ColonyPid, OtherColonyPid | BusyPids],
            NewColonyPids = {NewAvailablePids, NewBusyPids},
            manageColonies(BestPid, NewColonyPids)
    end.

%%%%%%%%%%
% Colony %
%%%%%%%%%%

createColonies(_ManagerPid, 0) ->
    % If there are no colonies to build, there are no PIDs to return.
    [];
createColonies(ManagerPid, NumberColonies) when NumberColonies > 0 ->
    % Otherwise, create a colony and add its PID to the list.
    ColonyPid = spawn(?MODULE, createColony, [ManagerPid]),
    [ColonyPid | createColonies(ManagerPid, NumberColonies - 1)].

createColony(ManagerPid) ->
    % Seed the random number generator to avoid determinism.
    random:seed(now()),
    % Invoke the helper function.
    createColonyHelper(ManagerPid, [], ?COLONY_START_SIZE).

createColonyHelper(ManagerPid, OrganismPids, 0) ->
    % If we've got a large enough colony, start managing it.
    manageColony(ManagerPid, {OrganismPids, []});
createColonyHelper(ManagerPid, OrganismPids, OrganismsNeeded) ->
    % Wait for enough organisms to join the colony before letthing them do
    % anything.
    receive
        {OrganismPid, organism, {register}} ->
            colonyHandleRegister(ManagerPid, {[], []}, OrganismPid),
            createColonyHelper(ManagerPid, OrganismPids,
                                OrganismsNeeded - 1)
    end.

manageColony(ManagerPid, ColonyPids = {[], []}) ->
    % If the colony no longer contains organisms, let the manager know.
    ManagerPid ! {self(), colony, {die}},
    receive
        % If the manager accepts, die.
        {ManagerPid, manager, {die}} ->
            ok
    ;
        % If the manager denies the request, keep trying.
        {ManagerPid, manager, {denied}} ->
            manageColony(ManagerPid, ColonyPids)
    ;
        % If another colony asks for something, handle it accordingly.
        {ColonyPid, colony, Action} ->
            colonyHandleColonyAction(ManagerPid, ColonyPids,
                                        ColonyPid, Action)
    end;
manageColony(ManagerPid, OrganismPids = {_AvailablePids, BusyPids}) ->
    receive
        % Handle requests from other colonies.
        {ColonyPid, colony, Action} ->
            colonyHandleColonyAction(ManagerPid, OrganismPids, ColonyPid,
                                        Action)
    ;
        % Handle requests from organisms within the colony.
        {OrganismPid, organism, Action} ->
            Busy = lists:member(OrganismPid, BusyPids),
            if
                % If the organism making the request is already doing
                % something, ignore the request.
                Busy ->
                    colonyHandleBusyAction(ManagerPid, OrganismPids,
                                            OrganismPid, Action)
            ;
                % Otherwise, handle the request.
                not Busy ->
                    colonyHandleAction(ManagerPid, OrganismPids,
                                            OrganismPid, Action)
            end
    after 0 ->
        manageColony(ManagerPid, {[], []})
    end.

colonyHandleColonyAction(ManagerPid, OrganismPids, ColonyPid, Action) ->
    % Call the helper function corresponding to the requested action.
    case Action of
        {get_organism_pid} ->
            colonyHandleGetPid(ManagerPid, OrganismPids, ColonyPid)
    end.

colonyHandleGetPid(ManagerPid, OrganismPids = {[], _BusyPids},
                    ColonyPid) ->
    % If there are no available PIDs, deny the request.
    ColonyPid ! {self(), colony, {denied}},
    % Let the manager know that this colony is available again.
    ManagerPid ! {self(), colony, {available}},
    manageColony(ManagerPid, OrganismPids);
colonyHandleGetPid(ManagerPid, {AvailablePids, BusyPids}, ColonyPid) ->
    % Otherwise, choose a random organism and mark it as busy.
    OrganismPid = randomElement(AvailablePids),
    ColonyPid ! {self(), colony, {get_organism_pid, OrganismPid}},
    NewAvailablePids = lists:delete(OrganismPid, AvailablePids),
    NewBusyPids = [OrganismPid | BusyPids],
    NewOrganismPids = {NewAvailablePids, NewBusyPids},
    % Let the manager know that this colony is available again.
    ManagerPid ! {self(), colony, {available}},
    manageColony(ManagerPid, NewOrganismPids).

colonyHandleAction(ManagerPid, OrganismPids, OrganismPid, Action) ->
    % Call the helper function corresponding to the requested action.
    case Action of
        {register} ->
            colonyHandleRegister(ManagerPid, OrganismPids, OrganismPid)
    ;
        {die} ->
            colonyHandleDie(ManagerPid, OrganismPids, OrganismPid)
    ;
        {fight} ->
            colonyHandleFight(ManagerPid, OrganismPids, OrganismPid)
    ;
        {mate} ->
            colonyHandleMate(ManagerPid, OrganismPids, OrganismPid)
    ;
        {mutate} ->
            colonyHandleMutate(ManagerPid, OrganismPids, OrganismPid)
    end.

colonyHandleBusyAction(ManagerPid, OrganismPids, OrganismPid, Action) ->
    % Call the helper function corresponding to the requested action.
    case Action of
        {fight_die} ->
            colonyHandleFightDie(ManagerPid, OrganismPids, OrganismPid)
    ;
        {fight_live} ->
            colonyHandleFightLive(ManagerPid, OrganismPids, OrganismPid)
    ;
        {mate_live} ->
            colonyHandleMateLive(ManagerPid, OrganismPids, OrganismPid)
    ;
        % Otherwise, ignore the organism's request.
        _OtherAction -> 
            manageColony(ManagerPid, OrganismPids)
    end.

colonyHandleRegister(ManagerPid, {AvailablePids, BusyPids},
                        OrganismPid) ->
    % Tell the organism that it is registered, then manage the updated
    % colony.
    OrganismPid ! {self(), colony, {register}},
    manageColony(ManagerPid, {[OrganismPid | AvailablePids], BusyPids}).

colonyHandleDie(ManagerPid, {AvailablePids, BusyPids}, OrganismPid) ->
    % Tell the organism to die, then remove it from the colony.
    OrganismPid ! {self(), colony, {die}},
    NewAvailablePids = lists:delete(OrganismPid, AvailablePids),
    manageColony(ManagerPid, {NewAvailablePids, BusyPids}).

colonyHandleFight(ManagerPid, OrganismPids = {[], _BusyPids},
                        OrganismPid) ->
    % If the organism has somehow stopped being available, deny its
    % request.
    OrganismPid ! {self(), colony, {denied}},
    manageColony(ManagerPid, OrganismPids);
colonyHandleFight(ManagerPid, OrganismPids = {AvailablePids, BusyPids},
                    OrganismPid) ->
    % Choose an organism to fight with.
    OtherPid = getOrganismPid(ManagerPid, OrganismPids),
    if
        % If the two fighting organisms are really the same, let the
        % organism know it needs to choose a new action.
        OrganismPid ==  OtherPid ->
            OrganismPid ! {self(), colony, {denied}},
            manageColony(ManagerPid, OrganismPids)
    ;
        % Otherwise, set up for the fight.
        OrganismPid =/= OtherPid ->
            % Reserve both PIDs.
            NewAvailablePids = AvailablePids -- [OrganismPid, OtherPid],
            NewBusyPids = [OrganismPid, OtherPid | BusyPids],
            % Let the organism know who it's supposed to fight.
            OrganismPid ! {self(), colony, {fight, OtherPid}},
            NewOrganismPids = {NewAvailablePids, NewBusyPids},
            manageColony(ManagerPid, NewOrganismPids)
    end.

colonyHandleFightDie(ManagerPid, {AvailablePids, BusyPids},
                        OrganismPid) ->
    % The organism no loger exists, so remove it.
    NewBusyPids = lists:delete(OrganismPid, BusyPids),
    manageColony(ManagerPid, {AvailablePids, NewBusyPids}).

colonyHandleFightLive(ManagerPid, {AvailablePids, BusyPids},
                        OrganismPid) ->
    % The organism is no longer busy, so make it available.
    NewAvailablePids = [OrganismPid | AvailablePids],
    NewBusyPids = lists:delete(OrganismPid, BusyPids),
    manageColony(ManagerPid, {NewAvailablePids, NewBusyPids}).

colonyHandleMate(ManagerPid, OrganismPids = {[], _BusyPids},
                        OrganismPid) ->
    % If the organism has somehow stopped being available, deny its
    % request.
    OrganismPid ! {self(), colony, {denied}},
    manageColony(ManagerPid, OrganismPids);
colonyHandleMate(ManagerPid, OrganismPids = {AvailablePids, BusyPids},
                        OrganismPid) ->
    % Choose an organism to mate with.
    OtherPid = getOrganismPid(ManagerPid, OrganismPids),
    if
        % If the two mating organisms are really the same, let the
        % organism know it needs to choose a new action.
        OrganismPid ==  OtherPid ->
            OrganismPid ! {self(), colony, {denied}},
            manageColony(ManagerPid, OrganismPids)
    ;
        % Otherwise, set up for the mating.
        OrganismPid =/= OtherPid ->
            % Reserve both PIDs.
            NewAvailablePids = AvailablePids -- [OrganismPid, OtherPid],
            NewBusyPids = [OrganismPid, OtherPid | BusyPids],
            % Let the organism know who it's supposed to mate with.
            OrganismPid ! {self(), colony, {mate, OtherPid}},
            NewOrganismPids = {NewAvailablePids, NewBusyPids},
            manageColony(ManagerPid, NewOrganismPids)
    end.

colonyHandleMateLive(ManagerPid, {AvailablePids, BusyPids},
                        OrganismPid) ->
    % The organism is no longer busy, so make it available.
    NewAvailablePids = [OrganismPid | AvailablePids],
    NewBusyPids = lists:delete(OrganismPid, BusyPids),
    NewOrganismPids = {NewAvailablePids, NewBusyPids},
    manageColony(ManagerPid, NewOrganismPids).

colonyHandleMutate(ManagerPid, OrganismPids, OrganismPid) ->
    % Tell the organism to mutate.
    OrganismPid ! {self(), colony, {mutate}},
    manageColony(ManagerPid, OrganismPids).

getOrganismPid(ManagerPid, OrganismPids = {AvailablePids, _BusyPids}) ->
    RandomVariable = random:uniform(),
    if
        RandomVariable =< ?OTHER_COLONY_PROBABILITY ->
            % If applicable, choose an organism from another colony.
            getOtherOrganismPid(ManagerPid, OrganismPids)
    ;
        RandomVariable >  ?OTHER_COLONY_PROBABILITY ->
            % Otherwise, choose an organism from this colony.
            randomElement(AvailablePids)
    end.

getOtherOrganismPid(ManagerPid, OrganismPids =
                                    {AvailablePids, _BusyPids}) ->
    % Ask the manager for another colony's PID.
    ManagerPid ! {self(), colony, {get_colony_pid}},
    receive
        {ManagerPid, manager, {denied}} ->
            % If the manager refuses, choose an organism from this colony.
            randomElement(AvailablePids)
    ;
        {ManagerPid, manager, {get_colony_pid, ColonyPid}} ->
            % Otherwise, ask the other colony for an organism.
            getOtherOrganismPidHelper(ManagerPid, OrganismPids, ColonyPid)
    end.

getOtherOrganismPidHelper(ManagerPid, {AvailablePids, _BusyPids},
                            ColonyPid) ->
    % Ask the other colony for an organism.
    ColonyPid ! {self(), colony, {get_organism_pid}},
    receive
        {ColonyPid, colony, {denied}} ->
            % Let the manager know that this colony is available again.
            ManagerPid ! {self(), colony, {available}},
            % If the colony refuses, choose an organism from this colony.
            randomElement(AvailablePids)
    ;
        {ColonyPid, colony, {get_organism_pid, OrganismPid}} ->
            % Let the manager know that this colony is available again.
            ManagerPid ! {self(), colony, {available}},
            % Otherwise, give the requested organism.
            OrganismPid
    end.

%%%%%%%%%%%%
% Organism %
%%%%%%%%%%%%

createOrganisms(_BestPid, _ColonyPid, 0) ->
    ok;
createOrganisms(BestPid, ColonyPid, NumberOrganisms)
                when NumberOrganisms > 0 ->
    RandomOrganism = genetic_specific:randomOrganism(),
    RandomFitness = genetic_specific:fitness(RandomOrganism),
    _OrganismPid = spawn(?MODULE, createOrganism, [BestPid, ColonyPid,
                            {RandomOrganism, RandomFitness,
                                ?NUMBER_START_ACTIONS}]),
    createOrganisms(BestPid, ColonyPid, NumberOrganisms - 1).

createOrganism(BestPid, ColonyPid, Data) ->
    % Set the random seed to avoid determinism.
    random:seed(now()),
    % Let the colony know about the new organism.
    ColonyPid ! {self(), organism, {register}},
    receive
        % Wait for the colony's response, then run.
        {ColonyPid, colony, {register}} ->
            manageOrganism(BestPid, ColonyPid, Data)
    % If the organism gets no response, the colony probably no longer
    % exists, so the organism should give up.
    after ?TIMEOUT ->
            BestPid ! Data
    end.

manageOrganism(BestPid, ColonyPid, Data = {_Organism, _Fitness, 0}) ->
    % If the organism has no actions left, it should attempt to die.
    ColonyPid ! {self(), organism, {die}},
    receive
        % If the colony agrees, the organism dies.
        {ColonyPid, colony, {die}} ->
            handleDie(BestPid, Data)
    ;
        % If the colony has already scheduled the organism for a fight,
        % the organism should fight.
        {OtherPid, organism, {fight, OtherData}} ->
            respondFight(BestPid, ColonyPid, Data, OtherPid, OtherData)
    ;
        % If the colony has already scheduled the organism to mate, the
        % organism should mate.
        {OtherPid, organism, {mate, OtherData}} ->
            respondMate(BestPid, ColonyPid, Data, OtherPid, OtherData)
    end;
manageOrganism(BestPid, ColonyPid, Data) ->
    % Choose something to do and ask the colony for permission to do it.
    ColonyPid ! {self(), organism, {randomAction()}},
    receive
        % If the colony says to perform an action, do so.
        {ColonyPid, colony, Action} ->
            handleColonyAction(BestPid, ColonyPid, Data, Action)
    ;
        % If another organism says to do something, do so.
        {OtherPid, organism, Action} ->
            handleOrganismAction(BestPid, ColonyPid, Data, OtherPid,
                                    Action)
    ;
        SomethingElse ->
            SomethingElse
    end.

handleColonyAction(BestPid, ColonyPid, Data, Action) ->
    case Action of
        {denied} ->
            manageOrganism(BestPid, ColonyPid, Data)
    ;
        {die} ->
            handleDie(BestPid, Data)
    ;
        {fight, OtherPid} ->
            requestFight(BestPid, ColonyPid, Data, OtherPid)
    ;
        {mate, OtherPid} ->
            requestMate(BestPid, ColonyPid, Data, OtherPid)
    ;
        {mutate} ->
            handleMutate(BestPid, ColonyPid, Data)
    end.

handleOrganismAction(BestPid, ColonyPid, Data, OtherPid, Action) ->
    case Action of
        {fight, OtherData} ->
            respondFight(BestPid, ColonyPid, Data, OtherPid, OtherData)
    ;
        {mate, OtherData} ->
            respondMate(BestPid, ColonyPid, Data, OtherPid, OtherData)
    ;
        SomethingElse ->
            SomethingElse
    end.

handleDie(BestPid, Data) ->
    % Let the best manager know about this organism, then die.
    BestPid ! Data.

requestFight(BestPid, ColonyPid, Data = {Organism, Fitness, Actions},
                OtherPid) ->
    OtherPid ! {self(), organism, {fight, Data}},
    receive
        {OtherPid, organism, {fight_die}} ->
            % Let the best manager know about this organism.
            BestPid ! Data,
            % Let the colony know that the organism is no longer busy,
            % then die.
            ColonyPid ! {self(), organism, {fight_die}}
    ;
        {OtherPid, organism, {fight_live, {_OtherOrganism, _OtherFitness,
                                   OtherActions}}} ->
            % Take the actions from the defeated organism.
            NewData = {Organism, Fitness, Actions + OtherActions - 1},
            % Let the colony know that the organism is no longer busy.
            ColonyPid ! {self(), organism, {fight_live}},
            manageOrganism(BestPid, ColonyPid, NewData)
    end.

respondFight(BestPid, ColonyPid, Data = {Organism, Fitness, Actions},
                OtherPid, OtherData = {_OtherOrganism, _OtherFitness,
                OtherActions}) ->
    Live = genetic_specific:fight(Data, OtherData),
    if
        Live ->
            % Let the other organism know that it's dead.
            OtherPid ! {self(), organism, {fight_die}},
            % Take the actions from the defeated organism.
            NewActions = max(Actions + OtherActions - 1, 0),
            NewData = {Organism, Fitness, NewActions},
            % Let the colony know that the organism is no longer busy.
            ColonyPid ! {self(), organism, {fight_live}},
            manageOrganism(BestPid, ColonyPid, NewData)
    ;
        not Live ->
            % Let the best manager know about this organism.
            BestPid ! Data,
            % Let the other organism know that it's still alive.
            OtherPid ! {self(), organism, {fight_live, Data}},
            % Let the colony know that the organism is no longer busy,
            % then die.
            ColonyPid ! {self(), organism, {fight_die}}
    end.

requestMate(BestPid, ColonyPid, Data = {Organism, Fitness, Actions},
            OtherPid) ->
    OtherPid ! {self(), organism, {mate, Data}},
    receive
        {OtherPid, organism, {mate_live, {ChildOrganism, ChildFitness,
                                ChildActions}}} ->
            % Give a random portion of the organism's actions to its
            % child.
            RandomActions = random:uniform(Actions),
            NewActions = max(Actions - RandomActions, 0),
            NewData = {Organism, Fitness, NewActions},
            NewChildActions = ChildActions + RandomActions,
            NewChildData = {ChildOrganism, ChildFitness, NewChildActions},
            % Create the child.
            _ChildPid = spawn(?MODULE, createOrganism,
                                [BestPid, ColonyPid, NewChildData]),
            % Tell the colony that the organism has finished mating.
            ColonyPid ! {self(), organism, {mate_live}},
            manageOrganism(BestPid, ColonyPid, NewData)
    end.

respondMate(BestPid, ColonyPid, {Organism, Fitness, Actions}, OtherPid,
            {OtherOrganism, _OtherFitness, _OtherActions}) ->
    % Mate with the other organism.
    ChildOrganism = genetic_specific:mate(Organism, OtherOrganism),
    ChildFitness = genetic_specific:fitness(ChildOrganism),
    % Give a random portion of the organism's actions to its child.
    ChildActions = random:uniform(Actions + 1) - 1,
    ChildData = {ChildOrganism, ChildFitness, ChildActions},
    NewActions = max(Actions - ChildActions - 1, 0),
    % Tell the other organism to create the child.
    OtherPid ! {self(), organism, {mate_live, ChildData}},
    % Tell the colony that the organism has finished mating.
    ColonyPid ! {self(), organism, {mate_live}},
    manageOrganism(BestPid, ColonyPid, {Organism, Fitness, NewActions}).
    
handleMutate(BestPid, ColonyPid, Data = {Organism, _Fitness, Actions}) ->
    % Let the best manager know what the organism used to be.
    BestPid ! Data,
    % Mutate
    NewOrganism = genetic_specific:mutate(Organism),
    NewFitness = genetic_specific:fitness(NewOrganism),
    NewActions = max(Actions - 1, 0),
    NewData = {NewOrganism, NewFitness, NewActions},
    manageOrganism(BestPid, ColonyPid, NewData).

%%%%%%%%
% Misc %
%%%%%%%%

randomAction() ->
    randomElement(?ACTIONS).

randomElement(List) ->
    Index = random:uniform(length(List)),
    lists:nth(Index, List).
