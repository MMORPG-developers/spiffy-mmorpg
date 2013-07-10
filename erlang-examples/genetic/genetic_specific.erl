-module(genetic_specific).

-export([
    % Exported for testing
    % Exported for use
    fight/2,
    mate/2,
    mutate/1,
    fitness/1,
    randomOrganism/0
]).

-define(MUTATION_PROBABILITY, 0.05).

randomOrganism() ->
    randomDigits(16).

fitness(List) ->
    pairScore(List, [5, 6, 1, 6, 1, 8, 5, 6, 5, 0, 5, 1, 8, 2, 9, 3], 2) +
    pairScore(List, [3, 8, 4, 7, 4, 3, 9, 6, 4, 7, 2, 9, 3, 0, 4, 7], 1) +
    pairScore(List, [5, 8, 5, 5, 4, 6, 2, 9, 4, 0, 8, 1, 0, 5, 8, 7], 3) +
    pairScore(List, [9, 7, 4, 2, 8, 5, 5, 5, 0, 7, 0, 6, 8, 3, 5, 3], 3) +
    pairScore(List, [4, 2, 9, 6, 8, 4, 9, 6, 4, 3, 6, 0, 7, 5, 4, 3], 3) +
    pairScore(List, [3, 1, 7, 4, 2, 4, 8, 4, 3, 9, 4, 6, 5, 8, 5, 8], 1) +
    pairScore(List, [4, 5, 1, 3, 5, 5, 9, 0, 9, 4, 1, 4, 6, 1, 1, 7], 2) +
    pairScore(List, [7, 8, 9, 0, 9, 7, 1, 5, 4, 8, 9, 0, 8, 0, 6, 7], 3) +
    pairScore(List, [8, 1, 5, 7, 3, 5, 6, 3, 4, 4, 1, 1, 8, 4, 8, 3], 1) +
    pairScore(List, [2, 6, 1, 5, 2, 5, 0, 7, 4, 4, 3, 8, 6, 8, 9, 9], 2) +
    pairScore(List, [8, 6, 9, 0, 0, 9, 5, 8, 5, 1, 5, 2, 6, 2, 5, 4], 3) +
    pairScore(List, [6, 3, 7, 5, 7, 1, 1, 9, 1, 5, 0, 7, 7, 0, 5, 0], 1) +
    pairScore(List, [6, 9, 1, 3, 8, 5, 9, 1, 7, 3, 1, 2, 1, 3, 6, 0], 1) +
    pairScore(List, [6, 4, 4, 2, 8, 8, 9, 0, 5, 5, 0, 4, 2, 7, 6, 8], 2) +
    pairScore(List, [2, 3, 2, 1, 3, 8, 6, 1, 0, 4, 3, 0, 3, 8, 4, 5], 0) +
    pairScore(List, [2, 3, 2, 6, 5, 0, 9, 4, 7, 1, 2, 7, 1, 4, 4, 8], 2) +
    pairScore(List, [5, 2, 5, 1, 5, 8, 3, 3, 7, 9, 6, 4, 4, 3, 2, 2], 2) +
    pairScore(List, [1, 7, 4, 8, 2, 7, 0, 4, 7, 6, 7, 5, 8, 2, 7, 6], 3) +
    pairScore(List, [4, 8, 9, 5, 7, 2, 2, 6, 5, 2, 1, 9, 0, 3, 0, 6], 1) +
    pairScore(List, [3, 0, 4, 1, 6, 3, 1, 1, 1, 7, 2, 2, 4, 6, 3, 5], 3) +
    pairScore(List, [1, 8, 4, 1, 2, 3, 6, 4, 5, 4, 3, 2, 4, 5, 8, 9], 3) +
    pairScore(List, [2, 6, 5, 9, 8, 6, 2, 6, 3, 7, 3, 1, 6, 8, 6, 7], 2).

mutate([]) ->
    [];
mutate([Digit | Digits]) ->
    RandomVariable = random:uniform(),
    if
        RandomVariable <  ?MUTATION_PROBABILITY ->
            [randomDigit() | mutate(Digits)]
    ;
        RandomVariable >= ?MUTATION_PROBABILITY ->
            [Digit | mutate(Digits)]
    end.

mate([], []) ->
    [];
mate([First | Firsts], [Second | Seconds]) ->
    RandomVariable = random:uniform(2),
    if
        RandomVariable == 1 ->
            [First | mate(Firsts, Seconds)]
    ;
        RandomVariable == 2 ->
            [Second | mate(Firsts, Seconds)]
    end.

fight({_Organism, Fitness, _Actions},
        {_OtherOrganism, OtherFitness, _OtherActions}) ->
    Prob = 353 - OtherFitness,
    OtherProb = 353 - Fitness,
    RandomVariable = random:uniform(Prob + OtherProb),
    RandomVariable =< Prob.

% Helper functions

randomDigit() ->
    random:uniform(10) - 1.

randomDigits(0) ->
    [];
randomDigits(NumberDigits) when NumberDigits > 0 ->
    [randomDigit() | randomDigits(NumberDigits - 1)].

distance(FirstList, []) ->
    length(FirstList);
distance([], SecondList) ->
    length(SecondList);
distance([Item | FirstList], [Item | SecondList]) ->
    distance(FirstList, SecondList);
distance([_FirstItem | FirstList], [_SecondItem | SecondList]) ->
    1 + distance(FirstList, SecondList).

pairScore(FirstList, SecondList, Expected) ->
    Len = length(FirstList),
    Agreement = Len - distance(FirstList, SecondList),
    Len - abs(Expected - Agreement).

