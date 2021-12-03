% day 2

%
% technical predicates
%

file_line(File, Line) :-
    setup_call_cleanup(open(File, read, In),
        stream_line(In, Line),
        close(In)).

stream_line(In, [Direction, Units]) :-
    repeat,
    (   read_line_to_string(In, Line0),
        Line0 \== end_of_file
    ->  split_string(Line0, " ", "", [Direction, Units0]),
        number_string(Units, Units0)
    ;   !,
        fail
    ).

load_directions(Directions, File) :-
    findall(Word, file_line(File, Word), Directions).

reverse(Result, [], Result).
reverse(Result, [Head|Tail], Acc) :- reverse(Result, Tail, [Head|Acc]).

%
% problem 1 - business logic
%

% end of directions
directions(0, 0, []).

% handle forward direction
directions(Forward, Depth, [["forward", Units]|Directions]) :-
    directions(Forward1, Depth, Directions),
    Forward is Forward1 + Units.

% handle up direction
directions(Forward, Depth, [["up", Units]|Directions]) :-
    directions(Forward, Depth1, Directions),
    Depth is Depth1 + Units.

% handle down direction
directions(Forward, Depth, [["down", Units]|Directions]) :-
    directions(Forward, Depth1, Directions),
    Depth is Depth1 - Units.

resolve_p1(Result, File) :-
    load_directions(Directions, File),
    directions(Forward, Depth, Directions),
    Result is abs(Forward * Depth), !.

%
% problem 2 - business logic
%

% end of directions
directions(0, 0, 0, []).

% handle forward direction
directions(Aim, Forward, Depth, [["forward", Units]|Directions]) :-
    directions(Aim, Forward1, Depth1, Directions),
    Forward is Forward1 + Units,
    Depth is Depth1 + (Units * Aim).

% handle up direction
directions(Aim, Forward, Depth, [["up", Units]|Directions]) :-
    directions(Aim1, Forward, Depth, Directions),
    Aim is Aim1 + Units.

% handle down direction
directions(Aim, Forward, Depth, [["down", Units]|Directions]) :-
    directions(Aim1, Forward, Depth, Directions),
    Aim is Aim1 - Units.

resolve_p2(Result, File) :-
    load_directions(Directions, File),
    % we need to reverse the list because the way recursivity work made us do the operations backwards
    reverse(ReversedDirections, Directions, []),
    directions(_, Forward, Depth, ReversedDirections),
    Result is abs(Forward * Depth), !.
