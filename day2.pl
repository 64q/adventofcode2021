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
directions(Forward, Height, [["forward", Units]|Directions]) :-
    directions(Forward1, Height, Directions),
    Forward is Forward1 + Units.

% handle up direction
directions(Forward, Height, [["up", Units]|Directions]) :-
    directions(Forward, Height1, Directions),
    Height is Height1 + Units.

% handle down direction
directions(Forward, Height, [["down", Units]|Directions]) :-
    directions(Forward, Height1, Directions),
    Height is Height1 - Units.

resolve_p1(Result, File) :-
    load_directions(Directions, File),
    directions(Forward, Height, Directions),
    Result is abs(Forward * Height), !.

%
% problem 2 - business logic
%

% end of directions
directions(0, 0, 0, []).

% handle forward direction
directions(Aim, Forward, Height, [["forward", Units]|Directions]) :-
    directions(Aim, Forward1, Height1, Directions),
    Forward is Forward1 + Units,
    Height is Height1 + (Units * Aim).

% handle up direction
directions(Aim, Forward, Height, [["up", Units]|Directions]) :-
    directions(Aim1, Forward, Height, Directions),
    Aim is Aim1 + Units.

% handle down direction
directions(Aim, Forward, Height, [["down", Units]|Directions]) :-
    directions(Aim1, Forward, Height, Directions),
    Aim is Aim1 - Units.

resolve_p2(Result, File) :-
    load_directions(Directions, File),
    % we need to reverse the list because the way recursivity work made us do the operations backwards
    reverse(InvertedDirections, Directions, []),
    directions(_, Forward, Height, InvertedDirections),
    Result is abs(Forward * Height), !.
