% day 1

%
% technical predicates
%

file_line(File, Line) :-
    setup_call_cleanup(open(File, read, In),
        stream_line(In, Line),
        close(In)).

stream_line(In, Line) :-
    repeat,
    (   read_line_to_string(In, Line0),
        Line0 \== end_of_file
    ->  number_string(Line, Line0)
    ;   !,
        fail
    ).

load_depths(Depths, File) :-
    findall(Word, file_line(File, Word), Depths).

%
% problem 1 - business logic
%

% end of counter
counter(0, [], _).

% element is inferior
counter(Result, [Element|List], Previous) :-
    Element =< Previous,
    counter(Result, List, Element).

% element is superior
counter(Result, [Element|List], Previous) :-
    Element > Previous,
    counter(Result1, List, Element),
    Result is Result1 + 1.

% resolve the first part of day 1
resolve_p1(Result, File) :-
    load_depths(File, [Head|List]),
    counter(Result, List, Head), !.

%
% problem 2 - business logic
%

% end of slide
slide([], [_, _]).

% just sum and slide the list before counting depths increases
slide(Result, [E1, E2, E3|List]) :-
    SumOfDepths is E1 + E2 + E3,
    nth0(0, List1, E3, List),
    nth0(0, List2, E2, List1),
    slide(Result2, List2),
    % do not forget appending to the list is inverted (recursivity is a b*tch)
    append([SumOfDepths], Result2, Result).

% resolve the second part of day 1
resolve_p2(Result, File) :-
    load_depths(Depths, File),
    slide([Head|SlidedList], Depths),
    counter(Result, SlidedList, Head), !.
