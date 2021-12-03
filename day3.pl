% day 3

:- use_module(library(clpfd)).

%
% technical predicates
%

file_line(File, Line) :-
    setup_call_cleanup(open(File, read, In),
        stream_line(In, Line),
        close(In)).

stream_line(In, Values) :-
    repeat,
    (   read_line_to_codes(In, Line0),
        Line0 \== end_of_file
    ->  maplist(plus(48), Values, Line0) % so dirty ...
    ;   !,
        fail
    ).

load_data(Data, File) :-
    findall(Word, file_line(File, Word), Data).

b_to_d(0, []).

b_to_d(Result, [Head|Binary]) :-
    b_to_d(Result1, Binary),
    length(Binary, Length),
    Result is Result1 + (Head * (2 ** (Length))). 

%
% problem 1 - business logic
%

gamma_col(Value, Column) :-
    sum_list(Column, Sum),
    length(Column, Length),
    Sum >= Length / 2,
    Value is 1.

gamma_col(Value, Column) :-
    sum_list(Column, Sum),
    length(Column, Length),
    Sum < Length / 2,
    Value is 0.

% end of gamma computing
gamma([], []).

gamma([Value|Gamma], [Column|Transposed]) :-
    gamma(Gamma, Transposed),
    gamma_col(Value, Column).

epsilon([], []).

epsilon([Flipped|Epsilon], [Value|Gamma]) :-
    epsilon(Epsilon, Gamma),
    Value = 1,
    Flipped is 0.

epsilon([Flipped|Epsilon], [Value|Gamma]) :-
    epsilon(Epsilon, Gamma),
    Value = 0,
    Flipped is 1.

resolve_p1(Result, File) :-
    load_data(Data, File),
    transpose(Data, Transposed),
    gamma(Gamma, Transposed),
    epsilon(Epsilon, Gamma),
    b_to_d(DecimalGamma, Gamma),
    b_to_d(DecimalEpsilon, Epsilon),
    Result is DecimalGamma * DecimalEpsilon, !.

%
% problem 2 - business logic
%


