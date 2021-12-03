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

b_to_d(Result, [Head|Tail]) :-
    b_to_d(Result1, Tail),
    length(Tail, Length),
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

filter([], [], _, _).

filter([Line|Result], [Line|Data], Value, Index) :-
    nth0(Index, Line, Value),
    filter(Result, Data, Value, Index), !. % on evalue pas les autres prédicats.

filter(Result, [_|Data], Value, Index) :-
    filter(Result, Data, Value, Index).

filter_o2(Data, [Data], _, _).

filter_o2(Result, Data, Gamma, Position) :-
    nth0(Position, Gamma, Gamma1),
    filter(Filtered, Data, Gamma1, Position),
    transpose(Filtered, Transposed),
    gamma(Gamma2, Transposed),
    Position1 is Position + 1,
    filter_o2(Result, Filtered, Gamma2, Position1), !.

filter_co2(Data, [Data], _, _).

filter_co2(Result, Data, Epsilon, Position) :-
    nth0(Position, Epsilon, Epsilon1),
    filter(Filtered, Data, Epsilon1, Position),
    transpose(Filtered, Transposed),
    gamma(Gamma, Transposed),
    epsilon(Epsilon2, Gamma),
    Position1 is Position + 1,
    filter_co2(Result, Filtered, Epsilon2, Position1), !.

resolve_p2(Result, File) :-
    load_data(Data, File),
    transpose(Data, Transposed),
    gamma(Gamma, Transposed),
    filter_o2(O2Value, Data, Gamma, 0),
    b_to_d(O2, O2Value),
    epsilon(Epsilon, Gamma),
    filter_co2(CO2Value, Data, Epsilon, 0),
    b_to_d(CO2, CO2Value),
    Result is O2 * CO2, !.
