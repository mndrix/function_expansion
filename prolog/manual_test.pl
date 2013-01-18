:- use_module(function_expansion).

user:function_expansion(pi, 3.14159, circles_are_round).
user:function_expansion(hi, X, X = hello).
user:function_expansion(e, 2.71828, true).
hmm :-
    say(hi, pi, bye, more(pi)),
    (   true -> writeln(e); fail),
    true.

user:function_expansion(double(X), Y, Y is 2*X).
ex :-
    V = 9,
    format('~p times 2 is ~p~n', [V, double(V)]).
