% vim: filetype=prolog
:- use_module(library(function_expansion)).
user:function_expansion(incr(N), X, X is N+1).
user:function_expansion(pi_atom, Atom, true) :-
    Pi is pi,
    atom_number(Atom, Pi).

:- use_module(library(tap)).

% not exactly what's in the synopsis, but close
'incr(N)' :-
    format(atom(A), 'After 2 comes ~p', [incr(2)]),
    A = 'After 2 comes 3'.

% other examples suggested by the documentation
'pi function producing an atom' :-
    % test only the prefix to avoid system-dependent float problems
    atom_concat('3.14159', _, pi_atom).
