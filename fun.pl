:- module(fun,
          [ op(950, xfy, $)
          , op(975, xfy, of)
          ]).

/*
%%	Function:callable $ X is det.
:- op(950, xfy, $).
$(_,_).
*/

%%	user:function_expansion(Term, Replacement, Guard).
%
%	Like term_expansion/2, function_expansion/3 provides for macro
%	expansion of Prolog source code.  In this case, by expanding a
%	Term which is nested inside a parent term.  Term is replaced with
%	Replacement.  Guard is placed as a conjunction before the parent
%	term.
%
%   For example, a function macro which doubles its argument might
%   expand this
%
%	==
%	user:function_expansion(double(X), Y, Y is 2*X).
%	main :-
%	    V = 9,
%	    format('~p times 2 is ~p~n', [V, double(V)]).
%	==
%
%	into this
%
%	==
%	main :-
%	    V = 9,
%	    A is 2*V,
%	    format('~p times 2 is ~p~n', [V, A]).
%	==
:- dynamic user:function_expansion/3.
:- multifile user:function_expansion/3.

function_expansion_loop(T, T) :-  % so `X=fn(1,234)` works
    var(T),
    !.
function_expansion_loop(T0, T) :-
    user:function_expansion(T0, _, T),
    !.
function_expansion_loop(T0, T) :-  % look for expandable terms inside T0
    nonvar(T0),
    \+ user:function_expansion(T0, _, _),
    T0 =.. [Functor|Args0],
    maplist(function_expansion_loop, Args0, Args),
    T =.. [Functor|Args].

expand_arglist([], [], []).
expand_arglist([H0|T0], [H|T], [Guard|Guards]) :-  % leaf
    nonvar(H0),
    user:function_expansion(H0, H, Guard),
    expand_arglist(T0, T, Guards),
    !.
expand_arglist([H0|T0], [H|T], Guards) :-          % subtree
    nonvar(H0),
    H0 =.. [Functor|Args0],
    expand_arglist(Args0, Args, NestedGuards),
    H =.. [Functor|Args],
    expand_arglist(T0, T, TailGuards),
    append(NestedGuards, TailGuards, Guards),
    !.
expand_arglist([H0|T0], [H0|T], Guards) :-
    var(H0),
    expand_arglist(T0, T, Guards).

%%	xfy_list(?Op:atom, ?Term, ?List) is det.
%
%	True if List joined together with xfy operator Op gives Term.
%	Usable in all directions.  For example,
%
%	==
%	?- xfy_list(',', (a,b,c), L).
%	L = [a, b, c].
%	==
xfy_list(Op, Term, [Left|List]) :-
    Term =.. [Op, Left, Right],
    xfy_list(Op, Right, List),
    !.
xfy_list(_, Term, [Term]).

%%	control(+Term) is semidet.
%
%	True if Term is a control structure such as =,=, =;=, etc.
control((_,_)).
control((_;_)).
control((_->_)).
control((_*->_)).
control(\+(_)).

user:goal_expansion(T0, T) :-
    \+ control(T0),  % goal_expansion/2 already descends into these
    T0 =.. [Functor|Args],
    fun:expand_arglist(Args, NewArgs, Preconditions),
    T1 =.. [Functor|NewArgs],
    xfy_list(',', Guard, Preconditions),
    T = (Guard, T1).

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
