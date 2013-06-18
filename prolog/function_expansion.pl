:- module(function_expansion, []).
:- use_module(library(lists), [append/3]).
:- use_module(library(apply), [exclude/3]).

%%	user:function_expansion(+Term, -Replacement, -Guard) is semidet.
%
%	Like term_expansion/2, function_expansion/3 provides for macro
%	expansion of Prolog source code.  In this case, by expanding
%	Term which is nested inside a parent term.  Term is replaced with
%	Replacement.  Guard is placed as a conjunction before the parent
%	term.  Guard typically binds Replacement in some useful fashion.
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
%
%	Mathematical constants might be implemented like
%
%	==
%	user:function_expansion(pi, 3.14159, true).
%	==
:- dynamic user:function_expansion/3.
:- multifile user:function_expansion/3.

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
    function_expansion:expand_arglist(Args, NewArgs, Preconditions),
    T1 =.. [Functor|NewArgs],

    % remove guards that are always true
    exclude(==(true), Preconditions, NoTrues),
    (   xfy_list(',', Guard, NoTrues)
    ->  T = (Guard, T1)
    ;   T = T1   % empty guard clause
    ).
