:- module(fun,
          [ op(950, xfy, $)
          , op(975, xfy, of)
          ]).

/*
%%	Function:callable $ X is det.
:- op(950, xfy, $).
$(_,_).
*/

:- dynamic user:function_expansion/3.
:- multifile user:function_expansion/3.
user:function_expansion(pi, 3.14159, circles_are_round).
user:function_expansion(hi, X, X = hello).

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
    ground(H0),
    user:function_expansion(H0, H, Guard),
    expand_arglist(T0, T, Guards),
    !.
expand_arglist([H0|T0], [H|T], Guards) :-          % subtree
    ground(H0),
    H0 =.. [Functor|Args0],
    expand_arglist(Args0, Args, NestedGuards),
    H =.. [Functor|Args],
    expand_arglist(T0, T, TailGuards),
    append(NestedGuards, TailGuards, Guards),
    !.
expand_arglist([H0|T0], [H0|T], Guards) :-
    var(H0),
    expand_arglist(T0, T, Guards).

% build a list out of a nested operator term. for example,
% (a,b,c) parses to ','(a,','(b,c)).  This converts between
% that form and simple list form: [a,b,c]
xfy_list(Op, Term, [Left|List]) :-
    Term =.. [Op, Left, Right],
    xfy_list(Op, Right, List),
    !.
xfy_list(_, Term, [Term]).

user:goal_expansion(T0, T) :-
    format('raw: ~p~n', [T0]),
    (_,_) \= T0,  % ignore conjunction of smaller goals
    (_;_) \= T0,  % ignore disjunction of smaller goals
    format('  inside~n'),
    T0 =.. [Functor|Args],
    fun:expand_arglist(Args, NewArgs, Preconditions),
    format('  guards: ~p~n', [Preconditions]),
    T1 =.. [Functor|NewArgs],
    xfy_list(',', Guard, Preconditions),
    format('  xfy_list: ~p~n', [Guard]),
    T = (Guard, T1),
    format('  done: ~p~n', [T]).

hmm :-
    say(hi, pi, bye, more(pi)),
    true.
