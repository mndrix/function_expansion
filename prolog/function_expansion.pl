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

%!    expand(+Goal, -Expanded) is semidet
%
%     Performs top level function expansion on =Goal= resulting in
%     =Expanded=.
expand(Goal, Expanded) :-
    expand(0, Goal, Expanded, _).

%!    expand(+MSpec, +Term, -Expanded, -Guards) is semidet
%
%     =MSpec= is a meta argument specifier (see meta_predicate/1).
%
%     TODO: meta argument specifier '^' is not supported. Not sure
%     what the best way to support it is, since it is not a predicate
%     itself and we only want to treat it as such when it is not the
%     enclosing context of the bagof/3, setof/3, etc.
%
%     TODO: support '//' meta argument
%
expand(MSpec, Term, Expanded, Guards) :-
    % This does the expansion of Term
    expand_aux(Term,
               Expanded0   /* expanded Term */,
               Guards0,    /* list of guards for Term */
               WasExpanded /* did any expansion take place in Term? */
              ),

    %% This uses MSpec to determine whether to merge the expansion and
    %% guards here, or to send the guards up to the parent of Term. 
    (WasExpanded == true -> 
         (% if term is not callable, pass guards to parent
             memberchk(MSpec, ['+', '*', '?', '-', ':']) ->  
             Expanded-Guards = Expanded0-Guards0
         ; % if term is a callable expecting MSpec additional args,
           % merge expanded term and guards, and send empty list of guards to parent
           (integer(MSpec), MSpec >= 0) -> % 
           term_guards_goal(MSpec, Expanded0, Guards0, Expanded),
           Guards = [] 
         ;
         print_message(warning, function_expansion(incomplete(Term, unsupported_meta_argument_specifier(MSpec))))
         )
    ; % WasExpanded == false ->
      Expanded-Guards = Expanded0-Guards0
    ).

%!    expand_aux(+Term, -Expanded, -Guards, -WasExpanded) is semidet
%
%     Expands =Term= into an expanded term =Expanded=, a list of gaurd
%     goals =Guards=. =WasExpanded= is true if any actual expansion
%     occured.
%
expand_aux(Term, Term, [], WasExpanded) :- 
    var(Term),
    !,
    WasExpanded = false.
expand_aux(Term0, Term1, [Guard], WasExpanded) :-
    user:function_expansion(Term0, Term1, Guard),    
    !,
    WasExpanded = true.
expand_aux(Term, Expanded, Guards, WasExpanded) :-
    Term =.. [_],
    !,
    Expanded = Term,
    Guards = [],
    WasExpanded = false,
    !.
expand_aux(Term, Expanded, Guards, WasExpanded) :-
    Term =.. [F|Args0],
    length(Args0, A), 
    pred_meta_arg_specs(F/A, MSpecs),
    maplist(expand, MSpecs, Args0, Args, GuardLists),
    append(GuardLists, Guards),
    Expanded =.. [F|Args],
    (Term \== Expanded ->
         WasExpanded = true;
     WasExpanded = false
    ),
    !.
expand_aux(Term, Term, [], false).
    

term_guards_goal(_, Term, [], Goal) :-
    !,
    Term = Goal.
term_guards_goal(A, Term, Guards, Goal) :-
    xfy_list(',', GuardTerm, Guards),
    (A =:= 0 ->
         Goal = (GuardTerm, Term)
    ; % otherwise, eta expand
      length(Vs, A),
      Goal1 =.. [call, Term|Vs],
      Goal = Vs >> (GuardTerm, Goal1)
    ).
    


pred_meta_arg_specs(F/A, MetaArgSpecs) :-
    (known_meta_arg_specs(F/A, MetaArgSpecs) ->
        true
    ;
     default_meta_arg_specs(F/A, MetaArgSpecs)
    ).

known_meta_arg_specs(F/A, MetaArgSpecs) :-
    functor(H, F, A), 
    predicate_property(H, meta_predicate(MSpec)),
    MSpec =.. [_|MetaArgSpecs].

default_meta_arg_specs(_/A, Types) :-
    length(Types, A),
    maplist(=('?'), Types).

    

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
    
    %% NB: this is needed because expand/2 calls predicate_property/2
    %% which then tries to load the module in which a predicate is
    %% defined, which in tern calls goal_expansion/2, leading to an
    %% infinite loop ... not sure this completely solves the issue
    \+ prolog_load_context(term, :- module(_, _)),
    
    expand(T0, T).



