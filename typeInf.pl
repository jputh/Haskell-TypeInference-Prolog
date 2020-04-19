:- dynamic gvar/2, local/1, lvar/2.

typeExp(X, int) :-
    integer(X).

typeExp(X, float) :-
    float(X).

typeExp(X, bool) :-
    typeBoolExp(X).



/* match functions by unifying with arguments 
    and infering the result
*/
typeExp(Fct, T):-
    \+ var(Fct), /* make sure Fct is not a variable */ 
    \+ atom(Fct), /* or an atom */
    functor(Fct, Fname, _Nargs), /* ensure we have a functor */
    !, /* if we make it here we do not try anything else */
    Fct =.. [Fname|Args], /* get list of arguments */
    append(Args, [T], FType), /* make it loook like a function signature */
    functionType(Fname, TArgs), /* get type of arguments from definition */
    typeExpList(FType, TArgs). /* recurisvely match types */


% assert type of global variable in code
typeExp(Var, T) :-
    atom(Var),
    (gvar(Var, T) ; lvar(Var, T)),
    bType(T).


/* propagate types */
typeExp(T, T).

/* list version to allow function mathine */
typeExpList([], []).
typeExpList([Hin|Tin], [Hout|Tout]):-
    typeExp(Hin, Hout), /* type infer the head */
    typeExpList(Tin, Tout). /* recurse */



hasComparison(int).
hasComparison(float).
hasComparison(string).

numericType(int).
numericType(float).

/* predicate to infer types for boolean expressions */
typeBoolExp(true).
typeBoolExp(false). 
typeBoolExp( X < Y) :- 
    typeExp(X, T),
    typeExp(Y, T),
    hasComparison(T).
typeBoolExp( X > Y) :- 
    typeExp(X, T),
    typeExp(Y, T),
    hasComparison(T).
typeBoolExp(X =< Y) :- 
    typeExp(X, T),
    typeExp(Y, T),
    hasComparison(T).
typeBoolExp( X >= Y) :- 
    typeExp(X, T),
    typeExp(Y, T),
    hasComparison(T).


/* TODO: add statements types and their type checking */



/* global variable definition
    Example:
        gvLet(v, T, int) ~ let v = 3;
 */
typeStatement(gvLet(Name, T, Code), unit):-
    atom(Name), /* make sure we have a bound name */
    typeExp(Code, T), /* infer the type of Code and ensure it is T */
    bType(T), /* make sure we have an infered type */
    (local(on) -> asserta(lvar(Name, T)) ; asserta(gvar(Name, T))). /* add definition to database */

/* if statements are encodes as:
    if(condition:Boolean, trueCode: [Statements], falseCode: [Statements])
*/
typeStatement(if(Cond, TrueB, FalseB), T) :-
    typeBoolExp(Cond),
    typeCode(TrueB, T),
    typeCode(FalseB, T).

/* global function should have a name, a list of variables (Vars) with the 
last one being the return type, a list of statements that make up the function (Code).
We should assert the type of the last variable in Vars is the same as the the type of the
last statement in Code 
*/
% GLOBAL FUNCTION
typeStatement(gFunction(Name, Vars, Code), T) :-
    atom(Name),
    typeCode(Code, T),
    typeCode(Vars, T),
    bType(T),
    (local(on) -> asserta(lvar(Name, Vars)) ; asserta(gvar(Name, Vars))).

% FUNCTION CALL
typeStatement(funcCall(Name, Params), T) :-
    atom(Name),
    append(Params, [T], FType),
    functionType(Name, Args),
    typeExpList(Args, FType),
    typeCode(Args, T).

% LET IN
typeStatement(letIn(Stmts, Stmt), T) :-
    deleteLVars(),
    asserta(local(on)),
    typeCode(Stmts, _),
    resetLocal(),
    typeStatement(Stmt, T),
    deleteLVars().





typeStatement(X, T) :-
    typeExp(X, T).

/* Code is simply a list of statements. The type is 
    the type of the last statement 
*/
typeCode([S], T):-typeStatement(S, T).
typeCode([S, S2|Code], T):-
    typeStatement(S,_T),
    typeCode([S2|Code], T).

/* top level function */
infer(Code, T) :-
    is_list(Code), /* make sure Code is a list */
    deleteGVars(), /* delete all global definitions */
    typeCode(Code, T).

/* Basic types
    TODO: add more types if needed
 */
bType(int).
bType(float).
bType(string).
bType(bool).
bType(unit). /* unit type for things that are not expressions */
/*  functions type.
    The type is a list, the last element is the return type
    E.g. add: int->int->int is represented as [int, int, int]
    and can be called as add(1,2)->3
 */
bType([H]):- bType(H).
bType([H|T]):- bType(H), bType(T).

/*
    TODO: as you encounter global variable definitions
    or global functions add their definitions to 
    the database using:
        asserta( gvar(Name, Type) )
    To check the types as you encounter them in the code
    use:
        gvar(Name, Type) with the Name bound to the name.
    Type will be bound to the global type
    Examples:
        g

    Call the predicate deleveGVars() to delete all global 
    variables. Best wy to do this is in your top predicate
*/

deleteGVars():-retractall(gvar(_, _)), asserta(gvar(_X,_Y):-false()).
deleteLVars():-retractall(lvar(_, _)), asserta(lvar(_X,_Y):-false()).
resetLocal():-retractall(local(_)), asserta(local(off)).

/*  builtin functions
    Each definition specifies the name and the 
    type as a function type

    TODO: add more functions
*/

/*

iplus :: int -> int -> int

*/

fType((+), [T, T, T]) :- numericType(T).
fType((-), [T, T, T]) :- numericType(T).
fType((*), [T, T, T]) :- numericType(T).
fType((/), [T, T, T]) :- numericType(T).
fType(fToInt, [float,int]).
fType(iToFloat, [int,float]).
fType(print, [_X, unit]). /* simple print */

/* Find function signature
   A function is either buld in using fType or
   added as a user definition with gvar(fct, List)
*/

% Check the user defined functions first
functionType(Name, Args):-
    gvar(Name, Args),
    is_list(Args). % make sure we have a function not a simple variable

% Check first built in functions
functionType(Name, Args) :-
    fType(Name, Args). % make deterministic

% This gets wiped out but we have it here to make the linter happy
gvar(_, _) :- false().
lvar(_, _) :- false().
