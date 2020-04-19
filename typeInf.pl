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
typeStatement(gvLet(Name, Code), T):-
    atom(Name), /* make sure we have a bound name */
    typeStatement(Code, T), /* infer the type of Code and ensure it is T */
    bType(T), /* make sure we have an infered type */
    (local(on) -> asserta(lvar(Name, T)) ; asserta(gvar(Name, T))). /* add definition to database */

/* if statements are encodes as:
    if(condition:Boolean, trueCode: [Statements], falseCode: [Statements])
*/
typeStatement(if(Cond, TrueB, FalseB), T) :-
    typeBoolExp(Cond), % Check if Cond is a boolean expression
    typeCode(TrueB, T), % Check the type of the true block 
    typeCode(FalseB, T). % asster that the type of the false block is the same as the true blcok

/* global function should have a name, a list of variables (Vars) with the 
last one being the return type, a list of statements that make up the function (Code).
We should assert the type of the last variable in Vars is the same as the the type of the
last statement in Code 
*/
% GLOBAL FUNCTION
typeStatement(gFunction(Name, Vars, Code), T) :-
    atom(Name), % Check that Name is an atom
    typeStatement(Code, T), % Find the type of the function's code
    typeCode(Vars, T), % Check that the return type (final type in parameter list) is the same as the function's return type
    bType(T), % assert that the function's return type is a valid type
    (local(on) -> asserta(lvar(Name, Vars)) ; asserta(gvar(Name, Vars))). % if local(on), add function to local scope, else add to global

% FUNCTION CALL
typeStatement(funcCall(Name, Params), T) :-
    atom(Name), % check that Name is an atom
    append(Params, [T], FType), % append parameters and return type, aka make it look like a function
    functionType(Name, Args), % lookup the function
    typeExpList(Args, FType), % assert that the function call's parameter list and the function signature match
    typeCode(Args, T). % assert the type of the function call

% LET IN
typeStatement(letIn(Stmts, Stmt), T) :-
    deleteLVars(), % delete all local variables
    asserta(local(on)), % "turn on" / allow saving local variables and functions
    typeCode(Stmts, _), % evaluate all statements and save their type to lvar (ignore return type)
    resetLocal(), % "turn off" / disable saving of local variables and function
    typeStatement(Stmt, T), % Evaluate the type of the "in" statement
    deleteLVars(). % delete local variables to close scope (and prevent propogation outside of the local scope) 

% WHERE 
typeStatement(where(Stmts, WStmts), T) :-
    deleteLVars(), % delete all local variables
    asserta(local(on)), % "turn on" / allow saving local variables and functions
    typeCode(WStmts, _), % evaluate all statements in where block and save type to lvar (ignore return type)
    resetLocal(), % "turn off" / disable saving of local variables and function
    typeCode(Stmts, T), % evaluate type of the statements above where block
    deleteLVars(). % delete local variables to close scope (and prevent propogation outside of the local scope)



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
    deleteLVars(),
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

deleteGVars():-retractall(gvar(_, _)), asserta(gvar(_X,_Y):-false()). % deletes all global variables
deleteLVars():-retractall(lvar(_, _)), asserta(lvar(_X,_Y):-false()). % deletes all local variables
resetLocal():-retractall(local(_)), asserta(local(off)). % turns off saving of local variables

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
