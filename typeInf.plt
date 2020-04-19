:- begin_tests(typeInf).
:- include(typeInf). 

/* Note: when writing tests keep in mind that 
    the use of of global variable and function definitions
    define facts for gvar() predicate. Either test
    directy infer() predicate or call
    delegeGVars() predicate to clean up gvar().
*/

%testing expressions (int, float, bool) as statements
test(exprStat, [nondet]) :-
    infer([
        3,
        true,
        float
        ], Ret),
        assertion(Ret==float).


% tests for typeExp
%ADDITION
test(typeExp_plus, [nondet]) :- 
    typeExp(+(int,int), int).

test(typeExp_plus_Fail, [fail]) :- % this test should fail
    typeExp(+(int, int), float).

test(typeExp_plus_T, [nondet, true(T == int)]) :-
    typeExp(+(int, int), T).

%SUBTRACTION
test(typeExp_minus, [nondet]) :- 
    typeExp(-(int,int), int).

test(typeExp_minus_F, [fail]) :- % this test should fail
    typeExp(-(float, float), int).

test(typeExp_minus_T, [nondet, true(T == int)]) :-
    typeExp(-(int, int), T).

%MULTIPLICATION
test(typeExp_mult, [nondet]) :- 
    typeExp(*(int,int), int).

test(typeExp_mult_F, [fail]) :- % this test should fail
    typeExp(*(float, float), int).

test(typeExp_mult_T, [nondet, true(T == int)]) :-
    typeExp(*(int, int), T).

%DIVISION
test(typeExp_dic, [nondet]) :- 
    typeExp(/(int,int), int).

test(typeExp_mult_F, [fail]) :- % this test should fail
    typeExp(/(float, float), int).

test(typeExp_mult_T, [nondet, true(T == int)]) :-
    typeExp(/(int, int), T).

% NOTE: use nondet as option to test if the test is nondeterministic




% Tests for Statements with state cleaning
%GLOBAL VARIABLE DECLARATION
test(typeStatement_gvar, [nondet, true(T == int)]) :- % should succeed with T=int
    deleteGVars(), /* clean up variables */
    typeStatement(gvLet(v, +(X, Y)), T),
    assertion(X == int), assertion( Y == int), % make sure the types are int
    gvar(v, int). % make sure the global variable is defined

% global variable with infer 
test(infer_gvar, [nondet]) :-
    infer([gvLet(v, +(X, Y))], T),
    assertion(T==int), assertion(X==int), assertion(Y=int),
    gvar(v,int).


% IF STATEMENT
test(simple_if, [nondet]) :-
    deleteGVars(),
    typeStatement( if(true, [3], [4]), T),
    assertion(T==int).

% if statement with infer and global variable
test(infer_complexIf, [nondet]) :-
    infer([gvLet(v, +(X, Y)), if(v < 3, [*(4.0, 5.0)], [/(7.0, 2.0)])], Ret),
    assertion(X==int), assertion(Y==int), assertion(Ret == float),
    gvar(v,int).


% GLOBAL FUNCTION STATEMENT
test(simple_globalFunction, [nondet]) :-
    deleteGVars(),
    typeStatement(gFunction(add, [X, Y, int], +(X, Y)), Ret),
    assertion(X==int), assertion(Y==int), assertion(Ret == int),
    gvar(add, [int, int, int]).

% global function with infer
test(infer_globalFunction, [nondet]) :-
    infer([gFunction(multiply, [string, X, Y, float], +(X, Y))], Ret),
    assertion(X==float), assertion(Y==float), assertion(Ret==float),
    gvar(multiply, [string, float, float, float]).


% FUNCTION CALL
test(simple_functioncall, [nondet]) :-
    deleteGVars(),
    typeStatement(gFunction(add, [string, bool, int], +(int, int)), int), % add global function
    typeStatement(funcCall(add, [X, Y]), Ret), %test function call
    assertion(X==string), assertion(Y==bool), assertion(Ret==int).

% global function declaration and call with infer
test(infer_gFuncDecAndCall, [nondet]) :-
    infer([ gFunction(add, [string, bool, int], +(int, int)), funcCall(add, [X, Y]) ], Ret),
    assertion(X==string), assertion(Y==bool), assertion(Ret==int),
    gvar(add, [string, bool, int]).

% function call in global let statement with infer
test(infer_FuncCallInGlobalLet, [nondet]) :-
    infer([ 
        gFunction(add, [string, bool, int], +(int, int)), 
        gvLet(q, funcCall(add, [X, Y])) 
        ], Ret),
    assertion(X==string), assertion(Y==bool), assertion(Ret==int).
    gvar(q, int).


% LET IN
test(simple_letIn, [nondet]) :-
    deleteGVars(),
    typeStatement(letIn([gvLet(p, -(4, 3))], +(p, 2)), Ret),
    assertion(Ret==int),
    not(gvar(p, int)).

% let in with infer
test(infer_letIn, [nondet]) :-
    infer([
        letIn([gvLet(p, -(4, 3))], +(p, 2))
        ], Ret),
    assertion(Ret==int),
    not(gvar(p, int)).


% WHERE STATEMENT
test(simple_where, [nondet]) :-
    deleteGVars(),
    typeStatement(
        where(
            [ gvLet(var1, wVar1), gvLet(var2, +(var1, wVar2)) ],
            [ gvLet(wVar1, int), gvLet(wVar2, int) ]),
            Ret
        ),
    assertion(Ret==int),
    gvar(var1, int), gvar(var2, int),
    not(gvar(wVar1, int)), not(gvar(wVar2, int)).

% where statement with infer
test(infer_where, [nondet]) :-
    infer(
        [ where(
            [ gFunction(multByQ, [float, float], *(float, q)), gvLet(f, +(multByQ(r), 6.0)) ],
            [ gvLet(q, +(float, float)), gvLet(r, 4.0) ])
            
        ], Ret),
    assertion(Ret==float), 
    gvar(multByQ, [float, float]), gvar(f, float),
    not(gvar(q, float)), not(gvar(r, float)).



% test custom function with mocked definition
test(mockedFct, [nondet]) :-
    deleteGVars(), % clean up variables since we cannot use infer
    asserta(gvar(my_fct, [int, float])), % add my_fct(int)-> float to the gloval variables
    typeExp(my_fct(X), T), % infer type of expression using or function
    assertion(X==int), assertion(T==float). % make sure the types infered are correct






:-end_tests(typeInf).
