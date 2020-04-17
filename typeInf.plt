:- begin_tests(typeInf).
:- include(typeInf). 

/* Note: when writing tests keep in mind that 
    the use of of global variable and function definitions
    define facts for gvar() predicate. Either test
    directy infer() predicate or call
    delegeGVars() predicate to clean up gvar().
*/



% tests for typeExp

%ADDITION
test(typeExp_plus) :- 
    typeExp(+(int,int), int).

test(typeExp_plus_F, [fail]) :- % this test should fail
    typeExp(+(float, float), int).

test(typeExp_plus_T, [true(T == int)]) :-
    typeExp(+(int, int), T).

%SUBTRACTION
test(typeExp_minus) :- 
    typeExp(-(int,int), int).

test(typeExp_minus_F, [fail]) :- % this test should fail
    typeExp(-(float, float), int).

test(typeExp_minus_T, [true(T == int)]) :-
    typeExp(-(int, int), T).

%Multiplication
test(typeExp_mult) :- 
    typeExp(*(int,int), int).

test(typeExp_mult_F, [fail]) :- % this test should fail
    typeExp(*(float, float), int).

test(typeExp_mult_T, [true(T == int)]) :-
    typeExp(*(int, int), T).

%Division
test(typeExp_dic) :- 
    typeExp(/(int,int), int).

test(typeExp_mult_F, [fail]) :- % this test should fail
    typeExp(/(float, float), int).

test(typeExp_mult_T, [true(T == int)]) :-
    typeExp(/(int, int), T).



% NOTE: use nondet as option to test if the test is nondeterministic

% test for statement with state cleaning
test(typeStatement_gvar, [nondet, true(T == int)]) :- % should succeed with T=int
    deleteGVars(), /* clean up variables */
    typeStatement(gvLet(v, T, +(X, Y)), unit),
    assertion(X == int), assertion( Y == int), % make sure the types are int
    gvar(v, int). % make sure the global variable is defined

% same test as above but with infer 
test(infer_gvar, [nondet]) :-
    infer([gvLet(v, T, +(X, Y))], unit),
    assertion(T==int), assertion(X==int), assertion(Y=int),
    gvar(v,int).

test(infer_complexIf, [nondet]) :-
    infer([gvLet(v, T, +(X, Y)), if(v < 3, [3], [4])], unit),
    assertion(T==int), assertion(X==int), assertion(Y=int),
    gvar(v,int).

% test custom function with mocked definition
test(mockedFct, [nondet]) :-
    deleteGVars(), % clean up variables since we cannot use infer
    asserta(gvar(my_fct, [int, float])), % add my_fct(int)-> float to the gloval variables
    typeExp(my_fct(X), T), % infer type of expression using or function
    assertion(X==int), assertion(T==float). % make sure the types infered are correct

test(simple_if, [nondet]) :-
    typeStatement( if(3 > 4, [3], [4]), T),
    assertion(T==int).


:-end_tests(typeInf).
