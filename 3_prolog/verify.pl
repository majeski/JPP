% Michal Majewski

:- ensure_loaded(library(lists)).


% Operators

:- op(500, 'xfx', '--').

:- op(700, 'xfx', '<>').
X <> Y :- X \= Y.


% Verify

verify(N, _) :-
    \+ correctN(N),
    parameterErr(N).

verify(N, Program) :-
    correctN(N),
    set_prolog_flag(fileerrors, off),
    ( see(Program) ->
      read(vars(Vars)),
      read(arrays(Arrs)),
      read(program(Stmts)),
      seen,
      initState(prog(Vars, Arrs, Stmts), N, State),
      unsafePath([nop|Stmts], N, State, Result),
      printResult(Result)
    ; fileErr(Program)
    ).

%% correctN(+N)
correctN(N) :-
    integer(N),
    N > 0.

%% unsafePath(+Statements, +N, +State, -Result)
%
% Result - safe/unsafe(Path, Processes, StateNumber)
unsafePath(Stmts, N, State, Result) :-
    unsafePath(Stmts, N, State, [], _, Result).

%% unsafePath(+Statements, +N, +State, 
%%   +VisitedStates, -VisitedStatesResult, -Result)
unsafePath(Stmts, N, State, Visited, VisitedOut, Result) :-
    ( member(State, Visited) ->
      Result = safe,
      VisitedOut = Visited
    ; NewVisited = [State|Visited],
      ( unsafe(Stmts, State, Procs) ->
        length(NewVisited, StateNum),
        Result = unsafe([], Procs, StateNum)
      ; stepForEachPid(Stmts, 0, N, State, NewVisited, VisitedOut, Result)
      )
    ).

%% stepForEachPid(+Statements, +Pid, +N, +State,
%%   +VisitedStates, -VisitedStatesResult, -Result)
stepForEachPid(_, N, N, _, Visited, Visited, safe).
stepForEachPid(Stmts, Pid, N, State, Visited, VisitedOut, Result) :-
    Pid < N,
    step(Stmts, State, Pid, Eip, NewState),
    unsafePath(Stmts, N, NewState, Visited, NewVisited, TmpResult),
    ( unsafe(Path, Procs, StateNum) = TmpResult ->
      Result = unsafe([Pid--Eip|Path], Procs, StateNum)
    ; NextPid is Pid + 1,
      stepForEachPid(Stmts, NextPid, N, State, 
          NewVisited, VisitedOut, Result)
    ).

%% step(+Statements, +State, +Pid, -Eip, -ResultState)
%
% Eip - instruction pointer to the executed instruction
% StateR - state after execution
step(Stmts, State, Pid, Eip, StateR) :-
    getEip(State, Pid, Eip),
    nth0(Eip, Stmts, Stmt), % get statement under eip
    execStmt(Stmt, State, Pid, StateR).


% Print

parameterErr(N) :-
    format('Error: parametr ~w powinien byc liczba > 0', [N]).

fileErr(Filename) :-
    format('Error: brak pliku o nazwie - ~w', [Filename]).
    
printResult(safe) :-
    format('Program jest poprawny (bezpieczny).~n', []).

printResult(unsafe(Path, Procs, StateIdx)) :-
    format('Program jest niepoprawny: stan nr ~d nie jest bezpieczny.~n',
        [StateIdx]),
    format('Niepoprawny przeplot:~n', []),
    printPath(Path),
    printProcs(Procs).

printPath([]).
printPath([P--Eip|T]) :-
    format('  Proces ~d: ~d~n', [P, Eip]),
    printPath(T).

printProcs([H|Procs]) :-
    format('Procesy w sekcji krytycznej: ~d', [H]),
    printProcsList(Procs),
    format('.~n', []).

printProcsList([]).
printProcsList([H|Procs]) :-
    format(', ~d', [H]),
    printProcsList(Procs).


%   Statements

%% execStmt(+Statement, +State, +Pid, -ResultState)
%
% eip is incremented after each statement except goto/condGoto(true, ...)
execStmt(goto(N), State, Pid, StateR) :-
    setEip(State, Pid, N, StateR).

execStmt(sekcja, State, Pid, StateR) :-
    nextInstr(State, Pid, StateR).

execStmt(condGoto(BExpr, N), State, Pid, StateR) :-
    ( evalBExpr(BExpr, State, Pid) ->
      execStmt(goto(N), State, Pid, StateR)
    ; nextInstr(State, Pid, StateR)
    ).

execStmt(assign(Id, Expr), State, Pid, StateR) :-
    atom(Id),
    evalExpr(Expr, State, Pid, Val),
    setVar(State, Id, Val, StateTmp),
    nextInstr(StateTmp, Pid, StateR).

execStmt(assign(arr(ArrId, IdxExpr), ValExpr), State, Pid, StateR) :-
    evalExpr(IdxExpr, State, Pid, Idx),
    evalExpr(ValExpr, State, Pid, Val),
    setArrElem(State, ArrId, Idx, Val, StateTmp),
    nextInstr(StateTmp, Pid, StateR).


% Evaluation

% arithmetic expressions

%% evalExpr(+Expr, +State, +Pid, -ResultInt)
evalExpr(pid, _, Pid, Pid).

evalExpr(Num, _, _, Num) :-
    integer(Num).

evalExpr(Id, State, _, Val) :-
    atom(Id),
    getVar(State, Id, Val).

evalExpr(arr(ArrId, IdxExpr), State, Pid, Val) :-
    evalExpr(IdxExpr, State, Pid, Idx),
    getArrElem(State, ArrId, Idx, Val).

evalExpr(Expr, State, Pid, Val) :-
    Expr =.. [Oper, Lhs, Rhs],
    member(Oper, [+, -, *, /]),
    evalExpr(Lhs, State, Pid, L),
    evalExpr(Rhs, State, Pid, R),
    IntExpr =.. [Oper, L, R],
    Val is IntExpr.

%   logical expressions

%% evalBExpr(+BoolExpression, +State, +Pid)
%
% true if BoolExpression result is true
evalBExpr(BExpr, State, Pid) :-
    BExpr =.. [Oper, Lhs, Rhs],
    evalExpr(Lhs, State, Pid, L),
    evalExpr(Rhs, State, Pid, R),
    call(Oper, L, R).


% State manipulations

%   var

getVar(state(Vars, _, _), Id, Val) :-
    member(Id--Val, Vars).

setVar(state(Vars, A, B), Id, Val, state(NewVars, A, B)) :-
    updateMap(Vars, Id, Val, NewVars).

%   array

getArrElem(State, ArrId, Idx, Val) :-
    getArr(State, ArrId, Arr),
    nth0(Idx, Arr, Val).

getArr(state(_, Arrs, _), ArrId, Arr) :-
    member(ArrId--Arr, Arrs).

%% setArrElem(+State, +ArrayID, +Index, +Value, -ResultState)
%
% updates value at index Index in an array with id ArrayID
setArrElem(state(A, Arrs, B), ArrId, Idx, Val, state(A, NewArrs, B)) :-
    State = state(A, Arrs, B),
    getArr(State, ArrId, OldArr),
    updateArray(OldArr, Idx, Val, NewArr),    % update array itself
    updateMap(Arrs, ArrId, NewArr, NewArrs).  % update map with arrays

%   eip

getEip(state(_, _, Eips), Pid, Eip) :-
    nth0(Pid, Eips, Eip).

setEip(state(A, B, Eips), Pid, Eip, state(A, B, NewEips)) :-
    updateArray(Eips, Pid, Eip, NewEips).

%% nextInstr(+State, +Pid, -NewState)
%
% NewState - state with incremented eip for given pid
nextInstr(State, Pid, NewState) :-
    getEip(State, Pid, Eip),
    NewEip is Eip + 1,
    setEip(State, Pid, NewEip, NewState).

%   critical section

%% unsafe(+Statements, +State, -Processes)
%
% true if there are at least two processes
% which eip points to the critical section
% Procs - list of process ids inside the critical section
unsafe(Stmts, state(_, _, Eips), Procs) :-
    procsInSection(Eips, Stmts, Procs),
    length(Procs, Count),
    Count > 1.

%% procsInSection(+Eips, +Statements, -Processes)
procsInSection(Eips, Stmts, Procs) :-
    procsInSection(Eips, 0, Stmts, [], Procs).

%% procsInSection(+Eips, +Pid, +Statements, +Acc, -Processes)
procsInSection([], _, _, Procs, Procs).
procsInSection([Eip|Eips], Pid, Stmts, Acc, Procs) :-
    NewPid is Pid + 1,
    ( nth0(Eip, Stmts, sekcja) -> % if eip points to 'sekcja' statement
      procsInSection(Eips, NewPid, Stmts, [Pid|Acc], Procs)
    ; procsInSection(Eips, NewPid, Stmts, Acc, Procs)
    ).


% State initialization

%% initState(+Program, +N, -State)
%
% Program format:
%    program(VarIds, ArrIds, Statements)
% State format:
%    state(Vars, Arrs, Eips)
%    Vars = [id1--val1, ...]
%    Arrs = [id1--[val10, val11, ...], ...]
%    Eips = [instr ptr, ...]
initState(prog(VarIds, ArrIds, _), N, state(Vars, Arrs, Eips)) :-
    createArray(N, 0, Arr),
    createMap(VarIds, 0, Vars),
    createMap(ArrIds, Arr, Arrs),
    createArray(N, 1, Eips).


% Map/Array operations 

%% createArray(+N, +Value, -Array)
%
% Array is a list with N elements with value Value.
createArray(0, _, []).
createArray(N, V, [V|R]) :-
    N > 0,
    N1 is N - 1,
    createArray(N1, V, R).

%% updateArray(+Array, +Index, +Value, -ResultArray)
updateArray([_|Arr], 0, Val, [Val|Arr]). 
updateArray([H|Arr], Idx, Val, [H|R]) :-
    Idx > 0,
    PrevIdx is Idx - 1,
    updateArray(Arr, PrevIdx, Val, R).

%% createMap(+Keys, +Value, -Map)
%
% Map is a list with pairs (Key--Value) with keys from Keys
% initial value for each entry is Value
createMap([], _, []).
createMap([Key|Keys], V, [Key--V|R]) :-
    createMap(Keys, V, R).

%% updateMap(+Map, +Key, +Value, -ResultMap)
updateMap([Key--_|Map], Key, Val, [Key--Val|Map]).
updateMap([H|Map], Key, Val, [H|R]) :-
    updateMap(Map, Key, Val, R).
