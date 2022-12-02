% Instructions:
%   - Please submit ONLY this file up to GradeScope!
%   - Implement as many of the predicates as you can as specified in the assignment!
%   - Have lots of fun! Wahoo!
%   - Do not change the module information of this file or the name of this file (i.e., do not change line 6)!
:- module( ex3, [ myappend/3 , myreverse/2 , myflatten/2 , mymember/2 , myremove/3 , mymember2/2 , mysubstring/2 , mysublists/2 , mypermutation/2 , grandfather/2 , grandmother/2 , brother/2 , sister/2 , sibling/2 , cousin/2 , contact/3 , josephus/5 ]).


%%%% Question 1.
myappend([], X, X).
myappend([X|XS], Y, [X|ZS]) :- myappend(XS, Y, ZS).

samelength([], []).
samelength([X|XS], [Y|YS])  :- samelength(XS, YS).

shunt([], X, X).
shunt([H|T], S, Z) :- shunt(T, [H|S], Z).

myreverse(X,Y) :-
    samelength(X,Y),
    shunt(X, [], Y).

myflatten([],[]).
myflatten([[]|XS], YS) :- myflatten(XS, YS).
myflatten([[X|XS]|XSS], [X|YS]) :- myflatten([XS|XSS], YS).

mymember(X, [X|_]).
mymember(X, [_|YS]) :- mymember(X, YS).

myremove(X, [X|Y], Y).
myremove(X, [Y|YS], [Y|ZS]) :- myremove(X, YS, ZS).


%%%% Question 2.
mymember2(X, Y) :- myremove(X, Y, R1) , myremove(X, R1, R2) , \+ mymember(X, R2).

%%%% Question 3.
mysubstring([], _).
mysubstring([X|XS], [X|YS]) :- myappend(XS, REST, YS) ; mysubstring([X|XS], YS).
mysubstring(XS, [_|YS]) :- mysubstring(XS, YS).


%%%% Question 4
issublist(_, []).
issublist(X, [Y|YS])    :- myremove(Y, X, REM) , issublist(REM, YS).

sublists([],[[]]).
sublists(X, [Y|YS])  :- issublist(X, Y) , sublists(X, YS).

mysublists(X, Y) :-
    sublists(X,Y).


%%%% Question 5
perm([], []).
perm(X, [Y|YS])    :- myremove(Y, X, REM) , perm(REM, YS).
mypermutation(X, Y) :-
    samelength(X,Y),
    perm(X,Y).


%%%% Question 6

% Understand these predicates as follows.
%   son(Mom, Dad, Child)      is read as ``Child is the son of the mother Mom and the father Dad''
%   daughter(Mom, Dad, Child) is read as ``Child is the daughter of the mother Mom and the father Dad''

son(mymom, mydad, theson).
daughter(mymom, mydad, thedaughter).
% Add your own family members too!



% Understand these predicates as follows.
%   grandfather(A,B). is read as ``A is a grandfather of B''
%   grandmother(A,B). is read as ``A is a grandmother of B''
%   brother(A,B).     is read as ``A is a brother of B''
%   sister(A,B).      is read as ``A is a sister of B''
%   sibling(A,B).     is read as ``A is a sibling of B''
%   cousin(A,B).      is read as ``A is a cousin of B''

parent(A,B):-
    father(A,B)
    ;
    mother(A,B).
father(A,B):-
    son(_, A, B);
    daughter(_, A, B).
mother(A,B):-
    son(A, _, B);
    daughter(A, _, B).
grandfather(A,B):-
    father(A,P),
    parent(P,B).
grandmother(A,B):-
    mother(A,P),
    parent(P,B).
brother(A,B):-
    A \= B,
    son(M,D,A),
    (
        son(M,D,B)
    ;
        daughter(M,D,B)
    ).
sister(A,B):-
    A \= B,
    daughter(M,D,A),
    (
        son(M,D,B)
    ;
        daughter(M,D,B)
    ).
sibling(A,B):-
    brother(A,B);
    sister(A,B).
cousin(A,B):-
    A \= B,
    parent(A,PA),
    parent(B,PB),
    sibling(PA,PB).


%%%% Question 7
contact(_,_,_).


%%%% Question 8
% Please see the assignment for the logic puzzle.
% This question will just be hand graded!


%%%% Question 9
% The parameters are as follows...
%   - NumberOfSoldiers: the total number of soldiers including Josephus and his accomplice (> 2)
%   - StartingPosition: the starting position
%   - N: The selected number to count down
%   - J: Output position for Josephus (< NumberOfSoldiers)
%   - A: Output position for the accomplice (< NumberOfSoldiers)
% where all positions are 0 indexed
josephus(NumberOfSoldiers, StartingPosition, N, J, A).

