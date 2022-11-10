% import possible moves

:- module(greedy, [greedy/3]).

:- use_module(util).

%%%%%%%%%%%%%%
%%% Public %%%
%%%%%%%%%%%%%%

% greedy(+Board, +Player, -Move)
% The greedy algorithm
% finds the best move C to play for the player Mark
% on the board B
greedy(B,Mark,C):- 
 findall((Col,Bs), (col(Col), move(B,Col,Mark,Bs),wins(Bs,Mark)),[(C,_)|_]),!.

greedy(B,Mark,C):- 
	inverse_mark(Mark,OppositeMark),
	findall((Col,Bs), (col(Col), move(B,Col,OppositeMark,Bs),wins(Bs,OppositeMark)),[(C,_)|_]),!.
   
greedy(B,Mark,C):-
 possible_moves(B,List),
 inverse_mark(Mark,OppositeMark),
 best_move(B,OppositeMark,List,_,C).

%%%%%%%%%%%%%%%
%%% Private %%%
%%%%%%%%%%%%%%%

% returns the mark Res in the column Col and in the row Row from the board Board
piece(Board,Col,Row,Res):-
	append(I,[C|_],Board),
	length(I,Col), 
	append(L,[Res|_],C),
	length(L,Row0),
	Row is 5-Row0,!.

% traverses the board B, starting from column C and row R
% incremeting c by IncC and R by incR
% and returns the number of connected pieces Res on this line.
traverse(B,C,R,IncC,IncR,Res):-
	NewC is C + IncC,
	NewR is R + IncR,
	NewC > -1, NewR > -1,
	NewC < 7, NewR < 6,
	piece(B,C,R,C1),
	piece(B,NewC,NewR,C2),
	C1 == C2,
	traverse(B,NewC,NewR,IncC,IncR,Res1),
	Res is Res1 + 1,!.
traverse(_,_,_,_,_,Res):-
 Res is 1.

% starting from column X and row Y on the board B, exploring all the directions
% it returns the maximum of connected pieces on the same line
get_max(B,X,Y,RES):-
 traverse(B,X,Y,1,0,R11),traverse(B,X,Y,-1,0,R12),R1 is R11 + R12 - 1,
 traverse(B,X,Y,0,1,R21),traverse(B,X,Y,0,-1,R22),R2 is R21 + R22 - 1,
 traverse(B,X,Y,1,1,R31),traverse(B,X,Y,-1,-1,R32),R3 is R31 + R32 - 1,
 traverse(B,X,Y,1,-1,R41),traverse(B,X,Y,-1,1,R42),R4 is R41 + R42 - 1,
 RES1 is max(R1,R2),RES2 is max(R3,R4),RES is max(RES1,RES2).

% returns the top piece Mark and its height Y in a column Col
top(Board,Col,Mark,Y):-
 append(I,[C|_],Board),
 length(I,Col),
 append(P,['-',X|_],C),
 Mark\='-',
 length(P,Y0),
 Y is 5-(1+Y0),!.

top(Board,Col,Mark,Y):-
 append(I,[[Mark|_]|_],Board),
 length(I,Col),
 (   Mark=='-' ->  
		 Y is 0;
		 Y is 5
 ).

% returns the col Col with the highest score Res
% if Res1 and Res2 are equal, returns a random Col between Col1 and Col2
col_max(Res1,Res2,COL1,_,RES,COL):-
 Res1 > Res2,
 RES is Res1,
 COL is COL1,!.

col_max(Res1,Res2,_,COL2,RES,COL):-
	Res1 < Res2,
 	RES is Res2,
 	COL is COL2,!.

col_max(Res1,Res2,COL1,COL2,RES,COL):-
	random_between(1,2,R),
	( R == 1 ->
		RES is Res2,
 		COL is COL2,!;
	  R == 2 ->
	  	RES is Res1,
	  	COL is COL1,!
	).

% searches the best move for the player with the mark Mark
% from the moves [Col|OtherMoves]
% and returns the best move in the column ColRes with its score Res
best_move(B,Mark,[Col|OtherMoves],Res,ColRes):-
	move(B,Col,Mark,B2),
	top(B2,Col,_,Y),
	get_max(B2,Col,Y,Res1),
	best_move(B,Mark,OtherMoves,Res2,ColRes2),
	col_max(Res1,Res2,Col,ColRes2,Res,ColRes).
   
best_move(_,_,[],0,_).
