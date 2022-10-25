% Copyright 2016 Ramon Viñas, Marc Roig
%
% Licensed under the Apache License, Version 2.0 (the "License");
% you may not use this file except in compliance with the License.
% You may obtain a copy of the License at
%
%     http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS,
% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
% See the License for the specific language governing permissions and
% limitations under the License.

%%%%%%%%%%%%%%%%%%
%%%%% BOARD %%%%%%
%%%%%%%%%%%%%%%%%%
%Initialize empty board (matrix of dimensions [columns=7, rows=6]. This board representation will make gameplay easier than if we used [rows, columns])
initial(board([['-','-','-','-','-','-'],
	       ['-','-','-','-','-','-'],
	       ['-','-','-','-','-','-'],
	       ['-','-','-','-','-','-'],
	       ['-','-','-','-','-','-'],
	       ['-','-','-','-','-','-'],
	       ['-','-','-','-','-','-']])).

%%%%%%%%%%%%%%%%%%
%%% SHOW BOARD %%%
%%%%%%%%%%%%%%%%%%
%show(X) shows board X
show(board(X)):- write('  A B C D E F G'), nl,
		 iShow(X,6).

%show(X,N) shows lines [N .. 1] of board X
iShow(_,0).
iShow(X,N):- showLine(X,N,X2),
	     Ns is N-1,
	     iShow(X2,Ns).

%showLine(X,N,X2) writes N and shows first line of board X (first element of every column). X2 is X without the shown line.
showLine(X,N,X2):- write(N), write(' '),
		   iShowLine(X,X2), nl.

%iShowLine(X,X2) writes first element of every column. X2 is X without the shown line.
iShowLine([],_).
iShowLine([[X|X2]|XS],[X2|XS2]):- write(X), write(' '),
			          iShowLine(XS,XS2).

%%%%%%%%%%%%%%%%%%
%%%% GAMEPLAY %%%%
%%%%%%%%%%%%%%%%%%
% Initializes board and starts the game
connect4:- initial(X),
	   show(X),
	   nextMove('X',X), !.

%nextMove(J,X) J is the player that needs to move ('O' or 'X') and X is the board. Checks if the game has finished. If it hasn't finished, performs next move.
nextMove('X',X):- wins('O',X),
		  write('Machine wins!').
nextMove('O',X):- wins('X',X),
		  write('You win!').
nextMove(_,X):- full(X),
		write('Draw').
nextMove('X',X):- repeat, %repeats in case a column is full
		  readColumn(C),
		  play('X',C,X,X2), !,
		  show(X2),
		  nextMove('O',X2). 
nextMove('O',X):- machine('O','X',X,X2),
		  show(X2),
		  nextMove('X',X2).

%play(X,P,T,T2) is satisfied if T2 is the board T after player X moves in column P
play(X,P,board(T),board(T2)):- append(I,[C|F],T),
			       length(I,P), 
		               playColumn(X,C,C2),
			       append(I,[C2|F],T2).

%playColumn(X,C,C2) is satisfied if column C2 is column C after player X plays there
playColumn(X,['-'],[X]):- !. % last spot in column
playColumn(X,['-',A|AS],[X,A|AS]):- A \== ('-'), !. % play above someone's piece
playColumn(X,['-'|AS],['-'|AS2]):- playColumn(X,AS,AS2). % descend column

%wins(X,T) is satisfied if player X has won in board T
%check if there's a column in T with 4 connected pieces of player X
wins(X,board(T)):- append(_, [C|_], T), % check if there's a column...
	           append(_,[X,X,X,X|_],C). % ...which has 4 connected pieces of player X
%check if there's a row in T with 4 connected pieces of player X
wins(X,board(T)):- append(_,[C1,C2,C3,C4|_],T), % check if 4 connected columns exists in board...
		   append(I1,[X|_],C1), %...such that all of them contain a piece of player X...
		   append(I2,[X|_],C2),
		   append(I3,[X|_],C3),
		   append(I4,[X|_],C4),
		   length(I1,M), length(I2,M), length(I3,M), length(I4,M). %...and every piece is in the same height
%check if there's a diagonal (type \) in T with 4 connected pieces of player X
wins(X,board(T)):- append(_,[C1,C2,C3,C4|_],T), % check if 4 connected columns exists in board...
		   append(I1,[X|_],C1), %...such that all of them contain a piece of player X...
		   append(I2,[X|_],C2),
		   append(I3,[X|_],C3),
		   append(I4,[X|_],C4),
		   length(I1,M1), length(I2,M2), length(I3,M3), length(I4,M4),
		   M2 is M1+1, M3 is M2+1, M4 is M3+1. %...and every piece is within the same diagonal \
%check if there's a diagonal (type /) in T with 4 connected pieces of player X
wins(X,board(T)):- append(_,[C1,C2,C3,C4|_],T), % check if 4 connected columns exists in board...
		   append(I1,[X|_],C1), %...such that all of them contain a piece of player X...
		   append(I2,[X|_],C2),
		   append(I3,[X|_],C3),
		   append(I4,[X|_],C4),
		   length(I1,M1), length(I2,M2), length(I3,M3), length(I4,M4),
		   M2 is M1-1, M3 is M2-1, M4 is M3-1. %...and every piece is within the same diagonal /
						
%full(T) is satisfied if there isn't any free spot ('-')
full(board(T)):- \+ (append(_,[C|_],T),
		 append(_,['-'|_],C)).

%%%%%%%%%%%%%%%%%%
%%% READ MOVES %%%
%%%%%%%%%%%%%%%%%%
%reads a column
readColumn(C):- nl, write('Column: '),
		repeat,
		get_char(L),
		associateColumn(L,C),
		col(C), !.

%associateColumn(L,C) column C is the column associated with char L
associateColumn(L,C):- atom_codes(L,[La|_]),
		       C is La - 65.

%associateChar(L, C) char L is the char associated with column C
associateChar(L, C):- Ln is 65+C,
		      atom_codes(L,[Ln]).

%valid columns
col(0).
col(1).
col(2).
col(3).
col(4).
col(5).
col(6).

%%%%%%%%%%%%%%%%%%
%%%%% MACHINE %%%%
%%%%%%%%%%%%%%%%%%
%machine(R,O,T,T2) Let R be the machine piece, O the opponent's piece and T the board game. Then T2 is board T after the machine movement
% win if possible
machine(R,_,T,T2):- iMachine(R,T,C,T2),
		    nl, write('machine: '),
		    associateChar(L,C),
		    write(L),
		    nl,!.
% otherwise, if machine can't win within a move, play a move that doesn't allow opponent O to win and that would allow us to obtain a connected 4
machine(R,O,T,T2):- findall((Col,TA), (col(Col), play(R,Col,T,TA),\+ iMachine(O,TA,_,_), goodMove(R,Col,T)), [(C,T2)|_]),
		    nl, write('machine: '),
		    associateChar(L,C),
		    write(L),
		    nl,!.
% otherwise play a move that doesn't allow opponent O to win
machine(R,O,T,T2):- findall((Col,TA), (col(Col), play(R,Col,T,TA),\+ iMachine(O,TA,_,_)), [(C,T2)|_]),
		    nl, write('machine: '),
		    associateChar(L,C),
		    write(L), nl,
		    write('-'),!.
% otherwise play a move intercepting one of the future winning options of opponent O
machine(R,O,T,T2):- iMachine(O,T,C,_),
		    play(R,C,T,T2),
		    nl, write('machine: '),
		    associateChar(L,C),
		    write(L), nl.
% otherwise play wherever
machine(R,_,T,T2):- col(C),
		    play(R,C,T,T2),
		    nl, write('machine: '),
		    associateChar(L,C),
		    write(L), nl.
				  
%iMachine(R,T,C,T2) is satisfied if player R can play in column C of board T and obtain a winning board T2
iMachine(R,T,C,T2):- findall((Col,TA), (col(Col), play(R,Col,T,TA),wins(R,TA)),[(C,T2)|_]).

%We consider that a good move is one allowing us to win in a column. Further improvements: rows and diagonals.
goodMove(R,Col,board(T)):- append(I,[C|_],T),
			   length(I,Col),
			   maxConnected(R,C,MaxConn),
			   MaxConn >= 4.						

% maxConnected(R,C,MaxConn) MaxConn is the maximum number of connected pieces that player R has/could have in column C
maxConnected(_,[],0).
maxConnected(R,[X|_],0):- X\=R.
maxConnected(R,['-'|X],N):- maxConnected(R,X,Ns),
			    N is Ns+1.
maxConnected(R,[R|X],N):- maxConnected(R,X,Ns),
			  N is Ns+1.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	
% Idea: we start by implementing functions that verify the columns, the rows or the diagonals in which we can potentially win (with 4 connected pieces), 
% which means they are currently occupied by our piece or they are simply empty
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	

% column
open(board(T),X):- append(_, [C|_], T), % check if there's a column in the board
	           append(_,[P1,P2,P3,P4|_],C), % where we 4 connected pieces
                   (P1 == '-' | P1 == X), (P2 == '-' | P2 == X), % of X or empty space
                   (P3 == '-' | P3 == X), (P4 == '-' | P4 == X).
		   
% row
open(board(T),X):- append(_,[C1,C2,C3,C4|_],T), % check if 4 connected columns in the board
		   append(I1,[P1|_],C1), % that all of them contain a piece 
		   append(I2,[P2|_],C2),
		   append(I3,[P3|_],C3),
		   append(I4,[P4|_],C4),
		   length(I1,M), length(I2,M), length(I3,M), length(I4,M), % and every single piece is in the same height (by using the same M)
                   (P1 == '-' | P1 == X), (P2 == '-' | P2 == X), % the grid is empty or occupied by X
                   (P3 == '-' | P3 == X), (P4 == '-' | P4 == X).
		   
		   
/*
 diagonal:
 acutally there are two cases:
	1. x
	    x
	     x
	      x
	      
	2.	x
	       x
	      x
	     x
*/

% diagonal case1:
open(board(T),X):- append(_,[C1,C2,C3,C4|_],T), % check if 4 connected columns in the board
		   append(I1,[P1|_],C1), % that all of them contain a piece 
		   append(I2,[P2|_],C2),
		   append(I3,[P3|_],C3),
		   append(I4,[P4|_],C4),
		   length(I1,M1), length(I2,M2), length(I3,M3), length(I4,M4),
		   M2 is M1+1, M3 is M2+1, M4 is M3+1, %...and every piece is within the same diagonal of case 1
                   (P1 == '-' | P1 == X), (P2 == '-' | P2 == X), % the grid is empty or occupied by X
                   (P3 == '-' | P3 == X), (P4 == '-' | P4 == X).
		   
% diagonal case2:
open(board(T),X):- append(_,[C1,C2,C3,C4|_],T), % check if 4 connected columns in the board
		   append(I1,[P1|_],C1), % that all of them contain a piece 
		   append(I2,[P2|_],C2),
		   append(I3,[P3|_],C3),
		   append(I4,[P4|_],C4),
		   length(I1,M1), length(I2,M2), length(I3,M3), length(I4,M4),
		   M2 is M1-1, M3 is M2-1, M4 is M3-1, %...and every piece is within the same diagonal of case 2
                   (P1 == '-' | P1 == X), (P2 == '-' | P2 == X), % the grid is empty or occupied by X
                   (P3 == '-' | P3 == X), (P4 == '-' | P4 == X).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	     
% In the next part, we are supposed to count elements that are necessary to build our heuristic function
% When it comes to h, personally I think it could be h=2*x3+x2, where x3 means the number of 3 connected pieces in the board 
% and x2 the number of 2 connected pieces, but this version is naive and the coefficent for x3 is actually arbitary
% And if we consider the case like oxxo and _xx_, they should have different weight in fact
% !!!TO BE DISCUSSED
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	

% threes(X,T) is true if player X has any three connect piece in the board 

% column
threes(board(T),X):- append(_, [C|_], T), % if a column in the board
	           append(_,[P1,P2,P3|_],C), %  which has 3 connected pieces
                   P1 == X, P2 == X, P3 == X. % of player X
		   
% row
threes(board(T),X):- append(_,[C1,C2,C3|_],T), % check if 3 connected columns 
		   append(I1,[P1|_],C1), % that all of them contain a piece 
		   append(I2,[P2|_],C2),
                   append(I3,[P3|_],C3),
                   length(I1,M), length(I2,M), length(I3,M), % at the same height
                   P1 == X, P2 == X, P3 == X. % all of them are X

% check if there's a diagonal in T with 3 connected pieces of X
% diagonal case1
threes(board(T),X):- append(_,[C1,C2,C3|_],T), % check if 3 connected columns 
		   append(I1,[P1|_],C1), % that all of them contain a piece 
		   append(I2,[P2|_],C2),
                   append(I3,[P3|_],C3),
                   length(I1,M1), length(I2,M2), length(I3,M3),
		   M2 is M1+1, M3 is M2+1, % in the diagonal of case 1
                   P1 == X, P2 == X, P3 == X. % all of them are X

% diagonal case2
threes(board(T),X):- append(_,[C1,C2,C3|_],T), % check if 3 connected columns 
		   append(I1,[P1|_],C1), % that all of them contain a piece 
		   append(I2,[P2|_],C2),
                   append(I3,[P3|_],C3),
                   length(I1,M1), length(I2,M2), length(I3,M3),
		   M2 is M1-1, M3 is M2-1, % in the diagonal of case 2
                   P1 == X, P2 == X, P3 == X. % all of them are X
