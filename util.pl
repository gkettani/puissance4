:- module(util, [move/4, wins/2, inverse_mark/2, possible_moves/2]).

%%%%%%%%%%%%%%
%%% Public %%%
%%%%%%%%%%%%%%
inverse_mark('X','O'). 	%%% determines the opposite of the given mark
inverse_mark('O','X'). 

%.......................................
% move
%.......................................
%move(T,P,X,T2) is satisfied if T2 is the board T after player with the mark X moves in column P
% 
move(T,P,X,T2):- append(I,[C|F],T),
			       length(I,P), 
    			   set_item(X,C,C2),
			       append(I,[C2|F],T2).

%.......................................
% possible_moves
%.......................................
% It retrieves a list of possible moves (empty columns) on a board.
% 
possible_moves(Board,List) 
	:-	not(wins(Board,'X')),	%%% if either player already won, 
							%%% then there are no available moves
		not(wins(Board,'O')),
		top_row(Board, 0, List), !. % get the list of the column where a player can place his mark 

%.......................................
% wins
%.......................................
% Players win by having their mark M in one of the following configurations:
% 
%wins(B, M) is satisfied if the player with mark M has won in board B
%check if there's a column in B with 4 connected marks M
wins(B, M):- append(_, [C|_], B), % check if there's a column...
	           append(_,[M,M,M,M|_],C). % ...which has 4 connected marks M
%check if there's a row in B with 4 connected marks M
wins(B, M):- append(_,[C1,C2,C3,C4|_],B), % check if 4 connected columns exists in board...
		   append(I1,[M|_],C1), %...such that all of them contain a mark M...
		   append(I2,[M|_],C2),
		   append(I3,[M|_],C3),
		   append(I4,[M|_],C4),
		   length(I1,L), length(I2,L), length(I3,L), length(I4,L). %...and every mark is in the same height
%check if there's a diagonal (type \) in B with 4 connected marks M
wins(B, M):- append(_,[C1,C2,C3,C4|_],B), % check if 4 connected columns exists in board...
		   append(I1,[M|_],C1), %...such that all of them contain a mark M...
		   append(I2,[M|_],C2),
		   append(I3,[M|_],C3),
		   append(I4,[M|_],C4),
		   length(I1,L1), length(I2,L2), length(I3,L3), length(I4,L4),
		   L2 is L1+1, L3 is L2+1, L4 is L3+1. %...and every mark is within the same diagonal \
%check if there's a diagonal (type /) in B with 4 connected marks M
wins(B, M):- append(_,[C1,C2,C3,C4|_],B), % check if 4 connected columns exists in board...
		   append(I1,[M|_],C1), %...such that all of them contain a mark M...
		   append(I2,[M|_],C2),
		   append(I3,[M|_],C3),
		   append(I4,[M|_],C4),
		   length(I1,L1), length(I2,L2), length(I3,L3), length(I4,L4),
		   L2 is L1-1, L3 is L2-1, L4 is L3-1. %...and every mark is within the same diagonal /


%%%%%%%%%%%%%%%
%%% Private %%%
%%%%%%%%%%%%%%%

%set_item(X,C,C2) is satisfied if column C2 is column C after player with the mark X plays there
set_item(X,['-'],[X]):- !. % last spot in column
set_item(X,['-',A|AS],[X,A|AS]):- A \== ('-'), !. % play above someone's piece
set_item(X,['-'|AS],['-'|AS2]):- set_item(X,AS,AS2). % descend column

%top_row(B, N, L) is satisfied if L is the list of the columns in board B where a player can place his mark
top_row([], _, []).
top_row([[E|_]|R], N, [N|L]) :- E == '-', Ns is N+1, top_row(R, Ns, L).
top_row([_|R], N, L) :- Ns is N+1, top_row(R, Ns, L).