:- module(minimax, [minimax/5, nbTokens/1, maximizing/1, minimizing/1]).

:- use_module(util).

%%%%%%%%%%%%%%
%%% Public %%%
%%%%%%%%%%%%%%

:-dynamic nbTokens/1.
:-dynamic maximizing/1.
:-dynamic minimizing/1.

minimax(_,[['-','-','-','-','-','-'],
			['-','-','-','-','-','-'],
			['-','-','-','-','-','-'],
			['-','-','-','-','-','-'],
			['-','-','-','-','-','-'],
			['-','-','-','-','-','-'],
			['-','-','-','-','-','-']],_,Column,_)
	:- random_between(0,6,Column), !.

minimax(Depth,Board,Mark,Column,Score) :-
 maxDepth(D), Depth<D,
 Depth2 is Depth+1,
 possible_moves(Board,List), !,		%%% get the list of possible moves
	best(Depth2,Board,Mark,List,Column,Score), !.	
					%%% recursively determine the best available move

% If there are no more available moves, then the minimax value is 
% the score of the given board position 
 
minimax(Depth,Board,_,_,Score) :- score(Board, Depth, Score).

%%%%%%%%%%%%%%%
%%% Private %%%
%%%%%%%%%%%%%%%

maxDepth(5). 			%%% the max depth for Minimax algorithm

%.......................................
% best
%.......................................
% determines the best move in a given list of moves by 
% recursively calling minimax
% 
% if there is only one move left in the list... 

best(Depth,Board,Mark,[Column1],Column1,Score) 
	:-	move(Board,Column1,Mark,Board2),	%%% apply that move to the board,
			inverse_mark(Mark,Mark2), !,
			%%% then recursively search for the score of that move.
				minimax(Depth,Board2,Mark2,_,Score), !, 
				output_value(Depth,Column1,Score), !.

% if there is more than one move in the list... 

best(Depth,Board,Mark,[Column1|Other_Moves],Column,Score) 
	:-	move(Board,Column1,Mark,Board2),	%%% apply the first move (in the list)
			inverse_mark(Mark,Mark2), !,
				minimax(Depth,Board2,Mark2,_,Score1),	
				%%% recursively search for the score value of that move
				%%% and determine the best move of the remaining moves
				best(Depth,Board,Mark,Other_Moves,Column2,Score2),
				output_value(Depth,Column1,Score1),
				%%% choose the better of the two moves based on their score values
				better(Depth,Mark,Column1,Score1,Column2,Score2,Column,Score).

%.......................................
% better
%.......................................
% returns the better of two moves based on their score values.
%
% if both moves have the same score value, then one is chosen at random. 
%
better(_,Mark,Column1,Score1,_,Score2,Column1,Score1) 
	:-	maximizing(Mark),				%%% if the player is maximizing
		Score1 > Score2, !.		%%% then greater is better.

better(_,Mark,Column1,Score1,_,Score2,Column1,Score1) 
	:-	minimizing(Mark),				%%% if the player is minimizing,
		Score1 < Score2, !.		%%% then lesser is better.
	
better(_,Mark,Column1,Score1,Column2,Score2,Column,Score) 
	:-	Score1 == Score2,		%%% if moves have equal score,
		random_between(1,10,R),		%%% then pick one of them at random
		better2(_,R,Mark,Column1,Score1,Column2,Score2,Column,Score), !.

better(_,_,_,_,Column2,Score2,Column2,Score2). 
									%%% otherwise, second move is better
	
%.......................................
% better2
%.......................................
% randomly selects among two columns of the same score value
%
better2(_,R,_,Column1,Score1,_,_,Column1,Score1) :- R < 6, !.
better2(_,_,_,_,_,Column2,Score2,Column2,Score2).

%.......................................
% score
%.......................................
% It computes the value of a given board position
% 
score(Board, Depth, S) :- maximizing(M), wins(Board, M), nbTokens(Nb), S is 100 - div(Nb+Depth+1, 2), !. % We add 1 because when X plays at first the number of X tokens is odd
score(Board, Depth, S) :- minimizing(M), wins(Board, M), nbTokens(Nb), S is div(Nb+Depth+1, 2) - 100 , !. 
score(Board, Depth, S) :- maximizing(M), three_in_a_row(Board, M, H), nbTokens(Nb), S is 28 - (div(Nb+Depth+1, 2) + H), !.
score(Board, Depth, S) :- minimizing(M), three_in_a_row(Board, M, H), nbTokens(Nb), S is div(Nb+Depth+1, 2) + H - 28 , !.
score(_,_,0).

%three_in_a_row(B, M, H) is satisfied if 3 marks M are connected in board B
%check if there's a column in B with 3 connected marks M
three_in_a_row(B, M, H):- append(_, [C|_], B), % check if there's a column...
	           append(_,['-',M,M,M|_],C), % ...which has 3 connected marks M
			   H is 1.
%check if there's a row in B with 3 connected marks M
three_in_a_row(B, M, H):- append(_,[C1,C2,C3,C4|_],B), % check if 3 connected columns exists in board...
		((
			append(I1,['-'|_],C1), %...such that all of them contain a mark M...
			append(I2,[M|_],C2),
			append(I3,[M|_],C3),
			append(I4,[M|_],C4),
			length(I1,L), length(I2,L), length(I3,L), length(I4,L), %...and every piece is in the same height
			H is 6 - L
		);(
			append(I1,[M|_],C1), %...such that all of them contain a mark M...
			append(I2,[M|_],C2),
			append(I3,[M|_],C3),
			append(I4,['-'|_],C4),
			length(I1,L), length(I2,L), length(I3,L), length(I4,L), %...and every piece is in the same height
			H is 6 - L
		)).
%check if there's a diagonal (type \) in B with 3 connected marks M
three_in_a_row(B, M, H):- append(_,[C1,C2,C3,C4|_],B), % check if 3 connected columns exists in board...
		((   
			append(I1,['-'|_],C1), %...such that all of them contain a mark M...
			append(I2,[M|_],C2),
			append(I3,[M|_],C3),
			append(I4,[M|_],C4),
			length(I1,L1), length(I2,L2), length(I3,L3), length(I4,L4),
			L2 is L1+1, L3 is L2+1, L4 is L3+1, %...and every piece is within the same diagonal \
			H is 6 - L1
		);(
			append(I1,[M|_],C1), %...such that all of them contain a mark M...
			append(I2,[M|_],C2),
			append(I3,[M|_],C3),
			append(I4,['-'|_],C4),
			length(I1,L1), length(I2,L2), length(I3,L3), length(I4,L4),
			L2 is L1+1, L3 is L2+1, L4 is L3+1, %...and every piece is within the same diagonal \
			H is 6 - L4
		)).
%check if there's a diagonal (type /) in B with 3 connected marks M
three_in_a_row(B, M, H):- append(_,[C1,C2,C3,C4|_],B), % check if 3 connected columns exists in board...
		((   
			append(I1,['-'|_],C1), %...such that all of them contain a mark M...
			append(I2,[M|_],C2),
			append(I3,[M|_],C3),
			append(I4,[M|_],C4),
			length(I1,L1), length(I2,L2), length(I3,L3), length(I4,L4),
			L2 is L1-1, L3 is L2-1, L4 is L3-1, %...and every piece is within the same diagonal /
			H is 6 - L1
		);(
			append(I1,[M|_],C1), %...such that all of them contain a mark M...
			append(I2,[M|_],C2),
			append(I3,[M|_],C3),
			append(I4,['-'|_],C4),
			length(I1,L1), length(I2,L2), length(I3,L3), length(I4,L4),
			L2 is L1-1, L3 is L2-1, L4 is L3-1, %...and every piece is within the same diagonal /
			H is 6 - L4
		)).
