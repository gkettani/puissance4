:-dynamic board/1.
:-dynamic player/2.
:-dynamic nbTokens/1.
:-dynamic alpha/1.
:-dynamic beta/1.
:-dynamic noeudsParcourus/1.

%%%%%%%%%%%%%%%%%%
%%%%% PARAMS %%%%%
%%%%%%%%%%%%%%%%%%
next_player(1,2).		%%% determines the next player after the given player
next_player(2,1). 

inverse_mark('X','O'). 	%%% determines the opposite of the given mark
inverse_mark('O','X'). 

player_mark(1,'X').		%%% the mark for the given player
player_mark(2,'O'). 
 
opponent_mark(1,'O'). 	%%% the inverse mark of the given player
opponent_mark(2,'X'). 

maximizing('X').	
% The player playing x is always trying to maximize board position score
minimizing('O').	
% The player playing o is always trying to minimize board position score

maxDepth(5).

%%%%%%%%%%%%%%%%%%
%%% SHOW BOARD %%%
%%%%%%%%%%%%%%%%%%
%show(X) shows board X
show(X):- write('  A B C D E F G'), nl,
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Main program
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

jouer :- welcome,				%%% Display welcome message, initialize game 
			play(1),			%%% Play the game starting with player 1 
				goodbye.        %%% Display end of game message
jouer :- goodbye.

welcome :- initialize, nl, nl, write('Début du jeu Puissance 4'),
			read_players, output_players.

initialize :-	%%% random seed may use time to initialize random number generator
				retractall(player(_,_)), retractall(board(_)), retractall(nbTokens(_)),	
                retractall(alpha(_)), retractall(beta(_)),
				asserta(board([['-','-','-','-','-','-'],
                               ['-','-','-','-','-','-'],
                               ['-','-','-','-','-','-'],
                               ['-','-','-','-','-','-'],
                               ['-','-','-','-','-','-'],
                               ['-','-','-','-','-','-'],
                               ['-','-','-','-','-','-']])),
				asserta(nbTokens(0)), asserta(alpha(-30)), asserta(beta(30)), asserta(noeudsParcourus(0)). 
				%%% create an empty board

goodbye :- 	board(Board), nl, nl, write('Game over: '), 
				output_winner(Board),
					retractall(player(_,_)), retractall(board(_)),	
						read_play_again(V), !, 
							(V == 'Y' ; V == 'y'), !, jouer.

read_play_again(V) :- nl, nl, write('Play again (Y/N)? '), get_char(V),
						(V == 'y' ; V == 'Y' ; V == 'n' ; V == 'N'), !.
read_play_again(V) :- nl, nl, write('Please enter Y or N.'),
						read_play_again(V).

read_players 
	:- nl, nl, write('Nombre de joueurs humains? '), read(N), set_players(N).

set_players(0) :- asserta(player(1,computer1)), asserta(player(2,computer2)), !.

set_players(1) :- nl, write('Voulez vous jouer X ou O ? '),
					get_char(M), human_playing(M), !.

set_players(2) :- asserta(player(1,human)), asserta(player(2,human)), !.
 
set_players(_) :- nl, write('Veuillez taper 0, 1, ou 2.'), read_players.

human_playing(M) :-
	(M == 'x' ; M == 'X'), 
		asserta(player(1,human)), asserta(player(2,computer1)), !.

human_playing(M) :-
	(M == 'o' ; M == 'O'),
		asserta(player(1,computer1)), asserta(player(2,human)), !.

human_playing(_) :- nl, write('Veuillez taper X ou O.'), set_players(1).
 
play(Player1) 
	:- 	board(Board), show(Board), !,
			not(game_over(Player1, Board)),
				make_move(Player1, Board), !,
					next_player(Player1,Player2), output_noeudsParcourus, play(Player2).

%.......................................
% wins
%.......................................
% Players win by having their mark X in one of the following configurations:
% 
%wins(T, X) is satisfied if the player with Mark X has won in board T
%check if there's a column in T with 4 connected pieces of player X
wins(T, X):- append(_, [C|_], T), % check if there's a column...
	           append(_,[X,X,X,X|_],C). % ...which has 4 connected pieces of player X
%check if there's a row in T with 4 connected pieces of player X
wins(T, X):- append(_,[C1,C2,C3,C4|_],T), % check if 4 connected columns exists in board...
		   append(I1,[X|_],C1), %...such that all of them contain a piece of player X...
		   append(I2,[X|_],C2),
		   append(I3,[X|_],C3),
		   append(I4,[X|_],C4),
		   length(I1,M), length(I2,M), length(I3,M), length(I4,M). %...and every piece is in the same height
%check if there's a diagonal (type \) in T with 4 connected pieces of player X
wins(T, X):- append(_,[C1,C2,C3,C4|_],T), % check if 4 connected columns exists in board...
		   append(I1,[X|_],C1), %...such that all of them contain a piece of player X...
		   append(I2,[X|_],C2),
		   append(I3,[X|_],C3),
		   append(I4,[X|_],C4),
		   length(I1,M1), length(I2,M2), length(I3,M3), length(I4,M4),
		   M2 is M1+1, M3 is M2+1, M4 is M3+1. %...and every piece is within the same diagonal \
%check if there's a diagonal (type /) in T with 4 connected pieces of player X
wins(T, X):- append(_,[C1,C2,C3,C4|_],T), % check if 4 connected columns exists in board...
		   append(I1,[X|_],C1), %...such that all of them contain a piece of player X...
		   append(I2,[X|_],C2),
		   append(I3,[X|_],C3),
		   append(I4,[X|_],C4),
		   length(I1,M1), length(I2,M2), length(I3,M3), length(I4,M4),
		   M2 is M1-1, M3 is M2-1, M4 is M3-1. %...and every piece is within the same diagonal /

%.......................................
% move
%.......................................
%move(T,P,X,T2) is satisfied if T2 is the board T after player with the mark X moves in column P
% 
move(T,P,X,T2):- append(I,[C|F],T),
			       length(I,P), 
    			   set_item(X,C,C2),
			       append(I,[C2|F],T2).

%set_item(X,C,C2) is satisfied if column C2 is column C after player with the mark X plays there
set_item(X,['-'],[X]):- !. % last spot in column
set_item(X,['-',A|AS],[X,A|AS]):- A \== ('-'), !. % play above someone's piece
set_item(X,['-'|AS],['-'|AS2]):- set_item(X,AS,AS2). % descend column

%.......................................
% game_over
%.......................................
% Game is over if opponent wins or if none of the columns are empty
%
game_over(Player,Board) :- opponent_mark(Player, Mark), wins(Board, Mark), !.
game_over(_,Board) :- isFull(Board). 

%isFull(T) is satisfied if there isn't any free spot ('-')
isFull(T):- \+ (append(_,[C|_],T), append(_,['-'|_],C)).

%.......................................
% readColumn
%.......................................
%reads a column
readColumn(C):- write(' , veuillez choisir une colonne : '),
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

%.......................................
% make_move
%.......................................
% It requests next move from human or computer,
% then it applies that move to the given board
% 
make_move(Player, Board1) 
	:- 	player(Player, Type_Joueur), 
			make_move2(Type_Joueur, Player, Board1, Board2),
				nbTokens(Nb), NewNb is Nb + 1, retractall(nbTokens(_)), asserta(nbTokens(NewNb)),
					retract(board(_)), asserta(board(Board2)).

make_move2(human, Player, Board1, Board2) 
	:-	nl, nl, write('C\'est au tour du joueur '), write(Player), readColumn(Column), !
    			, player_mark(Player, Mark), move(Board1,Column,Mark,Board2), !.
				
make_move2(human, Player, Board1, Board2) 
	:-	nl, nl, write('Veuillez choisir une lettre entre A et G.'),
			make_move2(human,Player,Board1,Board2).

% A move computed thanks to minimax is made of values for the 3 variables 
% Mark, Column and Score

make_move2(computer1, Player, Board1, Board2) 
	:-	nl, nl, write('Computer1 est entrain de réflechir ...'), nl,
			player_mark(Player, Mark),
				minimax(0, Board1, Mark, Column, _),
					move(Board1,Column,Mark,Board2), !,
		nl, nl, write('Computer1 place '), write(Mark), write(' dans la colonne '), associateChar(L,Column),
			write(L), write('.'), nl.

make_move2(computer2, Player, Board1, Board2) 
	:-	nl, nl, write('Computer2 est entrain de réflechir ...'), nl,
			player_mark(Player, Mark),
				repeat, random_between(0,6,Column),
					move(Board1,Column,Mark,Board2), !,
		nl, write('Computer2 place '), write(Mark), write(' dans la colonne '), associateChar(L,Column),
			write(L), write('.'), nl.

%.......................................
% possible_moves
%.......................................
% It retrieves a list of possible moves (empty columns) on a board.
% 
possible_moves(Board,List) 
	:-	not(wins(Board,'X')),	%%% if either player already won, 
							%%% then there are no available moves
		not(wins(Board,'O')),
		top_line(Board, 0, List), !. % get the list of the column where a player can place his mark 
		
top_line([], _, []).
top_line([[E|_]|R], N, [N|L]) :- E == '-', Ns is N+1, top_line(R, Ns, L).
top_line([_|R], N, L) :- Ns is N+1, top_line(R, Ns, L).

		
%.......................................
% score
%.......................................
% It computes the value of a given board position
% 
score(Board, Depth, S) :- wins(Board,'X'), nbTokens(Nb), S is 22 - (div(Nb+Depth, 2) + 1), !. % We add 1 because when X plays at first the number of X tokens is odd
score(Board, Depth, S) :- wins(Board,'O'), nbTokens(Nb), S is div(Nb+Depth, 2) - 22 , !. 
score(Board, Depth, S) :- three_in_a_row(Board,'X', H), nbTokens(Nb), S is 22 - (div(Nb+Depth, 2) + 1 + H), !.
score(Board, Depth, S) :- three_in_a_row(Board,'O', H), nbTokens(Nb), S is div(Nb+Depth, 2) + H - 22 , !.
score(_,_,0).

%three_in_a_row(T, X, H) is satisfied if the player with Mark X has 3 connected X in a row in board T
%check if there's a column in T with 3 connected pieces of player X
three_in_a_row(T, X, H):- append(_, [C|_], T), % check if there's a column...
	           append(_,['-',X,X,X|_],C), % ...which has 3 connected pieces of player X
			   H is 1.
%check if there's a row in T with 3 connected pieces of player X
three_in_a_row(T, X, H):- append(_,[C1,C2,C3,C4|_],T), % check if 3 connected columns exists in board...
		((
			append(I1,['-'|_],C1), %...such that all of them contain a piece of player X...
			append(I2,[X|_],C2),
			append(I3,[X|_],C3),
			append(I4,[X|_],C4),
			length(I1,M), length(I2,M), length(I3,M), length(I4,M), %...and every piece is in the same height
			H is 6 - M
		);(
			append(I1,[X|_],C1), %...such that all of them contain a piece of player X...
			append(I2,[X|_],C2),
			append(I3,[X|_],C3),
			append(I4,['-'|_],C4),
			length(I1,M), length(I2,M), length(I3,M), length(I4,M), %...and every piece is in the same height
			H is 6 - M
		)).
%check if there's a diagonal (type \) in T with 3 connected pieces of player X
three_in_a_row(T, X, H):- append(_,[C1,C2,C3,C4|_],T), % check if 3 connected columns exists in board...
		((   
			append(I1,['-'|_],C1), %...such that all of them contain a piece of player X...
			append(I2,[X|_],C2),
			append(I3,[X|_],C3),
			append(I4,[X|_],C4),
			length(I1,M1), length(I2,M2), length(I3,M3), length(I4,M4),
			M2 is M1+1, M3 is M2+1, M4 is M3+1, %...and every piece is within the same diagonal \
			H is 6 - M1
		);(
			append(I1,[X|_],C1), %...such that all of them contain a piece of player X...
			append(I2,[X|_],C2),
			append(I3,[X|_],C3),
			append(I4,['-'|_],C4),
			length(I1,M1), length(I2,M2), length(I3,M3), length(I4,M4),
			M2 is M1+1, M3 is M2+1, M4 is M3+1, %...and every piece is within the same diagonal \
			H is 6 - M4
		)).
%check if there's a diagonal (type /) in T with 3 connected pieces of player X
three_in_a_row(T, X, H):- append(_,[C1,C2,C3,C4|_],T), % check if 3 connected columns exists in board...
		((   
			append(I1,['-'|_],C1), %...such that all of them contain a piece of player X...
			append(I2,[X|_],C2),
			append(I3,[X|_],C3),
			append(I4,[X|_],C4),
			length(I1,M1), length(I2,M2), length(I3,M3), length(I4,M4),
			M2 is M1-1, M3 is M2-1, M4 is M3-1, %...and every piece is within the same diagonal /
			H is 6 - M1
		);(
			append(I1,[X|_],C1), %...such that all of them contain a piece of player X...
			append(I2,[X|_],C2),
			append(I3,[X|_],C3),
			append(I4,['-'|_],C4),
			length(I1,M1), length(I2,M2), length(I3,M3), length(I4,M4),
			M2 is M1-1, M3 is M2-1, M4 is M3-1, %...and every piece is within the same diagonal /
			H is 6 - M4
		)).

%.......................................
% minimax
%.......................................
% The minimax algorithm always assumes an optimal opponent.
% For tic-tac-toe, optimal play will always result in a draw, 
% so the algorithm is effectively playing not-to-lose. 

% For the opening move against an optimal player, the best 
% minimax can ever hope for is a draw. Technically speaking, 
% any opening move is acceptable. Save the user the trouble 
% of waiting for the computer to search the entire minimax tree
% by simply selecting a random column. 
 
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
	:-	alpha(A), beta(B), A<B, !,
            move(Board,Column1,Mark,Board2),	%%% apply the first move (in the list)
            inverse_mark(Mark,Mark2), !,
				minimax(Depth,Board2,Mark2,_,Score1),	
				%%% recursively search for the score value of that move
			% 	(
			% 		maximizing(Mark) ->
			% 		Alpha0 is max(Alpha, Score1),
			% 		Beta0 is Beta
			% 	;	
			% 		Alpha0 is Alpha,
			% 		Beta0 is min(Beta, Score1)
			% 	), 
			% 	(	
			% 		not(Beta0 =< Alpha0) ->
			% 		%%% determine the best move of the remaining moves
			% 		best(Depth,Board,Mark,Other_Moves,Column2,Score2,Alpha0,Beta0)	
			% 	;	%%% if Beta <= Alpha we don't explore remaining moves 
			% 		Column2 is Column1,
			% 		Score2 is Score1
			% 	),
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
better(_,Mark,Column1,Score1,Column2,Score2,Column1,Score1) 
	:-	maximizing(Mark),				%%% if the player is maximizing
		Score1 > Score2, 
        asserta(alpha(Score1)), 
        noeudsParcourus(Np), NewNp is Np + 1, 
        retractall(noeudsParcourus(_)), asserta(noeudsParcourus(NewNp)), !.		%%% then greater is better.

better(_,Mark,Column1,Score1,Column2,Score2,Column1,Score1) 
	:-	minimizing(Mark),				%%% if the player is minimizing,
		Score1 < Score2, 
        asserta(beta(Score1)), noeudsParcourus(Np), NewNp is Np + 1, 
        retractall(noeudsParcourus(_)), asserta(noeudsParcourus(NewNp)), !.		%%% then lesser is better.
	
better(_,Mark,Column1,Score1,Column2,Score2,Column,Score) 
	:-	Score1 == Score2,		%%% if moves have equal score,
		random_between(1,10,R),		%%% then pick one of them at random
		better2(_,R,Mark,Column1,Score1,Column2,Score2,Column,Score), 
        noeudsParcourus(Np), NewNp is Np + 1, 
        retractall(noeudsParcourus(_)), asserta(noeudsParcourus(NewNp)), !.

better(_,Mark,Column1,Score1,Column2,Score2,Column2,Score2). 
									%%% otherwise, second move is better
	
%.......................................
% better2
%.......................................
% randomly selects among two columns of the same score value
%
better2(_,R,Mark,Column1,Score1,Column2,Score2,Column1,Score1) :- R < 6, !.
better2(_,R,Mark,Column1,Score1,Column2,Score2,Column2,Score2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Output and display
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

output_players :-
	nl, player(1, Who1),
	write('Le joueur 1 est '),	%%% either human or computer1 or computer2
	write(Who1), 
	nl, player(2, Who2),
	write('Le joueur 2 est '),	%%% either human or computer1 or computer2
	write(Who2), nl, ! .

 output_winner(Board) :- wins(Board,'X'), write('X gagne.'), output_noeudsParcourus, !.
 output_winner(Board) :- wins(Board,'O'), write('O gagne.'), output_noeudsParcourus, !.
 output_winner(Board) :- write('No winner: Draw').

output_value(1,Column,Score) 
	:- nl, write('Column '), write(Column), write(', score: '), write(Score), !.
output_value(Depth,Column,Score).

output_token :- nl, write('nbTokens '), nbTokens(Nb), write(Nb), nl, !.

output_noeudsParcourus :- nl, write('nbNoeudsParcourus '), noeudsParcourus(Np), write(Np), nl, !.