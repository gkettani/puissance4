:-dynamic board/1.
:-dynamic player/2.

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
% The player playing x is always trying to maximize board position utility
minimizing('O').	
% The player playing o is always trying to minimize board position utility

maxDepth(4).
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
				retractall(player(_,_)), retractall(board(_)),	
				asserta(board([['-','-','-','-','-','-'],
                               ['-','-','-','-','-','-'],
                               ['-','-','-','-','-','-'],
                               ['-','-','-','-','-','-'],
                               ['-','-','-','-','-','-'],
                               ['-','-','-','-','-','-'],
                               ['-','-','-','-','-','-']])). 
				%%% create an empty board

goodbye :- 	board(Board), nl, nl, write('Game over: '), 
				output_winner(Board),
					retractall(player(_,_)), retractall(board(_)),	
						read_play_again(V), !, 
							(V == 'Y' ; V == 'y'), !, jouer.

read_play_again(V) :- nl, nl, write('Play again (Y/N)? '), read(V),
						(V == 'y' ; V == 'Y' ; V == 'n' ; V == 'N'), !.
read_play_again(V) :- nl, nl, write('Please enter Y or N.'),
						read_play_again(V).

read_players 
	:- nl, nl, write('Number of human players? '), read(N), set_players(N).

set_players(0) :- asserta(player(1,computer1)), asserta(player(2,computer2)), !.

set_players(1) :- nl, write('Voulez vous jouer X ou O (X commence en premier)? '),
					read(M), human_playing(M), !.

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
					next_player(Player1,Player2), play(Player2).

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
readColumn(C):- nl, write(' , veuillez choisir une colonne : '),
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
				retract(board(_)), asserta(board(Board2)).

make_move2(human, Player, Board1, Board2) 
	:-	nl, nl, write('C\'est au tour du joueur '), write(Player), readColumn(Column), !
    			, player_mark(Player, Mark), move(Board1,Column,Mark,Board2), !.
				
make_move2(human, Player, Board1, Board2) 
	:-	nl, nl, write('Veuillez choisir une lettre entre A et G.'),
			make_move2(human,Player,Board1,Board2).

% A move computed thanks to minimax is made of values for the 3 variables 
% Mark, Column and Utility

make_move2(computer1, Player, Board1, Board2) 
	:-	nl, nl, write('Computer1 est entrain de réflechir ...'), nl,
			player_mark(Player, Mark),
				minimax(0, Board1, Mark, Column, _),
					move(Board1,Column,Mark,Board2), !,
		nl, nl, write('Computer1 place '), write(Mark), write(' dans la colonne '), associateChar(L,Column),
			write(L), write('.').

make_move2(computer2, Player, Board1, Board2) 
	:-	nl, nl, write('Computer2 est entrain de réflechir ...'), nl,
			player_mark(Player, Mark),
				repeat, random_between(0,6,Column),
					move(Board1,Column,Mark,Board2), !,
		nl, write('Computer2 place '), write(Mark), write(' dans la colonne '), associateChar(L,Column),
			write(L), write('.').

%.......................................
% possible_moves : A adapter !
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
% utility
%.......................................
% It computes the value of a given board position
% 
utility(Board,1) :- wins(Board,'X'), !.
utility(Board,-1) :- wins(Board,'O'), !.
utility(_,0).

%.......................................
% minimax : A adapter !
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



/*
	Implémentation de l'algorithme minimax
	Appel de l'algorithme : minimax(EtatDuJeu, ProfondeurLimite, JoueurActuel)

	minimax(EtatDuJeu, ProfondeurLimite, JoueurActuel)
		Si ProfondeurLimite est défini à 0 ou si on est sur un noeud feuille
		Alors retourner la valeur de l'évaluation du jeu

		Si le joueur est celui à maximiser
		Alors
			Définir la valeur de base à -infini
			Pour chaque Noeud enfant (possible moves)
				valeur = maximum(valeur, minimax(NoeudEnfant, ProfondeurLimite - 1, JoueurSuivant))
		Sinon // Le joueur est celui à minimiser
			Définir la valeur de base à +infini
			Pour chaque Noeud enfant (possible moves)
				valeur = minimum(valeur, minimax(NoeudEnfant, ProfondeurLimite - 1, JoueurSuivant))
		Return valeur



		minimax(Board, Depth, Player, Move, Eval) :-
			Depth == MaxDepth,
			evaluate(Board, Player, Eval).
			// TODO - Remonter la valeur donnée par evaluate

		minimax(Board, Depth, Player, [Move|MoveList], Eval) :-
			playerMark(Player, Mark),
			move(Board, Move, Mark, NewBoard),

			// TODO - trouver un moyen de jouer un coup
			possibleMoves(NewBoard, [FirstMove|Moves]),
			move(Board, FirstMove, Player, NewBoard),
			minimax(Depth-1),
			minimax(Depth, Moves)

			// TODO - Faire un minimax qui descend et un autre qui reste sur la meme profondeur

			nextPlayer(Player, NextPlayer),
			NextDepth is Depth - 1,
			minimax(Board, NextDepth, NextPlayer, Move, Eval),

			better(Depth,Player,Move1,Score1,Move2,Score2,ResMove,ResScore)



			Problèmes identifiés :
									Remonter les valeurs et les sélectionner
*/


minimax(Depth,Board,Mark,Column,Utility) :-
 maxDepth(D), Depth<D,
 Depth2 is Depth+1,
 possible_moves(Board,List), !,		%%% get the list of possible moves
	best(Depth2,Board,Mark,List,Column,Utility), !.	
					%%% recursively determine the best available move

% If there are no more available moves, then the minimax value is 
% the utility of the given board position 
 
minimax(_,Board,_,_,Utility) :- utility(Board,Utility).

%.......................................
% best : A adapter !
%.......................................
% determines the best move in a given list of moves by 
% recursively calling minimax
% 
% if there is only one move left in the list... 

best(Depth,Board,Mark,[Column1],Column1,Utility) 
	:-	move(Board,Column1,Mark,Board2),	%%% apply that move to the board,
			inverse_mark(Mark,Mark2), !,
			%%% then recursively search for the utility of that move.
				minimax(Depth,Board2,Mark2,_,Utility), !,	 
				output_value(Depth,Column1,Utility), !.

% if there is more than one move in the list... 

best(Depth,Board,Mark,[Column1|Other_Moves],Column,Utility) 
	:-	move(Board,Column1,Mark,Board2),	%%% apply the first move (in the list)
			inverse_mark(Mark,Mark2), !,
				minimax(Depth,Board2,Mark2,_,Utility1),
			%%% recursively search for the utility value of that move
			%%% and determine the best move of the remaining moves
				best(Depth,Board,Mark,Other_Moves,Column2,Utility2),	
				output_value(Depth,Column1,Utility1),
			better(Depth,Mark,Column1,Utility1,Column2,Utility2,Column,Utility). 	
	%%% choose the better of the two moves based on their utility values

%.......................................
% better : A adapter !
%.......................................
% returns the better of two moves based on their utility values.
%
% if both moves have the same utility value, then one is chosen at random. 
%
better(_,Mark,Column1,Utility1,_,Utility2,Column1,Utility1) 
	:-	maximizing(Mark),			%%% if the player is maximizing
		Utility1 > Utility2, !.		%%% then greater is better.

better(_,Mark,Column1,Utility1,_,Utility2,Column1,Utility1) 
	:-	minimizing(Mark),			%%% if the player is minimizing,
		Utility1 < Utility2, !.		%%% then lesser is better.
	
better(_,Mark,Column1,Utility1,Column2,Utility2,Column,Utility) 
	:-	Utility1 == Utility2,		%%% if moves have equal utility,
		random_between(1,10,R),		%%% then pick one of them at random
		better2(_,R,Mark,Column1,Utility1,Column2,Utility2,Column,Utility), !.

better(_,_,_,_,Column2,Utility2,Column2,Utility2). 
									%%% otherwise, second move is better
	
%.......................................
% better2 : A adapter !
%.......................................
% randomly selects among two columns of the same utility value
%
better2(_,R,_,Column1,Utility1,_,_,Column1,Utility1) :- R < 6, !.
better2(_,_,_,_,_,Column2,Utility2,Column2,Utility2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Output and display
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

output_players :-
	nl, player(1, Who1),
	write('Le joueur 1 est '),	%%% either human or computer1 or computer2
	write(Who1), 
	nl, player(2, Who2),
	write('Le joueur 2 est '),	%%% either human or computer1 or computer2
	write(Who2), ! .

 output_winner(Board) :- wins(Board,'X'), write('X gagne.'), !.
 output_winner(Board) :- wins(Board,'O'), write('O gagne.'), !.
 output_winner(_) :- write('No winner: Draw').

output_value(1,Square,Utility) 
	:- nl, write('Column '), write(Square), write(', utility: '), write(Utility), !.
output_value(_,_,_).
