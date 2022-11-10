:- use_module(util).
:- use_module(minimax).
:- use_module(greedy).

:-dynamic board/1.
:-dynamic player/2.

%%%%%%%%%%%%%%%
%%% CLAUSES %%%
%%%%%%%%%%%%%%%
next_player(1,2).		%%% determines the next player after the given player
next_player(2,1). 

player_mark(1,'X').		%%% the mark for the given player
player_mark(2,'O'). 
 
opponent_mark(1,'O'). 	%%% the inverse mark of the given player
opponent_mark(2,'X'). 

maxDepth(5). 			%%% the max depth for Minimax algorithm


%%%%%%%%%%%%%%%%%%%%
%%% Main program %%%
%%%%%%%%%%%%%%%%%%%%
jouer :- welcome,				%%% Display welcome message, initialize game 
			play(1),			%%% Play the game starting with player 1 
				goodbye.        %%% Display end of game message
jouer :- goodbye.

welcome :- initialize, nl, nl, write('Début du jeu Puissance 4'),
			read_players, output_players.

initialize :-	%%% random seed may use time to initialize random number generator
				retractall(player(_,_)), retractall(board(_)), retractall(nbTokens(_)),	
				asserta(board([['-','-','-','-','-','-'],
                               ['-','-','-','-','-','-'],
                               ['-','-','-','-','-','-'],
                               ['-','-','-','-','-','-'],
                               ['-','-','-','-','-','-'],
                               ['-','-','-','-','-','-'],
                               ['-','-','-','-','-','-']])),
				asserta(nbTokens(0)). 
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
	:- nl, nl, write('Nombre de joueurs humains? '), flush_output, read(N), set_players(N).

set_players(0) :- asserta(player(1,computer1)), asserta(player(2,computer2)), !.

set_players(1) :- nl, write('Voulez vous jouer X ou O ? (X joue avec minimax, O avec greedy)'),
					get_char(M), human_playing(M), !.

set_players(2) :- asserta(player(1,human)), asserta(player(2,human)), !.
 
set_players(_) :- nl, write('Veuillez taper 0, 1, ou 2.'), read_players.

human_playing(M) :-
	(M == 'x' ; M == 'X'), 
		asserta(player(1,human)), asserta(player(2,computer2)), !.

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
% readColumn
%.......................................
%reads a column
readColumn(C):- writeln('Veuillez choisir une colonne entre A et G : '),
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
	:-	nl, nl, write('C\'est au tour du joueur '), write(Player), nl, readColumn(Column)
    			, player_mark(Player, Mark), move(Board1,Column,Mark,Board2), !.
				
make_move2(human, Player, Board1, Board2) 
	:-	nl, nl, write('Veuillez choisir une colonne valide.'),
			make_move2(human,Player,Board1,Board2).

% A move computed thanks to minimax is made of values for the 3 variables 
% Mark, Column and Score

make_move2(computer1, Player, Board1, Board2) 
	:-	nl, nl, write('Computer1 est entrain de réflechir ...'), nl,
			player_mark(Player, Mark),
				retractall(maximizing(_)), asserta(maximizing(Mark)),
				retractall(minimizing(_)), inverse_mark(Mark, Opponent),asserta(minimizing(Opponent)),
					minimax(0, Board1, Mark, Column, _),
						move(Board1,Column,Mark,Board2), !,
		nl, nl, write('Computer1 place '), write(Mark), write(' dans la colonne '), associateChar(L,Column),
			write(L), write('.'), nl.

make_move2(computer2, Player, Board1, Board2) 
	:-	nl, nl, write('Computer2 est entrain de réflechir ...'), nl,
			player_mark(Player, Mark),
				greedy(Board1,Mark,Column),
					move(Board1,Column,Mark,Board2), !,
		nl, write('Computer2 place '), write(Mark), write(' dans la colonne '), associateChar(L,Column),
			write(L), write('.'), nl.

%.......................................
% game_over
%.......................................
% Game is over if opponent wins or if none of the columns are empty
%
game_over(Player,Board) :- opponent_mark(Player, Mark), wins(Board, Mark), !.
game_over(_,Board) :- isFull(Board). 

%isFull(T) is satisfied if there isn't any free spot ('-')
isFull(T):- \+ (append(_,[C|_],T), append(_,['-'|_],C)).


%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% OUTPUT AND DISPLAY %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%
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

output_players :-
	nl, player(1, Who1),
	write('Le joueur 1 est '),	%%% either human or computer1 or computer2
	write(Who1), 
	nl, player(2, Who2),
	write('Le joueur 2 est '),	%%% either human or computer1 or computer2
	write(Who2), nl, nl, ! .

 output_winner(Board) :- wins(Board,'X'), write('X gagne.'), !.
 output_winner(Board) :- wins(Board,'O'), write('O gagne.'), !.
 output_winner(_) :- write('No winner: Draw').

output_value(1,Column,Score) 
	:- nl, write('Column '), write(Column), write(', score: '), write(Score), !.
output_value(_,_,_).

output_token :- nl, write('nbTokens '), nbTokens(Nb), write(Nb), nl, !.