


% ------------------------------------------------------------------------------------------



% :- use_module(minimax).

% :- use_module(quarto).

piece(black_Round_Tall,[b,r,t]).
piece(black_Round_Short,[b,r,s]).
piece(black_Square_Tall,[b,s,t]).
piece(black_Square_Short,[b,s,s]).

piece(white_Round_Tall,[w,r,t]).
piece(white_Round_Short,[w,r,s]).
piece(white_Square_Tall,[w,s,t]).
piece(white_Square_Short,[w,s,s]).

list([black_Round_Tall,black_Round_Short,black_Square_Tall,black_Square_Short,white_Round_Tall,white_Round_Short,white_Square_Tall,white_Square_Short]).

random_choice([], []).
random_choice(List, Elt) :-
        length(List, Length),
        random(0, Length, Index),
        nth0(Index, List, Elt).

screen([]).
screen([X|T]):-
	write(X),nl,
	screen(T).		





play :-
    
  nl,
    write('===================='), 
  nl,    write('= Prolog Quarto ='),
  nl,
    write('===================='), 
  nl, 
  playAskDiff(Depth),
  playAskColor(Depth).
depth(easy,1).
depth(medium,4).
depth(hard,10).
	
playAskDiff(Depth) :-
    nl, write('select difficulty !'), nl,
    write('1-easy') , nl ,
    write('2-medium') , nl,
    write('3-hard')  , nl,
    
    
	  read(Diff), nl,

	  (\+ depth(Diff,_),!,     
	    write('Error : not a valid difficulty !'), nl, playAskDiff(Depth);
	    depth(Diff,Depth)
	  ). 

playAskColor(Depth) :-

	  nl, write('Color for human player ? (you or computer)'), nl,

	  read(Player), nl,

	  (Player \= computer, Player \= you, !,     
	    write('Error : not a valid color !'), nl, playAskColor(Depth)                    
	    ;
	    EmptyBoard = [e, e, e, e, e, e, e, e, e],
	    show(EmptyBoard), nl,
	
	    list(X),
	    play([you, play, EmptyBoard], Player,X,Depth)
	  ).

play([Player, play, Board], Player,List,Depth) :- 
    !,
   nl, 
   random_choice(List,Z),
   delete(List,Z,Newlist),
   
    write(Z),nl,
    write('Next move ?'), 
    nl, read(Pos), nl,
  % Ask human where to play
      (
        humanMove([Player, play, Board], [NextPlayer, State, NextBoard], Pos,Z), !,
        show(NextBoard),
        (
          State = win, !,                             
  % If Player win -> stop
          nl, write('End of game : '),
         nl,
         write('player win !'), nl, nl
          ;
   State = draw, !,                            
  % If draw -> stop
          nl, write('End of game : '),
          write(' draw !'), nl, nl
          ;
    %	nl,screen(NewList),nl,
    %	write('Enter a piece for me?'),nl,
    %	read(Piece),nl,
    %	delete(NewList,Piece,Secondlist),
          play([NextPlayer, play, NextBoard], Player,Newlist,Depth) 
  % Else -> continue the game
        )
   ;
  write('-> Bad Move !'), nl,                
  % If humanMove fail -> bad move
        play([Player, play, Board], Player,_,Depth)        
  % Ask again
      ).
  
  
  


play([Player, play, Board], HumanPlayer,List,Depth) :-
      
    readPiece(List,Piece),
    write(Piece),nl,
    delete(List,Piece,List_computer),
    
    nl, write('computer play : '), nl, nl,
    
  
      bestMove([Player, play, Board], [NextPlayer, State, BestSuccBoard],Piece,Depth),
      write('computer choosed!') , nl,
      show(BestSuccBoard),
      (
        State = win, !,                                 
  % If Player win -> stop
        nl, write('End of game : '), nl,
        write('computer win !'), nl, nl
        ;
        State = draw, !,                                
  % If draw -> stop
        nl, write('End of game : '), write(' draw !'), nl, nl
        ;
        
  % Else -> continue the game
        play([NextPlayer, play, BestSuccBoard], HumanPlayer,List_computer,Depth)
      ).
  


readPiece(List,Piece) :-
    write('Enter a piece for me ?'), 
    nl,screen(List),nl,
    nl, read(Piece), nl, 
    member(Piece, List),!.

readPiece(List , Piece) :-
  write('Please enter a valid piece :)'),
  readPiece(List , Piece).
    






% When human play
humanMove([X1, play, Board], [X2, State, NextBoard], Pos,Z) :-
    nextPlayer(X1, X2),
    set1(Pos, Z, Board, NextBoard),
    (
      winPos( NextBoard), !, State = win ;
      drawPos(NextBoard), !, State = draw ;
      State = play
    ).






set1(1, E, [X|Ls], [E|Ls]) :- 
  !, X = e.


set1(P, E, [X|Ls], [X|L2s]) :-
    
  number(P),
    
  P1 is P - 1,
    
  set1(P1, E, Ls, L2s).



show([X1, X2, X3, X4, X5, X6, X7, X8, X9]) :-
    
   
  write('   '), show2(X1),
 write(' | '), show2(X2),
 write(' | '), show2(X3), nl,
  write('  ------------------------------------------'), nl,

  write('   '), show2(X4),
 write(' | '), show2(X5),
 write(' | '), show2(X6), nl,
 
  write('  ------------------------------------------'), nl,

  write('   '), show2(X7), write(' | '), show2(X8),
 write(' | '), show2(X9), nl.


show2(X) :-
    
  X = e, !,
    write('                 ').


show2(X) :-
    
  write(X).



bestMove(Pos, NextPos,Piece,Depth) :-
    
  minimax(Pos, NextPos, _,Piece , Depth).

% :- module(minimax, [minimax/4]).

minimax(Pos, Pos, Val,_, 0) :-
    utility(Pos, Val) , ! .                                         % stop recursion here

minimax(Pos, BestNextPos, Val,Piece , Depth) :-                     % Pos has successors
    bagof(NextPos, move(Pos, NextPos,Piece), NextPosList),
    NextDepth is Depth - 1 ,
    best(NextPosList, BestNextPos, Val,Piece ,NextDepth), !.

minimax(Pos, Pos, Val,_ ,_) :-                     % Pos has no successors
    utility(Pos, Val).


best([Pos], Pos, Val,Piece ,Depth) :-
    minimax(Pos, _, Val,Piece , Depth), !.

best([Pos1 | PosList], BestPos, BestVal,Piece , Depth) :-
    minimax(Pos1, _, Val1,Piece , Depth),
    best(PosList, Pos2, Val2,Piece , Depth),
    betterOf(Pos1, Val1, Pos2, Val2, BestPos, BestVal).



betterOf(Pos0, Val0, _, Val1, Pos0, Val0) :-   % Pos0 better than Pos1
    min_to_move(Pos0),                         % MIN to move in Pos0
    Val0 > Val1, !                             % MAX prefers the greater value
    ;
    max_to_move(Pos0),                         % MAX to move in Pos0
    Val0 < Val1, !.                            % MIN prefers the lesser value

betterOf(_, _, Pos1, Val1, Pos1, Val1).        % Otherwise Pos1 better than Pos0



% ------------------------------------------------------------------------------------------
% :- module(quarto, [move/3,min_to_move/1,max_to_move/1,utility/2,winPos/1,drawPos/1]).


nextPlayer(you,computer).
nextPlayer(computer,you).


move([X1, play, Board],[X2, win, NextBoard],Piece) :-

 nextPlayer(X1, X2),

 move_aux(Piece, Board, NextBoard),
    
 winPos(NextBoard), !.



move([X1, play, Board],[X2, draw, NextBoard],Piece) :-

 nextPlayer(X1, X2),

 move_aux(Piece, Board, NextBoard),
    
 drawPos(NextBoard),!.



move([X1, play, Board], [X2, play, NextBoard],Piece) :-
 nextPlayer(X1, X2),
    
 move_aux(Piece, Board, NextBoard).


move_aux(P, [e|Bs], [P|Bs]).


move_aux(P, [B|Bs], [B|B2s]) :-
    
move_aux(P, Bs, B2s).





min_to_move([you, _, _]).


max_to_move([computer, _, _]).




utility([computer, win, _], 1).      

utility([you, win, _], -1).      

utility([_, draw,  _], 0).

utility([_, play, _],0). % undetermine state!



winPos([X1, X2, X3, X4, X5, X6, X7, X8, X9]) :-
    
equal(X1, X2, X3) ;    
% 1st line
    
equal(X4, X5, X6) ;    
% 2nd line
    
equal(X7, X8, X9) ;    
% 3rd line
    
equal(X1, X4, X7) ;    
% 1st col
    
equal(X2, X5, X8) ;    
% 2nd col
    
equal(X3, X6, X9) ;
% 3rd col
    
equal(X1, X5, X9) ;    
% 1st diag
    
equal(X3, X5, X7).     
% 2nd diag




drawPos(Board) :-

  \+ member(e, Board).



equal(X1,X2,X3):-
(

piece(X1,[X,_,_]),
piece(X2,[X,_,_]),
piece(X3,[X,_,_])
);
(
piece(X1,[_,XX,_]),
piece(X2,[_,XX,_]),
piece(X3,[_,XX,_])
);
(
piece(X1,[_,_,XXX]),
piece(X2,[_,_,XXX]),
piece(X3,[_,_,XXX])
).

