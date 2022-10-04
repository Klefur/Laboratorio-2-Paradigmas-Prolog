pixbit(X, Y, Bit, D, [X, Y, Bit, D]) :-
    integer(X), integer(Y), integer(Bit), integer(D),
    between(0, 1, Bit).

pixrgb(X, Y, R, G, B, D, [X, Y, [R, G, B], D]) :-
    integer(X), integer(Y), integer(R), integer(G), integer(B), integer(D),
    between(0, 255, R), between(0, 255, G), between(0, 255, B).

pixhex(X, Y, Hex, D, [X, Y, Hex, D]) :-
    integer(X), integer(Y), string(Hex), integer(D).

image(A, L, Pixeles, [A, L, Pixeles]).

bitmap([_,_,[]]).

bitmap([_,_,[H|C]]):-
    pixbit(_,_,_,_,H),
    bitmap([_,_,C]).

pixmap([_,_,[]]).

pixmap([_,_,[H|C]]):-
    pixrgb(_,_,_,_,_,_,H),
    pixmap([_,_,C]).

hexmap([_,_,[]]).

hexmap([_,_,[H|C]]):-
    pixhex(_,_,_,_,H),
    hexmap([_,_,C]).

imagen([_,_,[]]).

imagen([_,_,[H|C]]):-
    bitmap([_,_,[H|C]]);
    pixmap([_,_,[H|C]]);
    hexmap([_,_,[H|C]]).


flipV([_,_,[]],_).

flipV([A,L,[H|_]], [A,L,[H2]]) :-
    pixbit(X,Y,_,_,H),
    Y2 is (L - 1) - X,
    pixbit(X,Y2,_,_,H2). 

flipPix([X, Y, C, D], [X, Y2, C, D]) :-
    

img1(I) :-
    pixbit(0, 0, 1, 10, P1),
    pixbit(0, 1, 0, 10, P2),
    pixbit(1, 0, 0, 10, P3),
    pixbit(1, 0, 1, 10, P4), 
    image(2, 2, [P1, P2, P3, P4], I).

img2(I) :-
    pixrgb(0, 0, 1, 10, 30, 10, P1),
    pixrgb(0, 1, 0, 50, 132, 10, P2),
    pixrgb(1, 0, 0, 70, 122, 10, P3),
    pixrgb(1, 0, 1, 23, 133, 10, P4), 
    image(2, 2, [P1, P2, P3, P4], I).

img3(I) :-
    pixhex(0, 0, "#192341", 10, P1),
    pixhex(0, 1, "#00FFCC", 10, P2),
    pixhex(1, 0, "#00FFDD", 10, P3),
    pixhex(1, 0, "#77FFCC", 10, P4), 
    image(2, 2, [P1, P2, P3, P4], I).