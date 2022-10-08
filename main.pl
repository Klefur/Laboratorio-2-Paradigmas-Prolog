pixbit-d(X,Y,Bit,D,[X,Y,Bit,D]) :-
    integer(X), integer(Y), integer(Bit), integer(D),
    between(0, 1, Bit).

pixrgb-d(X,Y,R,G,B,D,[X,Y,[R,G,B],D]) :-
    integer(X), integer(Y), integer(R), integer(G), integer(B), integer(D),
    between(0, 255, R), between(0, 255, G), between(0, 255, B).

pixhex-d(X,Y,Hex,D,[X, Y, Hex, D]) :-
    integer(X), integer(Y), string(Hex), integer(D).

pixel(X,Y,Color,D,P1) :-
    pixbit-d(X,Y,Color,D,P1);
    pixhex-d(X,Y,Color,D,P1);
	pixrgb-d(_,_,R,G,B,_,[_,_,Color,_]), pixrgb-d(X,Y,R,G,B,D,P1).

image(A,L,Pixeles,[A,L,Pixeles,C]):-
    string(C);integer(C);is_list(C); C is -1.

setCompressValue([A,L,P,C], X, [A,L,P,X]) :-
    image(_,_,_,[A,L,P,C]), image(_,_,_,[A,L,P,X]).

imageIsBitmap([_,_,[],_]).
imageIsBitmap([_,_,[H|T],_]):-
    pixbit-d(_,_,_,_,H),
    imageIsBitmap([_,_,T,_]).

imageIsPixmap([_,_,[],_]).
imageIsPixmap([_,_,[H|T],_]):-
    pixrgb-d(_,_,_,_,_,_,H),
    imageIsPixmap([_,_,T,_]).

imageIsHexmap([_,_,[],_]).
imageIsHexmap([_,_,[H|T],_]):-
    pixhex-d(_,_,_,_,H),
    imageIsHexmap([_,_,T,_]).

imageIsCompressed([L,A,P,C]):-
    C \== -1, image(_,_,_,[L,A,P,C]). 

imagen([_,_,[H|T],_]):-
    imageIsBitmap([_,_,[H|T],_]);imageIsPixmap([_,_,[H|T],_]);imageIsHexmap([_,_,[H|T],_]).
    
imageFlipV([A,L,P,C],[A,L,P2,C]):-
    image(_,_,_,[A,L,P,C]),
    not(imageIsCompressed([A,L,P,C])),
    flipPixsV(P,L,P2).

flipPixsV([],_,[]).
flipPixsV([H|T],L,[H2|T2]):-
    pixel(X,Y,Color,D,H),
    Y2 is L - 1 - Y,
    pixel(X,Y2,Color,D,H2),
    flipPixs(T,L,T2).

imageFlipH([A,L,P,C],[A,L,P2,C]):-
    image(_,_,_,[A,L,P,C]),
    not(imageIsCompressed([A,L,P,C])),
    flipPixsH(P,A,P2).

flipPixsH([],_,[]).
flipPixsH([H|T],A,[H2|T2]):-
    pixel(X,Y,Color,D,H),
    X2 is A - 1 - X,
    pixel(X2,Y,Color,D,H2),
    flipPixsH(T,A,T2).

imageCrop([A,L,P,C],X0,Y0,X1,Y1,I2) :-
    image(_,_,_,[A,L,P,C]),
    not(imageIsCompressed([A,L,P,C])),
    cropPixs(P,X0,Y0,X1,Y1,P2),
    A2 is X1 + 1 - X0, L2 is Y1 + 1 - Y0,
    image(A2,L2,P2,I2).

cropPixs([],_,_,_,_,[]).
cropPixs([H|T],X0,Y0,X1,Y1,T2):-
    pixel(X,Y,_,_,H),
    (X =< X0;X1 =< X),(Y =< Y0; Y1 =< Y),
    cropPixs(T,X0,Y0,X1,Y1,T2).
cropPixs([H|T],X0,Y0,X1,Y1,[H2|T2]):-
    pixel(X,Y,Color,D,H),
    X0 =< X,X =< X1,Y0 =< Y, Y =< Y1,
    X2 is X - X0, Y2 is Y - Y0,
    pixel(X2,Y2,Color,D,H2),
    cropPixs(T,X0,Y0,X1,Y1,T2).

imageRGBToHex([A,L,P,C], [A,L,P2,C]):-
    imageIsPixmap([A,L,P,C]),
    not(imageIsCompressed([A,L,P,C])),
    rgbToHexPixs(P,P2),
    image([A,L,P2,C]).

rgbToHexPixs([],[]).
rgbToHexPixs([H|T],[H2|T2]):-
    pixrgb-d(X,Y,R,G,B,D,H),
    hex_bytes(Hex,[R,G,B]), concat("#",Hex,Hex2),
    pixhex-d(X,Y,Hex2,D,H2),
    rgbToHexPixs(T,T2).

imageRGBToHex([A,L,P,C], I2):-
    imageIsPixmap([A,L,P,C]),
    not(imageIsCompressed([A,L,P,C])),
    rgbToHexPixs(P,P2),
    image(A,L,P2,I2).

rgbToHexPixs([],[]).
rgbToHexPixs([H|T],[H2|T2]):-
    pixrgb-d(X,Y,R,G,B,D,H),
    hex_bytes(Hex,[R,G,B]), string_concat("#",Hex,Hex2),
    pixhex-d(X,Y,Hex2,D,H2),
    rgbToHexPixs(T,T2).

img1(I) :-
    pixbit-d(0, 0, 1, 10, P1),
    pixbit-d(0, 1, 0, 10, P2),
    pixbit-d(1, 0, 0, 10, P3),
    pixbit-d(1, 0, 1, 10, P4), 
    image(2, 2, [P1, P2, P3, P4], I).

img2(I) :-
    pixrgb-d(0, 0, 1, 10, 30, 10, P1),
    pixrgb-d(0, 1, 0, 50, 132, 10, P2),
    pixrgb-d(1, 0, 0, 70, 122, 10, P3),
    pixrgb-d(1, 1, 1, 23, 133, 10, P4), 
    image(2, 2, [P1, P2, P3, P4], I).

img3(I) :-
    pixhex-d(0, 0, "#192341", 10, P1),
    pixhex-d(0, 1, "#00FFCC", 10, P2),
    pixhex-d(1, 0, "#00FFDD", 10, P3),
    pixhex-d(1, 0, "#77FFCC", 10, P4), 
    image(2, 2, [P1, P2, P3, P4], I).