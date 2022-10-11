pixbit-d(X,Y,Bit,Depth,[X,Y,Bit,Depth]) :-
    integer(X), integer(Y), integer(Bit), integer(Depth),
    between(0, 1, Bit).

pixrgb-d(X,Y,R,G,B,Depth,[X,Y,[R,G,B],Depth]) :-
    integer(X), integer(Y), integer(R), integer(G), integer(B), integer(Depth),
    between(0, 255, R), between(0, 255, G), between(0, 255, B).

pixhex-d(X,Y,Hex,Depth,[X, Y, Hex, D]) :-
    integer(X), integer(Y), string(Hex), integer(Depth),
    string_length(Hex,7).

pixel(X,Y,Color,Depth,[X,Y,Color,Depth]) :-
    pixbit-d(X,Y,Color,Depth,[X,Y,Color,Depth]);
    pixhex-d(X,Y,Color,Depth,[X,Y,Color,Depth]);
	pixrgb-d(X,Y,_,_,_,Depth,[X,Y,Color,Depth]).

image(A,L,Pixeles,[Ancho,Alto,Pixeles,CompressColor]):-
    string(CompressColor);integer(CompressColor);is_list(CompressColor); CompressColor is -1.

setCompressValue([Ancho,Alto,Pixs,CompressColor], X, [Ancho,Alto,Pixs,X]) :-
    image(_,_,_,[Ancho,Alto,Pixs,CompressColor]), image(_,_,_,[Ancho,Alto,Pixs,X]).

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

imageIsCompressed([Ancho,Alto,Pixs,CompressColor]):-
    CompressColor \== -1, image(_,_,_,[Alto,Ancho,Pixs,CompressColor]). 

imagen([_,_,[H|T],_]):-
    imageIsBitmap([_,_,[H|T],_]);imageIsPixmap([_,_,[H|T],_]);imageIsHexmap([_,_,[H|T],_]).
    
imageFlipV([Ancho,Alto,Pixs,CompressColor],[Ancho,Alto,P2,CompressColor]):-
    image(_,_,_,[Ancho,Alto,Pixs,CompressColor]),
    not(imageIsCompressed([Ancho,Alto,Pixs,CompressColor])),
    flipPixsV(Pixs,Alto,P2).

flipPixsV([],_,[]).
flipPixsV([H|T],Alto,[H2|T2]):-
    pixel(X,Y,Color,Depth,H),
    Y2 is Alto - 1 - Y,
    pixel(X,Y2,Color,Depth,H2),
    flipPixs(T,Alto,T2).

imageFlipH([Ancho,Alto,Pixs,CompressColor],[Ancho,Alto,P2,CompressColor]):-
    image(_,_,_,[Ancho,Alto,Pixs,CompressColor]),
    not(imageIsCompressed([Ancho,Alto,Pixs,CompressColor])),
    flipPixsH(Pixs,A,P2).

flipPixsH([],_,[]).
flipPixsH([H|T],A,[H2|T2]):-
    pixel(X,Y,Color,Depth,H),
    X2 is A - 1 - X,
    pixel(X2,Y,Color,Depth,H2),
    flipPixsH(T,A,T2).

imageCrop([Ancho,Alto,Pixs,CompressColor],X0,Y0,X1,Y1,I2) :-
    image(_,_,_,[Ancho,Alto,Pixs,CompressColor]),
    not(imageIsCompressed([Ancho,Alto,Pixs,CompressColor])),
    cropPixs(P,X0,Y0,X1,Y1,P2),
    Ancho2 is X1 + 1 - X0, Alto2 is Y1 + 1 - Y0,
    image(Ancho2,Alto2,P2,I2).

cropPixs([],_,_,_,_,[]).
cropPixs([H|T],X0,Y0,X1,Y1,T2):-
    pixel(X,Y,_,_,H),
    (X =< X0;X1 =< X),(Y =< Y0; Y1 =< Y),
    cropPixs(T,X0,Y0,X1,Y1,T2).
cropPixs([H|T],X0,Y0,X1,Y1,[H2|T2]):-
    pixel(X,Y,Color,Depth,H),
    X0 =< X,X =< X1,Y0 =< Y, Y =< Y1,
    X2 is X - X0, Y2 is Y - Y0,
    pixel(X2,Y2,Color,Depth,H2),
    cropPixs(T,X0,Y0,X1,Y1,T2).

imageRGBToHex([Ancho,Alto,Pixs,CompressColor], I2):-
    imageIsPixmap([Ancho,Alto,Pixs,CompressColor]),
    not(imageIsCompressed([Ancho,Alto,Pixs,CompressColor])),
    rgbToHexPixs(Pixs,P2),
    image(Ancho,Alto,P2,I2).

rgbToHexPixs([],[]).
rgbToHexPixs([H|T],[H2|T2]):-
    pixrgb-d(X,Y,R,G,B,Depth,H),
    rgbToHex([R,G,B],Hex),
    pixhex-d(X,Y,Hex,Depth,H2),
    rgbToHexPixs(T,T2).

hex("0",0).
hex("1",1).
hex("2",2).
hex("3",3).
hex("4",4).
hex("5",5).
hex("6",6).
hex("7",7).
hex("8",8).
hex("9",9).
hex("A",10).
hex("B",11).
hex("C",12).
hex("D",13).
hex("E",14).
hex("F",15).

rgbToHex([R,G,B], Hex):-
	R1 is R // 16, R2 is R - (R1 * 16),
    G1 is G // 16, G2 is G - (G1 * 16),
    B1 is B // 16, B2 is B - (B1 * 16),
    hex(StrR1, R1),hex(StrR2, R2),
    string_concat(StrR1,StrR2,StrR3),
    hex(StrG1, G1),hex(StrG2, G2),
    string_concat(StrG1,StrG2,StrG3),
    hex(StrB1, B1),hex(StrB2, B2),
    string_concat(StrB1,StrB2,StrB3),
    string_concat(StrR3,StrG3,StrRG3),
    string_concat(StrRG3,StrB3,StrRGB),
    string_concat("#",StrRGB,Hex).

imageRotate90([Ancho,Alto,Pixs,CompressColor], I2):-
    image(_,_,_,[Ancho,Alto,Pixs,CompressColor]),
    not(imageIsCompressed([Ancho,Alto,Pixs,CompressColor])),
    rotatePixs(P,Alto,P2),
    image(Alto,A,P2,I2).

rotatePixs([],_,[]).
rotatePixs([H|T],Alto,[H2|T2]):-
    pixel(X,Y,Color,Depth,H),
    Y2 is Alto - 1 - Y,
    pixel(Y2,X,Color,Depth,H2),
    rotatePixs(T,Alto,T2).

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