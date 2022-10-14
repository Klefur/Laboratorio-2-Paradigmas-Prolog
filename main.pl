pixbit-d(X,Y,Bit,Depth,[X,Y,Bit,Depth]) :-
    integer(X), integer(Y), integer(Bit), integer(Depth),
    between(0, 1, Bit).

pixrgb-d(X,Y,R,G,B,Depth,[X,Y,[R,G,B],Depth]) :-
    integer(X), integer(Y), integer(R), integer(G), integer(B), integer(Depth),
    between(0, 255, R), between(0, 255, G), between(0, 255, B).

pixhex-d(X,Y,Hex,Depth,[X, Y, Hex, Depth]) :-
    integer(X), integer(Y), string(Hex), integer(Depth),
    string_length(Hex,7).

pixel(X,Y,Color,Depth,[X,Y,Color,Depth]) :-
    pixbit-d(X,Y,Color,Depth,[X,Y,Color,Depth]);
    pixhex-d(X,Y,Color,Depth,[X,Y,Color,Depth]);
	pixrgb-d(X,Y,_,_,_,Depth,[X,Y,Color,Depth]).

image(Width,Height,Pixs,[Width,Height,Pixs,CompressColor]):-
    string(CompressColor);integer(CompressColor);is_list(CompressColor); CompressColor is -1.

setCompressValue([Width,Height,Pixs,CompressColor], X, [Width,Height,Pixs,X]) :-
    image(_,_,_,[Width,Height,Pixs,CompressColor]), image(_,_,_,[Width,Height,Pixs,X]).

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

imageIsCompressed([Width,Height,Pixs,CompressColor]):-
    CompressColor \== -1, image(_,_,_,[Height,Width,Pixs,CompressColor]).
    
imageFlipV([Width,Height,Pixs,CompressColor],[Width,Height,P2,CompressColor]):-
    not(imageIsCompressed([Width,Height,Pixs,CompressColor])),
    flipPixsV(Pixs,Height,P2).

flipPixsV([],_,[]).
flipPixsV([H|T],Height,[H2|T2]):-
    pixel(X,Y,Color,Depth,H),
    Y2 is Height - 1 - Y,
    pixel(X,Y2,Color,Depth,H2),
    flipPixs(T,Height,T2).

imageFlipH([Width,Height,Pixs,CompressColor],[Width,Height,P2,CompressColor]):-
    not(imageIsCompressed([Width,Height,Pixs,CompressColor])),
    flipPixsH(Pixs,Width,P2).

flipPixsH([],_,[]).
flipPixsH([H|T],Width,[H2|T2]):-
    pixel(X,Y,Color,Depth,H),
    X2 is Width - 1 - X,
    pixel(X2,Y,Color,Depth,H2),
    flipPixsH(T,Width,T2).

imageCrop([Width,Height,Pixs,CompressColor],X0,Y0,X1,Y1,I2) :-
    not(imageIsCompressed([Width,Height,Pixs,CompressColor])),
    cropPixs(Pixs,X0,Y0,X1,Y1,P2),
    Width2 is X1 + 1 - X0, Height2 is Y1 + 1 - Y0,
    image(Width2,Height2,P2,I2).

cropPixs([],_,_,_,_,[]).
cropPixs([H|T],X0,Y0,X1,Y1,[H2|T2]):-
    pixel(X,Y,Color,Depth,H),
    X0 =< X,X =< X1,Y0 =< Y, Y =< Y1,
    X2 is X - X0, Y2 is Y - Y0,
    pixel(X2,Y2,Color,Depth,H2),
    cropPixs(T,X0,Y0,X1,Y1,T2).
cropPixs([H|T],X0,Y0,X1,Y1,T2):-
    pixel(X,Y,_,_,H),
    (X =< X0;X1 =< X),(Y =< Y0; Y1 =< Y),
    cropPixs(T,X0,Y0,X1,Y1,T2).

imageRGBToHex([Width,Height,Pixs,CompressColor], I2):-
    not(imageIsCompressed([Width,Height,Pixs,CompressColor])),
    rgbToHexPixs(Pixs,P2),
    image(Width,Height,P2,I2).

rgbToHexPixs([],[]).
rgbToHexPixs([H|T],[H2|T2]):-
    pixrgb-d(X,Y,R,G,B,Depth,H),
    hex_bytes(Hex,[R,G,B]),string_upper(Hex,HexUp),
    string_concat("#",HexUp,Hex2),
    pixhex-d(X,Y,Hex2,Depth,H2),
    rgbToHexPixs(T,T2).

imageToHistogram([Width,Height,Pixs,CompressColor], Histogram):-
    not(imageIsCompressed([Width,Height,Pixs,CompressColor])),
	genHistogram(Pixs,Histogram).

genHistogram([],[]).
genHistogram([H|T],H2):-
    genHistogram(T,T2),
    pixel(_,_,Color,_,H),
    not(member([Color,_],T2)),
    myAppend(T2,[Color, 1],H2).
genHistogram([H|T],H2):-
    genHistogram(T,T2),
    pixel(_,_,Color,_,H),
    member([Color,_],T2),
    add1(T2,Color,H2).

myAppend([],E,[E]).
myAppend([H],E,[H,E]).
myAppend([H|T],E,[H|T2]):-
    myAppend(T,E,T2).
    
add1([],_,[]).
add1([[E,C]|T],E2,[[E,C2]|T]):-
    E2 == E,
    C2 is C + 1.
add1([[E,C]|T],E2,[[E,C]|T2]):-
    add1(T,E2,T2).

getMayor(M,[M,_],[]).
getMayor(M,[_,C],[[H,C2]|T]):-
    C2 >= C,
    getMayor(M,[H,C2],T).
getMayor(M,[Y,C],[[_,C2]|T]):-
    C2 =< C,
    getMayor(M,[Y,C],T).

imageRotate90([Width,Height,Pixs,CompressColor], I2):-
    not(imageIsCompressed([Width,Height,Pixs,CompressColor])),
    rotatePixs(Pixs,Height,P2),
    image(Height,Width,P2,I2).

rotatePixs([],_,[]).
rotatePixs([H|T],Height,[H2|T2]):-
    pixel(X,Y,Color,Depth,H),
    Y2 is Height - 1 - Y,
    pixel(Y2,X,Color,Depth,H2),
    rotatePixs(T,Height,T2).

imageCompress([Width,Height,Pixs,CompressedColor],I3):-
    not(imageIsCompressed([Width,Height,Pixs,CompressedColor])),
    imageToHistogram([Width,Height,Pixs,CompressedColor], Histo),
    getMayor(M,[-1,-1],Histo),
    compressPixs(Pixs,M,P2),
    image(Width,Height,P2,I2),
    setCompressValue(I2,M,I3).

compressPixs([],_,[]).
compressPixs([H|T],M,[H|T2]):-
    pixel(_,_,Color,_,H),
    Color \== M,
    compressPixs(T,M,T2).
compressPixs([_|T],M,T2):-
    compressPixs(T,M,T2).

imageChangePixel([Width,Height,Pixs,CompressColor],P2Mod,I2):-
    pixel(_,_,_,_,P2Mod),
    not(imageIsCompressed([Width,Height,Pixs,CompressColor])),
    changePixs(Pixs,P2Mod,Pixs2),
    image(Width,Height,Pixs2,I2).

changePixs([],_,[]).
changePixs([H|T],PMod,[PMod|T2]):-
    pixel(X,Y,_,_,H),
    pixel(X,Y,_,_,PMod),
    changePixs(T,PMod,T2).
changePixs([H|T],PMod,[H|T2]):-
    changePixs(T,PMod,T2).

img1(I) :-
    pixbit-d(0, 0, 1, 10, P1),
    pixbit-d(0, 1, 0, 12, P2),
    pixbit-d(1, 0, 0, 10, P3),
    pixbit-d(1, 0, 1, 12, P4), 
    image(2, 2, [P1, P2, P3, P4], I).

img2(I) :-
    pixrgb-d(0, 0, 1, 10, 30, 10, P1),
    pixrgb-d(0, 1, 0, 50, 132, 12, P2),
    pixrgb-d(1, 0, 0, 70, 122, 10, P3),
    pixrgb-d(1, 1, 1, 23, 133, 12, P4), 
    image(2, 2, [P1, P2, P3, P4], I).

img3(I) :-
    pixhex-d(0, 0, "#192341", 10, P1),
    pixhex-d(0, 1, "#00FFCC", 12, P2),
    pixhex-d(1, 0, "#00FFDD", 10, P3),
    pixhex-d(1, 0, "#77FFCC", 12, P4), 
    image(2, 2, [P1, P2, P3, P4], I).