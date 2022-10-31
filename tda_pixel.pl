:- module(tda_pixel,[pixbit/5, pixhex/5, pixel/5, pixrgb/7]).

% -------------------------------------- PIXEL -------------------------------------------------

/*
Dominio:
    pixbit: Pixbit
    pixhex: Pixhex
    pixrgb: Pixrgb
    Pixel: Pixrgb|Pixhex|Pixbit
    X,Y,Bit,Depth: Integer

Predicados:
    pixbit(X,Y,Bit,Depth,[X,Y,Bit,Depth]) (aridad = 5)
    pixrgb(X,Y,R,G,B,Depth,[X,Y,R,G,B,Depth]) (aridad = 5)
    pixbit(X,Y,Hex,Depth,[X,Y,Hex,Depth]) (aridad = 5)
    pixbit(X,Y,Color,Depth,[X,Y,Color,Depth]) (aridad =5)

Metas primarias: 
    pixbit, pixhex, pixrgb
Metas secundarias: 
    pixel

*/

% Clausulas
% Reglas

% -------------------------------------- CONSTRUCTORES -------------------------------------------------

% Dom: Cuatro integer y un pixbit
% Desc: Predicado que crea un pixbit
pixbit(X,Y,Bit,Depth,[X,Y,Bit,Depth]):-
    integer(X), integer(Y), integer(Bit), integer(Depth),
    between(0, 1, Bit).

% Dom: Seis integer y un pixrgb
% Desc: Predicado que crea un pixrgb
pixrgb(X,Y,R,G,B,Depth,[X,Y,[R,G,B],Depth]) :-
    integer(X), integer(Y), integer(R), integer(G), integer(B), integer(Depth),
    between(0, 255, R), between(0, 255, G), between(0, 255, B).

% Dom: Tres integer, un string y un pixhex
% Desc: Predicado que crea un pixhex
pixhex(X,Y,Hex,Depth,[X, Y, Hex, Depth]) :-
    integer(X), integer(Y), string(Hex), integer(Depth),
    string_length(Hex,7).

% Dom: Tres integer, un color (integer|list|string) y un pixel
% Desc: Predicado que crea un pixel de cualquier tipo
pixel(X,Y,Color,Depth,[X,Y,Color,Depth]) :-
    pixbit(X,Y,Color,Depth,[X,Y,Color,Depth]);
    pixhex(X,Y,Color,Depth,[X,Y,Color,Depth]);
	pixrgb(X,Y,_,_,_,Depth,[X,Y,Color,Depth]).