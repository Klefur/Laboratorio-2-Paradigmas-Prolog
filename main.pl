% -------------------------------------- PIXEL -------------------------------------------------

% -------------------------------------- CONSTRUCTORES -------------------------------------------------

% Dom: Cuatro integer y un pixbit-d
% Desc: Predicado que crea un pixbit-d
pixbit-d(X,Y,Bit,Depth,[X,Y,Bit,Depth]) :-
    integer(X), integer(Y), integer(Bit), integer(Depth),
    between(0, 1, Bit).

% Dom: Seis integer y un pixrgb-d
% Desc: Predicado que crea un pixrgb-d
pixrgb-d(X,Y,R,G,B,Depth,[X,Y,[R,G,B],Depth]) :-
    integer(X), integer(Y), integer(R), integer(G), integer(B), integer(Depth),
    between(0, 255, R), between(0, 255, G), between(0, 255, B).

% Dom: Tres integer, un string y un pixhex-d
% Desc: Predicado que crea un pixhex-d
pixhex-d(X,Y,Hex,Depth,[X, Y, Hex, Depth]) :-
    integer(X), integer(Y), string(Hex), integer(Depth),
    string_length(Hex,7).

% Dom: Tres integer, un color (integer|list|string) y un pixel
% Desc: Predicado que crea un pixel de cualquier tipo
pixel(X,Y,Color,Depth,[X,Y,Color,Depth]) :-
    pixbit-d(X,Y,Color,Depth,[X,Y,Color,Depth]);
    pixhex-d(X,Y,Color,Depth,[X,Y,Color,Depth]);
	pixrgb-d(X,Y,_,_,_,Depth,[X,Y,Color,Depth]).

% -------------------------------------- IMAGEN -------------------------------------------------

% -------------------------------------- CONSTRUCTOR -------------------------------------------------

% Dom: Dos integer, una lista de pixeles y una image
% Desc: Predicado que crea una imagen
image(Width,Height,Pixs,[Width,Height,Pixs,CompressColor]):-
    string(CompressColor);integer(CompressColor);is_list(CompressColor); CompressColor is -1.

% -------------------------------------- SELECTOR -------------------------------------------------

% Dom: Una image y un integer
% Desc: Predicado que entrega el color comprimido en una imagen
getCompressColor([Width,Height,Pixs,CompressedColor],CompressedColor):-
    image(_,_,_,[Width,Height,Pixs,CompressedColor]).

% -------------------------------------- MODIFICADOR -------------------------------------------------

% Dom: Una image y un integer
% Desc: Predicado que cambia el color comprimido en una imagen
setCompressColor([Width,Height,Pixs,CompressColor], X, [Width,Height,Pixs,X]) :-
    image(_,_,_,[Width,Height,Pixs,CompressColor]), image(_,_,_,[Width,Height,Pixs,X]).

% -------------------------------------- PERTENENCIA -------------------------------------------------

% Dom: Una image
% Desc: Predicado que determina si una imagen tiene pixeles pixbit-d
imageIsBitmap([_,_,[],_]).
imageIsBitmap([_,_,[H|T],_]):-
    pixbit-d(_,_,_,_,H),
    imageIsBitmap([_,_,T,_]).

% Dom: Una image
% Desc: Predicado que determina si una imagen tiene pixeles pixrgb-d
imageIsPixmap([_,_,[],_]).
imageIsPixmap([_,_,[H|T],_]):-
    pixrgb-d(_,_,_,_,_,_,H),
    imageIsPixmap([_,_,T,_]).

% Dom: Una image
% Desc: Predicado que determina si una imagen tiene pixeles pixhex-d
imageIsHexmap([_,_,[],_]).
imageIsHexmap([_,_,[H|T],_]):-
    pixhex-d(_,_,_,_,H),
    imageIsHexmap([_,_,T,_]).

% Dom: Una image
% Desc: Predicado que determina si una imagen tiene pixeles homogeneos 
imagen(I):-
    imageIsBitmap(I);
    imageIsPixmap(I);
    imageIsHexmap(I).

% Dom: Una image
% Desc: Predicado que determina si una imagen esta comprimida
imageIsCompressed([Width,Height,Pixs,CompressColor]):-
    CompressColor \== -1, image(_,_,_,[Height,Width,Pixs,CompressColor]).

% -------------------------------------- OTRAS FUNCIONES -------------------------------------------------

% Dom: Dos image
% Desc: Predicado que voltea los pixeles verticalmente de una imagen
imageFlipV(I,I2):-
    imagen(I), image(Width,Height,Pixs,I),
    not(imageIsCompressed(I)),
    flipPixsV(Pixs,Height,P2),
    sortPixs(P2,0,0,Width,Height,SortP),
    image(Height,Width,SortP,I2).

% Dom: Dos listas de pixeles y un integer
% Desc: Predicado que voltea la posicion en Y de cada pixel
flipPixsV([],_,[]).
flipPixsV([H|T],Height,[H2|T2]):-
    pixel(X,Y,Color,Depth,H),
    Y2 is Height - 1 - Y,
    pixel(X,Y2,Color,Depth,H2),
    flipPixs(T,Height,T2).

% Dom: Dos image
% Desc: Predicado que voltea los pixeles horizontalmente de una imagen
imageFlipH(I,I2):-
    imagen(I), image(Width,Height,Pixs,I),
    not(imageIsCompressed(I)),
    flipPixsH(Pixs,Width,P2),
    sortPixs(P2,0,0,Width,Height,SortP),
    image(Height,Width,SortP,I2).

% Dom: Dos listas de pixeles y un integer
% Desc: Predicado que voltea la posicion en X de cada pixel
flipPixsH([],_,[]).
flipPixsH([H|T],Width,[H2|T2]):-
    pixel(X,Y,Color,Depth,H),
    X2 is Width - 1 - X,
    pixel(X2,Y,Color,Depth,H2),
    flipPixsH(T,Width,T2).

% Dom: Dos listas de pixeles y cuatro integer
% Desc: Predicado que ordena los pixeles desde el X
sortPixs(_,_,Y,_,Height,[]):-
    Y == Height.
sortPixs(Pixs,X,Y,Width,Height,T2):-
    X == Width,
    Y1 is Y + 1,
    sortPixs(Pixs,0,Y1,Width,Height,T2).
sortPixs(Pixs,X,Y,Width,Height,[[X,Y,Color,Depth]|T2]):-
    X < Width,
    member([X,Y,Color,Depth],Pixs),
    X1 is X + 1,
    sortPixs(Pixs,X1,Y,Width,Height,T2).


% Dom: Dos image, cuatro integer
% Desc: Predicado que recorta una imagen dados un punto de inicio x0, y0 hasta un x1, y1
imageCrop(I,X0,Y0,X1,Y1,I2) :-
    imagen(I), image(_,_,Pixs,I),
    not(imageIsCompressed(I)),
    cropPixs(Pixs,X0,Y0,X1,Y1,P2),
    Width2 is X1 + 1 - X0, Height2 is Y1 + 1 - Y0,
    image(Width2,Height2,P2,I2).

% Dom: Dos listas de pixeles y cuatro integer
% Desc: Predicado que elimina pixeles de una lista de pixeles dado un punto de inicio y un punto de fin 
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

% Dom: Dos image
% Desc: Predicado que convierte una imagen con pixeles rgb a pixeles hex
imageRGBToHex(I, I2):-
    imagen(I), image(Width,Height,Pixs,I),
    not(imageIsCompressed(I)),
    rgbToHexPixs(Pixs,P2),
    image(Width,Height,P2,I2).

% Dom: Dos listas de pixeles
% Desc: Predicado que transforma cada pixel rgb en pixel hex
rgbToHexPixs([],[]).
rgbToHexPixs([H|T],[H2|T2]):-
    pixrgb-d(X,Y,R,G,B,Depth,H),
    hex_bytes(Hex,[R,G,B]),string_upper(Hex,HexUp),
    string_concat("#",HexUp,Hex2),
    pixhex-d(X,Y,Hex2,Depth,H2),
    rgbToHexPixs(T,T2).

% Dom: Una image y una lista
% Desc: Predicado que cuenta los colores de una imagen creando una lista histograma
imageToHistogram(I, Histogram):-
    imagen(I), image(_,_,Pixs,I),
    not(imageIsCompressed(I)),
	genHistogram(Pixs,Histogram).

% Dom: Dos listas
% Desc: Predicado que construye la lista histograma
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

% Dom: Dos listas y un elemento
% Desc: Predicado que agrega un elemento al final de una lista
myAppend([],E,[E]).
myAppend([H],E,[H,E]).
myAppend([H|T],E,[H|T2]):-
    myAppend(T,E,T2).

% Dom: Dos listas y un elemento perteneciente a la lista
% Desc: Predicado que agrega uno al contador donde se encuentra el elemento
add1([],_,[]).
add1([[E,C]|T],E,[[E,C2]|T]):-
    E2 == E,
    C2 is C + 1.
add1([[E,C]|T],E2,[[E,C]|T2]):-
    add1(T,E2,T2).

% Dom: Dos listas y un elemento
% Desc: Predicado que busca el elemento mas repetido en una lista de listas
getMayor(M,[M,_],[]).
getMayor(M,[_,C],[[H,C2]|T]):-
    C2 >= C,
    getMayor(M,[H,C2],T).
getMayor(M,[Y,C],[[_,C2]|T]):-
    C2 =< C,
    getMayor(M,[Y,C],T).

% Dom: Dos image
% Desc: Predicado que rota los pixeles en 90° en sentido horario de una imagen
imageRotate90(I, I2):-
    imagen(I), image(Width,Height,Pixs,I),
    not(imageIsCompressed(I)),
    rotatePixs(Pixs,Height,P2),
    sortPixs(P2,0,0,Width,Height,SortP),
    image(Height,Width,SortP,I2).

% Dom: Dos listas y un entero
% Desc: Predicado que rota las posiciones de los pixeles en 90° en sentido horario
rotatePixs([],_,[]).
rotatePixs([H|T],Height,[H2|T2]):-
    pixel(X,Y,Color,Depth,H),
    Y2 is Height - 1 - Y,
    pixel(Y2,X,Color,Depth,H2),
    rotatePixs(T,Height,T2).

% Dom: Dos image
% Desc: Predicado que comprime los pixeles de una imagen, sacando el color mas repetido
imageCompress(I,I3):-
    imagen(I), image(Width,Height,Pixs,I),
    not(imageIsCompressed(I)),
    imageToHistogram(I, Histo),
    getMayor(M,[-1,-1],Histo),
    compressPixs(Pixs,M,P2),
    image(Width,Height,P2,I2),
    setCompressColor(I2,M,I3).

% Dom: Dos listas y un color
% Desc: Predicado que elimina los pixeles de una lista de pixeles que tengan el mismo color que el entregado
compressPixs([],_,[]).
compressPixs([H|T],M,[H|T2]):-
    pixel(_,_,Color,_,H),
    Color \== M,
    compressPixs(T,M,T2).
compressPixs([_|T],M,T2):-
    compressPixs(T,M,T2).

% Dom: Dos image y un pixel
% Desc: Predicado que cambia un pixel en especifico de una imagen
imageChangePixel(I,P2Mod,I2):-
    (imageIsBitmap(I),pixbit-d(_,_,_,_,P2Mod);
    imageIsPixmap(I),pixrgb-d(_,_,_,_,P2Mod);
    imageIsHexmap(I),pixhex-d(_,_,_,_,P2Mod)),
    image(Width,Height,Pixs,I),
    not(imageIsCompressed(I)),
    changePixs(Pixs,P2Mod,Pixs2),
    image(Width,Height,Pixs2,I2).

% Dom: Dos listas y un pixel
% Desc: Predicado cambia un pixel de la lista de pixeles por uno entregado
changePixs([H|T],PMod,[PMod|T]):-
    pixel(X,Y,_,_,H),
    pixel(X,Y,_,_,PMod).
changePixs([H|T],PMod,[H|T2]):-
    changePixs(T,PMod,T2).

% Dom: Dos image
% Desc: Predicado que invierte el color de los pixeles rgb de una imagen
imageInvertColorRGB(I,I2) :-
    imagen(I), image(Width,Height,Pixs,I),
    not(imageIsCompressed(I)),
    inverColorPixsRGB(Pixs,P2),
    image(Width,Height,P2,I2).

% Dom: Dos listas
% Desc: Predicado invierte los colores rgb de una lista de pixeles
inverColorPixsRGB([],[]).
inverColorPixsRGB([H|T],[H2|T2]) :-
    pixrgb-d(X,Y,R,G,B,Depth,H),
    R2 is 255 - R,G2 is 255 - G,B2 is 255 - B,
    pixrgb-d(X,Y,R2,G2,B2,Depth,H2),
    inverColorPixsRGB(T,T2).

% Dom: Una image y un string
% Desc: Predicado que transforma el color de los pixeles de una imagen en un string
imageToString(I,String):-
    imagen(I), image(Width,Height,Pixs,I),
    not(imageIsCompressed(I)),
    (imageIsBitmap(I),bitToString(Pixs,0,0,Width,Height,String);
    imageIsPixmap(I),rgbToString(Pixs,0,0,Width,Height,String);
    imageIsHexmap(I),hexToString(Pixs,0,0,Width,Height,String)).

% Dom: Una lista, cuatro integer y un string
% Desc: Predicado que transforma una lista de pixbit-d a string
bitToString(_,_,Y,_,Height,""):-
    Height == Y.
bitToString(Pixs,X,Y,Width,Height,Str3):-
    X < Width,member([X,Y,Color,_],Pixs),
    string_concat(Color," ",Str),
    X1 is X + 1,
    bitToString(Pixs,X1,Y,Width,Height,Str2),
    string_concat(Str,Str2,Str3).
bitToString(Pixs,X,Y,Width,Height,Str3):-
    X == Width,Y1 is Y + 1,
    bitToString(Pixs,0,Y1,Width,Height,Str2),
    string_concat("\n",Str2,Str3).

% Dom: Una lista, cuatro integer y un string
% Desc: Predicado que transforma una lista de pixrgb-d a string
rgbToString(_,_,Y,_,Height,""):-
    Height == Y.
rgbToString(Pixs,X,Y,Width,Height,Str3):-
    X < Width,member([X,Y,Color,_],Pixs),
    pixrgb-d(_,_,R,G,B,_,[X,Y,Color,0]),
    string_concat(R,",",StrR),
    string_concat(G,",",StrG),
    string_concat(B," ",StrB),
    string_concat(StrR,StrG,StrRG),string_concat(StrRG,StrB,Str),
    X1 is X + 1,
    rgbToString(Pixs,X1,Y,Width,Height,Str2),
    string_concat(Str,Str2,Str3).
rgbToString(Pixs,X,Y,Width,Height,Str3):-
    X == Width,Y1 is Y + 1,
    rgbToString(Pixs,0,Y1,Width,Height,Str2),
    string_concat("\n",Str2,Str3).

% Dom: Una lista, cuatro integer y un string
% Desc: Predicado que transforma una lista de pixhex-d a string
hexToString(_,_,Y,_,Height,""):-
    Height == Y.
hexToString(Pixs,X,Y,Width,Height,Str3):-
    X < Width,member([X,Y,Color,_],Pixs),
    string_concat(Color," ",Str),
    X1 is X + 1,
    hexToString(Pixs,X1,Y,Width,Height,Str2),
    string_concat(Str,Str2,Str3).
hexToString(Pixs,X,Y,Width,Height,Str3):-
    X == Width,Y1 is Y + 1,
    hexToString(Pixs,0,Y1,Width,Height,Str2),
    string_concat("\n",Str2,Str3).

% Dom: Una image y una lista de image
% Desc: Predicado que recibe una imagen y crea una imagen por cada profundidad diferente
imageDepthLayers(I, ImageList):-
    imagen(I), image(Width,Height,Pixs,I),
    not(imageIsCompressed(I)),
	genDepthogram(Pixs,Depthogram),
    (imageIsBitmap(I),rellenarImagenesBit(Depthogram,Width,Height,ImageList);
    imageIsHexmap(I),rellenarImagenesHex(Depthogram,Width,Height,ImageList);
    imageIsPixmap(I),rellenarImagenesRGB(Depthogram,Width,Height,ImageList)).

% Dom: Una image y una lista
% Desc: Predicado que cuenta los colores de una imagen creando una lista depthograma
genDepthogram([],[]).
genDepthogram([H|T],H2):-
    genDepthogram(T,T2),
    pixel(_,_,_,Depth,H),
    not(member([[_,_,_,Depth]|_],T2)),
    myAppend(T2,[H],H2).
genDepthogram([H|T],H2):-
    genDepthogram(T,T2),
    pixel(_,_,_,Depth,H),
    member([[_,_,_,Depth]|_],T2),
    addPix(T2,H,H2).

% Dom: Dos listas y un pixel
% Desc: Predicado que agrega un pixel a la lista de pixeles con misma profundidad
addPix([],_,[]).
addPix([[H|Ts]|T],Pix,[[H|H2]|T]):-
	pixel(_,_,_,Depth,H),pixel(_,_,_,Depth,Pix),
    myAppend(Ts,Pix,H2).
addPix([H|T],Pix,[H|T2]):-
    addPix(T,Pix,T2).

% Dom: Dos listas y dos integer
% Desc: Predicado que rellena los pixeles faltantes de un imagen bitmap con color blanco
rellenarImagenesBit([],_,_,[]).
rellenarImagenesBit([[H|Ts]|T],Width,Height,[H2|T2]):-
    pixel(_,_,_,Depth,H),
    rellenarPixs([H|Ts],0,0,Width,Height,1,Depth,P2),
    image(Width,Height,P2,H2),
    rellenarImagenesBit(T,Width,Height,T2).

% Dom: Dos listas y dos integer
% Desc: Predicado que rellena los pixeles faltantes de un imagen pixmap con color blanco
rellenarImagenesRGB([],_,_,[]).
rellenarImagenesRGB([[H|Ts]|T],Width,Height,[H2|T2]):-
    pixel(_,_,_,Depth,H),
    rellenarPixs([H|Ts],0,0,Width,Height,[255,255,255],Depth,P2),
    image(Width,Height,P2,H2),
    rellenarImagenesRGB(T,Width,Height,T2).

% Dom: Dos listas y dos integer
% Desc: Predicado que rellena los pixeles faltantes de un imagen hexmap con color blanco
rellenarImagenesHex([],_,_,[]).
rellenarImagenesHex([[H|Ts]|T],Width,Height,[H2|T2]):-
    pixel(_,_,_,Depth,H),
    rellenarPixs([H|Ts],0,0,Width,Height,"#FFFFFF",Depth,P2),
    image(Width,Height,P2,H2),
    rellenarImagenesHex(T,Width,Height,T2).

% Dom: Dos listas y cinco integer y un color
% Desc: Predicado que rellena una lista de pixeles hasta tener el ancho y alto indicado con un color y profundidad entregadas
rellenarPixs(_,_,Y,_,Height,_,_,[]):-
    Y == Height.
rellenarPixs(Pixs,X,Y,Width,Height,Color,Depth,T2):-
    X == Width,
    Y1 is Y + 1,
    rellenarPixs(Pixs,0,Y1,Width,Height,Color,Depth,T2).
rellenarPixs(Pixs,X,Y,Width,Height,Color,Depth,[[X,Y,Color2,Depth]|T2]):-
    X < Width,
    member([X,Y,Color2,Depth],Pixs),
    X1 is X + 1,
    rellenarPixs(Pixs,X1,Y,Width,Height,Color,Depth,T2).
rellenarPixs(Pixs,X,Y,Width,Height,Color,Depth,[H2|T2]):-
    X < Width,
    pixel(X,Y,Color,Depth,H2),
    X1 is X + 1,
    rellenarPixs(Pixs,X1,Y,Width,Height,Color,Depth,T2).

% Dom: Dos image
% Desc: Predicado que recibe una imagen comprimida y entrega la imagen descomprimida
imageDecompress(I,I2):-
    imagen(I), image(Width,Height,Pixs,I),
    imageIsCompressed(I),getCompressColor(I,CompressedColor),
    rellenarPixs(Pixs,0,0,Width,Height,CompressedColor,10,P2),
    image(Width,Height,P2,I2).

% -------------------------------------- PRUEBAS -------------------------------------------------

img1(I) :-
    pixbit-d(0, 0, 1, 10, P1),
    pixbit-d(0, 1, 0, 12, P2),
    pixbit-d(1, 0, 0, 10, P3),
    pixbit-d(1, 1, 1, 12, P4), 
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
    pixhex-d(1, 1, "#77FFCC", 12, P4), 
    image(2, 2, [P1, P2, P3, P4], I).