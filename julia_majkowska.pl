% Definiujemy moduł zawierający rozwiązanie.
% Należy zmienić nazwę modułu na {imie}_{nazwisko} gdzie za
% {imie} i {nazwisko} należy podstawić odpowiednio swoje imię
% i nazwisko bez wielkich liter oraz znaków diakrytycznych
:- module(julia_majkowska, [parse/3]).

% Główny predykat rozwiązujący zadanie.
% UWAGA: to nie jest jeszcze rozwiązanie; należy zmienić jego
% definicję.
parse(_Path, Codes, Program) :-
    make_lista(_Path, Codes, Leksowane), 
    phrase(program(Program), Leksowane).
    
  
/*Lekser */

nawias(Kod) :- char_code(X, Kod), ( X = '(' ; X = '[' ;X  = ']'; X = ')').
znak_bialy(X) :- (13 >= X, X >=9).
znak_bialy(X) :- char_code(' ', X).
przystanek(X) :- znak_bialy(X).
cyfra(X) :- X >= "0" ,  "9">= X.
podkreslnik(X) :- char_code('_', X).
apostrof(X) :- char_code('\'', X).
mala_litera(X) :- X >= "a" , "z" >= X.
duza_litera(X) :- X >= "A" , "Z" >= X.
%nowa_linia(X) :- string_to_list("\n", [X]).
nowa_linia(10).
litera(X) :- mala_litera(X) ; duza_litera(X).

key("def").
key("else").
key("_").
key("if").
key("in").
key("let").
key("then").

ident([H|T],  1, [identyfikator, H|Slowo], Ak, Dl, T1) :- 
    !,
    (litera(H) ; podkreslnik(H)), 
    %char_code(H1, H)
    Ak1 is Ak +1,
    ident(T, 0, Slowo, Ak1, Dl, T1).

ident([H|T], 0, [H|Slowo], Ak, Dl, T1) :-
    (litera(H); podkreslnik(H); cyfra(H); apostrof(H)), 
    !,
    Ak1 is Ak +1,
    ident(T, 0, Slowo,Ak1, Dl, T1).
    
ident(T, 0,[], Dl, Dl, T).

operator([A,B|T], [operator,'<>'], 2, T) :- string_to_list("<>", [A,B]), !.
operator([A,B|T], [operator,'<='], 2, T) :- string_to_list("<=", [A,B]), !.
operator([A,B|T], [operator,'>='], 2, T) :- string_to_list(">=", [A,B]), !.
operator([A,B|T], [operator,'..'], 2, T) :- string_to_list("..", [A,B]), !.
operator([Kod|T], [operator,X], 1, T) :-char_code(X, Kod), ( X = ','; X = '='; X = '<'; X = '>';X = '^'; X = '|' ; X = '+'; X = '-'; X = '&'; X = '*'; X = '/'; X = '%'; X = '@';X = '#'; X ='~').

liczba([H|T], [H|Slowo], Ak, Dl, T1) :- 
    cyfra(H), 
    !,
    Ak1 is Ak +1,
    liczba(T, Slowo,Ak1, Dl, T1).
liczba(T, [], Dl, Dl, T).

komentarz([A,B|T], 1, Ak, Dl, AkL, L, AkOst, Ost, T1) :- 
    string_to_list("(*", [A,B]), !,
    Ak1 is Ak +2, 
    AkOst1 is AkOst +2,
    komentarz(T, 0, Ak1, Dl, AkL, L,AkOst1, Ost, T1).

komentarz([A,B|T], 0, Ak, Dl, AkL, AkL, AkOst, Ost, T) :- string_to_list("*)", [A,B]), !, Dl is Ak+2, Ost is AkOst+2.

komentarz(Lista, 0, Ak, Dl, AkL, L, AkOst, Ost,T1) :- 
    komentarz(Lista, 1,Ak, Dl1, AkL, L1, AkOst, Ost1, T),!, 
    komentarz(T, 0, Dl1, Dl,L1, L, Ost1, Ost, T1).
    
komentarz([H|T], 0, Ak, Dl, AkL, L, _, Ost, T1) :- nowa_linia(H), !, Ak1 is Ak +1, AkL1 is AkL+1, komentarz(T, 0, Ak1, Dl, AkL1, L, 1, Ost, T1).
komentarz([_|T], 0, Ak, Dl, AkL, L, AkOst, Ost, T1) :- Ak1 is Ak +1, AkOst1 is AkOst+1,  komentarz(T, 0, Ak1, Dl, AkL, L, AkOst1, Ost, T1).

zamien_na_string([H|T], [H, S]) :- string_to_list(S, T).
dodaj_pozycje(Sparsowane, Path, Line, LinePos, CharNo, Dl, [file(Path, Line, LinePos, CharNo)|Sparsowane]).
    
%slowa(Wejscie, Path, Line, LinePos, CharNo, Sparsowane)
slowa([],_,_,_,_,[]) :-!.

slowa([H|T], Path,Line, _, CharNo, Slowoteraz) :-
    nowa_linia(H), !,
    Line1 is Line+1,
    CharNo1 is CharNo +1,
    slowa(T,Path, Line1, 1, CharNo1, Slowoteraz).
    
slowa([A,B|T1],Path,Line, LinePos, CharNo, Slowateraz) :-
    string_to_list("(*", [A,B]),
    !,
    komentarz([A,B|T1], 1,CharNo, Dl, Line, L, LinePos, Ost, T), 
    slowa(T,Path, L, Ost, Dl, Slowateraz).
    
slowa([H|T],Path,Line, LinePos, CharNo, T1) :-
    (znak_bialy(H); przystanek(H)),
    !,
    CharNo1 is CharNo +1,
    LinePos1 is LinePos +1,
    slowa(T, Path, Line, LinePos1, CharNo1, T1).
    
slowa(Lista, Path, Line, LinePos, CharNo, [Slowoteraz|T1]) :-
    operator(Lista, Slowoteraz1, Dl,T), !, 
    dodaj_pozycje(Slowoteraz1, Path, Line, LinePos, CharNo, Dl, Slowoteraz),
    LinePos1 is LinePos +Dl, 
    CharNo1 is CharNo +Dl, 
    slowa(T, Path, Line, LinePos1, CharNo1,T1).
    
slowa([H|T],Path, Line, LinePos, CharNo,[Slowo|T1]) :- 
    nawias(H), 
    string_to_list(H1, [H]), 
    dodaj_pozycje([nawias,H1], Path, Line, LinePos, CharNo, 1, Slowo),
    LinePos1 is LinePos +1,
    CharNo1 is CharNo +1,
    slowa(T,Path, Line, LinePos1, CharNo1,T1). 

slowa(Lista, Path, Line, LinePos, CharNo, [Slowoteraz1|T1]) :-
    ident(Lista, 1, Slowoteraz, 0, Dl, T),!, zamien_na_string(Slowoteraz, [H,S]),
    ((key(S), !, Slowoteraz2 = [klucz, S]); Slowoteraz2 = [H|S]),
    dodaj_pozycje(Slowoteraz2, Path, Line, LinePos, CharNo, Dl, Slowoteraz1),
    LinePos1 is LinePos +Dl,
    CharNo1 is CharNo +Dl,
    slowa(T,Path, Line, LinePos1, CharNo1, T1).
    
slowa(Lista, Path, Line, LinePos, CharNo, [Slowoteraz|T1]) :-
    liczba(Lista, Slowobeztyp, 0, Dl,  T),
    Slowobeztyp \= [], 
    number_codes(Dobraliczba, Slowobeztyp), 
    dodaj_pozycje([liczba|Dobraliczba], Path, Line, LinePos, CharNo, Dl, Slowoteraz),
    !, 
    LinePos1 is LinePos +Dl,
    CharNo1 is CharNo +Dl,
    slowa(T, Path, Line, LinePos1, CharNo1,T1).
    
make_lista(Path,L1, L) :-
    string_to_list(L1, L2), 
    slowa(L2,Path, 1, 1, 0, L).
    
    
/*Parser */

/* Pytania 
 czy operatory unarne są łączne? - tak ale niekonicznie
 czy .. jest poełnoprawnym operatorem binarnym - nie
 czy operatory też można dawać przy pomocy atom atom_codes - wziac dwa znaki i wsadzic w apostrofy
 
*/

liczpos( file(Path, Line, LinePos, CharNo), file(Path, _,_,_), file(Path, Line, LinePos, CharNo) ). 
program(X) --> definicje(X).
definicje([]) --> [].
definicje([H|T]) --> definicja(H) , definicje(T).
definicja(def(Name, P, E)) --> [[_,klucz, "def"]],identyfikator(_,Name), [[_,nawias, "("]], wzorce(P,_), [[_,nawias, ")"]], rownosc(_,_), wyrazenie(E,_).
wzorzec(wildcard(Pos), Pos) --> [[Pos,klucz, "_"]].
wzorzec(X, Pos) --> zmienna(X, Pos).
wzorzec(X, Pos) --> [[P1, nawias, "("]], wzorce(X, _), [[P2, nawias, ")"]],{liczpos(P1,P2, Pos)}.
wzorce(X, Pos) --> wzorzec(X1, Pos), (([], {X = X1}); ([[_,operator, ',']], wzorce(P2, _), {X = pair(Pos, X1, P2)})).
%wzorce(pair(Pos,P1, P2), Pos) --> wzorzec(P1, Pos), [[_,operator, ',']], wzorce(P2, _).
wyrazenie(if(Pos,E1, E2, E3), Pos) --> [[Pos,klucz, "if"]], wyrazenie(E1,_), [[_,klucz, "then"]], wyrazenie(E2,_), [[_,klucz, "else"]], wyrazenie(E3,_).
wyrazenie(let(Pos, P, E1, E2), Pos) --> [[Pos, klucz, "let"]],wzorce(P,_), rownosc(_,_), wyrazenie(E1, _), [[_,klucz, "in"]], wyrazenie(E2,_).
wyrazenie(X, Pos) --> wyrazenieOp(X, Pos).


wyrUn(X, P)  --> wyrazenieProste(X, P).
wyrUn(op(Pos, O, U),Pos) --> opUnarny(Pos,O),wyrUn(U, _).
/*czesc kodu z parsera z SKOS*/
wyrMult(Wynik, Pos) --> wyrUn(A, Pos),wyrMult(A, Wynik, Pos). % akumulator * X
wyrMult(Akumulator, Wynik, Pos) --> opMult(_,M), !, wyrUn(A, _), {Akum1 = op(Pos, M, Akumulator, A)}, wyrMult(Akum1, Wynik, Pos).
wyrMult(A, A, _) --> [].

wyrAdd(Wynik, Pos) --> wyrMult(A, Pos), wyrAdd(A, Wynik, Pos).
wyrAdd(Akumulator, Wynik, Pos) --> opAdd(_,M), !, wyrMult(A,_), {Akum1 = op(Pos, M, Akumulator, A)}, wyrAdd(Akum1, Wynik, Pos).
wyrAdd(A, A, _) --> [].

wyrMalp(X, P) --> wyrAdd(X1,P), (([], {X = X1}); (malpa(_,O), wyrMalp(B, _), {X = op(P, O, X1, B)})).
%wyrMalp(op(Pos, O, A, B),Pos) --> wyrAdd(A, Pos), malpa(_,O),!, wyrMalp(B, _).

wyrazeniePor(X,P) --> wyrMalp(X1,P), (([], {X = X1}); (porownanie(_,Por),wyrMalp(B, _), {X = op(P, Por, X1, B)})).
%wyrazeniePor(op(Pos, Por, A, B), Pos) --> wyrMalp(A, Pos), porownanie(_,Por),!,wyrMalp(B, _).

wyrazenieOp(X,P) --> wyrazeniePor(X1,P), (([], {X = X1}); ([[_,operator, ',']], wyrazenieOp(Y, _), {X = pair( P, X1, Y)})).
%wyrazenieOp(pair(Pos, X, Y),Pos) --> wyrazeniePor(X, Pos), [[_,operator, ',']],!, wyrazenieOp(Y, _).

wyrazeniePolProste(X, Pos) --> [[P1,nawias, "("]], wyrazenie(X,_), [[P2,nawias, ")"]], {liczpos(P1,P2, Pos)}.
wyrazeniePolProste(X, P) --> wyrazenieAtomowe(X,P).

%wyrazenieProste(X,Pos) --> [[P1,nawias, "("]], wyrazenie(X,_), [[P2,nawias, ")"]], {liczpos(P1,P2, Pos)}.
wyrazenieProste(X, Pos) --> wyborBitu(X, Pos).
%wyrazenieProste(X, Pos) --> wyrazenieAtomowe(X, Pos).


wyborBitu(Wynik, PosNawias) --> wyrazeniePolProste(A, PosNawias), wyborBitowy(A, Wynik, PosNawias).%(Aku[[nawias, "["]], wyrazenie, [[nawias, "]"]].
wyborBitowy(A, Wynik, PosNawias) --> [[_,nawias, "["]], wyrazenie(X, _), [[_,nawias, "]"]], !,{A1 = bitsel(PosNawias, A, X)}, wyborBitowy(A1, Wynik, PosNawias).
wyborBitowy(A, Wynik, PosNawias) --> [[_,nawias, "["]], wyrazenie(X, _), [[_,operator, '..']],!, wyrazenie(Y,_), [[_,nawias, "]"]], { A1 = bitsel(PosNawias, A, X, Y)}, wyborBitowy(A1, Wynik, PosNawias).
wyborBitowy(A, A, _) --> [].

wyrazenieAtomowe(X,PosNawias) --> zmienna(X, PosNawias).
wyrazenieAtomowe(X,PosNawias) --> wywolanieFunkcji(X,PosNawias).
wyrazenieAtomowe(X,PosNawias) --> literalCalkowitoliczbowy(X,PosNawias).
wyrazenieAtomowe(X,PosNawias) --> pustyWektor(X,PosNawias).
wyrazenieAtomowe(X,PosNawias) --> pojedynczyBit(X,PosNawias).

zmienna(var(Pos,X), Pos) --> identyfikator(Pos,X).
wywolanieFunkcji(call(Pos, Name,X), Pos) -->  identyfikator(Pos1, Name), [[_,nawias, "("]], wyrazenie(X, _), [[Pos2,nawias, ")"]], {liczpos(Pos1, Pos2, Pos)}.
pustyWektor(empty(Pos), Pos) --> [[Pos1,nawias, "["]],[[Pos2,nawias, "]"]], {liczpos(Pos1, Pos2, Pos)}.
pojedynczyBit(bit(PosNawias, X), PosNawias) --> [[Pos1,nawias, "["]], wyrazenie(X,_), [[Pos2,nawias,"]"]], {liczpos(Pos1, Pos2, PosNawias)}.

identyfikator(Pos,X1) --> [[Pos,identyfikator|X]], {atom_codes(X1, X)}.
literalCalkowitoliczbowy(num(Pos,X), Pos) --> [[Pos,liczba| X]].

opMult(Pos,X) --> [[Pos,operator, X]], {(X = '&';X= '*'; X = '/'; X = '%')}.
opAdd(Pos,X) --> [[Pos,operator, X]], {(X = '|'; X = '^'; X = '+'; X = '-')}.
rownosc(Pos,'=') --> [[Pos,operator, '=']].
malpa(Pos,'@') --> [[Pos,operator, '@']].
porownanie(Pos,X) --> [[Pos,operator, X]], {(X = '='; X = '<>'; X = '<'; X = '>'; X = '<='; X= '>=')}.
opUnarny(Pos,X) --> [[Pos,operator, X]], {(X = '-'; X = '#'; X = '~')}.
