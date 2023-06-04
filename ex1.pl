estado_inicial(e(
    [
        v(p(1),[1,2,3,4,5,6,7,8,9],_),
        v(p(2),[1,2,3,4,5,6,7,8,9],_),
        v(p(3),[1,2,3,4,5,6,7,8,9],_),
        v(p(4),[1,2,3,4,5,6,7,8,9],_),
        v(p(5),[1,2,3,4,5,6,7,8,9],_),
        v(p(6),[1,2,3,4,5,6,7,8,9],_),
        v(p(7),[1,2,3,4,5,6,7,8,9],_),
        v(p(8),[1,2,3,4,5,6,7,8,9],_),
        v(p(9),[1,2,3,4,5,6,7,8,9],_)
    ],[]
    )).
ve_restricoes(e(_,[])).
ve_restricoes(e([],
    [
        v(p(1),_,V1),
        v(p(2),_,V2),
        v(p(3),_,V3),
        v(p(4),_,V4),
        v(p(5),_,V5),
        v(p(6),_,V6),
        v(p(7),_,V7),
        v(p(8),_,V8),
        v(p(9),_,V9) 
        ])):-
    SomaLinha1 is V1+V2+V3, SomaLinha2 is V4+V5+V6, SomaLinha3 is V7+V8+V9,
    SomaCol1 is V1+V4+V7, SomaCol2 is V2+V5+V8, SomaCol3 is V3+V6+V9,
    SomaDig1 is V1+V6+V9, SomaDig2 is V3+V5+V7,
    SomaLinha1 = SomaLinha2, SomaLinha1 = SomaLinha3,
    SomaLinha1 = SomaCol1, SomaLinha1 = SomaCol2, SomaLinha1 = SomaCol3,
    SomaLinha1 = SomaDig1.
    
ve_restricoes(e(_,[v(_,_,V)|T])):- \+member(v(_,_,V),T),ve_restricoes(e(_,T)).

escreve([v(p(3),_,V)|T]):- write(V), nl, write('--+---+--'),nl,escreve(T).
escreve([v(p(6),_,V)|T]):- write(V), nl, write('--+---+--'),nl,escreve(T).
escreve([v(p(9),_,V)|_]):- write(V), nl, write('Fim').
escreve([v(_,_,V)|T]):- write(V),write(' | '),escreve(T).

p:- estado_inicial(E0), back(E0,A),sort(A, L),escreve(L).

back(e([],A),A).
back(E,Sol):- sucessor(E,E1), ve_restricoes(E1),
                          back(E1,Sol).

sucessor(e([v(N,D,V)|R],E),e(R,[v(N,D,V)|E])):- member(V,D).