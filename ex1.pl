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
        v(p(9),[1,2,3,4,5,6,7,8,9],_)],[])).


linhas([1,2,3]).
linhas([4,5,6]).
linhas([7,8,9]).
colunas([1,4,7]).
colunas([2,5,8]).
colunas([3,6,9]).
diagonais([1,5,9]).
diagonais([3,5,7]).

/*
ve_restricoes( e([], [v(p(1),[1,2,3,4,5,6,7,8,9],1),
                v(p(2),[1,2,3,4,5,6,7,8,9],2),
                v(p(3),[1,2,3,4,5,6,7,8,9],3),
                v(p(4),[1,2,3,4,5,6,7,8,9],4),
                v(p(5),[1,2,3,4,5,6,7,8,9],5),
                v(p(6),[1,2,3,4,5,6,7,8,9],6),
                v(p(7),[1,2,3,4,5,6,7,8,9],7),
                v(p(8),[1,2,3,4,5,6,7,8,9],8),
                v(p(9),[1,2,3,4,5,6,7,8,9],9)
    ])).
*/
igual(X,X).

doSoma(_,[],0).
doSoma(LE, [H|T], S):- member(v(p(H),_,V), LE), doSoma(LE,T, S1), S is S1 + V.
ve_restricoes(e([],L)):- ve_repetidos(L), findall(LL, linhas(LL), [L1,L2,L3]), findall(C, colunas(C), [C1,C2,C3]), findall(D, diagonais(D), [D1,D2]),
doSoma(L,L1,SL1), doSoma(L,L2,SL2), doSoma(L,L3,SL3), doSoma(L,C1,SC1), doSoma(L,C2,SC2), doSoma(L,C3,SC3), doSoma(L,D1,SD1), doSoma(L,D2,SD2),
igual(SL1,SL2), igual(SL1,SL3),igual(SL1,SC1),igual(SL1,SC2),igual(SL1,SC3),igual(SL1,SD1),igual(SL1,SD2).

ve_restricoes(e(Ln1,L)):- \+igual(Ln1, []), ve_repetidos(L).

ve_repetidos([]).
ve_repetidos([v(_,_,V)|T]):- \+member(v(_,_,V),T), ve_repetidos(T).

escreve([v(p(3),_,V)|T]):- write(V), nl, write('---+---+---'),nl,write(' '),escreve(T).
escreve([v(p(6),_,V)|T]):- write(V), nl, write('---+---+---'),nl,write(' '),escreve(T).
escreve([v(p(9),_,V)|_]):- write(V), nl, write('Fim').
escreve([v(_,_,V)|T]):- write(V),write(' | '),escreve(T).



b:- estado_inicial(E0), back(E0,A),sort(A, L),write(' '),escreve(L).

back(e([],A),A).
back(E,Sol):- sucessor(E,E1), ve_restricoes(E1),
                          back(E1,Sol).

sucessor(e([v(N,D,V)|R],E),e(R,[v(N,D,V)|E])):- member(V,D).

:- dynamic(nos/1).

nos(0).

inc:- retract(nos(N)), N1 is N+1, asserta(nos(N1)).

  
back_f:- estado_inicial(E0), back(E0,A),sort(A, L),write(' '),escreve(L).

back(e([],A),A).
  back(E,Sol):- sucessor(E,E1), inc, ve_restricoes(E1),
                       forCheck(E1,E2),
                          back(E2,Sol).

sucessor(e([v(N,D,V)|R],E),e(R,[v(N,D,V)|E])):- member(V,D).