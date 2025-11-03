% SICStus PROLOG: definicoes iniciais
:- dynamic paciente/4. 
:- dynamic consulta/7.
:- dynamic condicao/3. % não precisamos (acho eu)
:- dynamic financeiro/4. % não precisamos (acho eu)
:- dynamic '-'/1.
:- op( 900,xfy,'::' ).

% -------------------------------- - - - - - - - - - -  -  -  -  -   -
% SICStus PROLOG: Declaracoes iniciais
:- set_prolog_flag( discontiguous_warnings,off ).
:- set_prolog_flag( single_var_warnings,off ).
:- set_prolog_flag( unknown,fail ).

% -------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado paciente: IdPaciente, Nome, Data de Nascimento, Idade, Sexo, Morada -> {V,F,D}

paciente(p1, 'Ana Martins', 15-09-1987, 38, feminino, '23 Avenida Central').
paciente(p2, 'Carlos Pereira', 27-11-2001, 23, masculino, '12 Rua das Flores').
paciente(p3, 'Joana Costa', 03-02-1995, 30, feminino, '5 Largo da Estação').
paciente(p4, 'Miguel Fernandes', 20-07-1978, 47, masculino, '8 Travessa do Sol').
paciente(p5, 'Rita Sousa', 11-12-1992, 32, feminino, '14 Rua das Amendoeiras').
% Conhecimento incerto
paciente(p6, 'Ana Franco', 13-05-1992, 32, feminino, morada_desconhecida).
excecao(paciente(Id, N, D, I, S, M)) :-
    paciente(Id, N, D, I, S, morada_desconhecida).


% Extensao do predicado consulta: IdConsulta, Data, IdPaciente, Idade, Diastolica, Sistolica, Pulsacao, Classifiacao -> {V,F,D}

consulta(c1, 02-02-2025, p1, 38, 70, 115, 70).
consulta(c2, 05-02-2025, p2, 23, 90, 130, 80).
consulta(c3, 10-02-2025, p3, 30, 85, 125, 78).
consulta(c4, 12-02-2025, p4, 47, 88, 140, 85).
consulta(c5, 14-02-2025, p5, 32, 76, 118, 72).

% Extensao do predicado tensao arterial: IdTA, Classifcacao, IdPaciente, Sistolica Inferior, Sistolica Superior, Diastolica Inferior, Diastolica Superior -> {V,F,D}

% VERIFICAR VALORES QUE É O CHAT QUE DEU E PODE TER PERCEBIDO MAL!!

ta(ta1, 'Normal-baixa', p1, 85, 110, 55, 75).
ta(ta2, 'Normal-alta', p2, 120, 129, 75, 84).
ta(ta3, 'Pré-hipertensão', p3, 130, 139, 85, 89).
ta(ta4, 'Hipertensão', p4, 140, 159, 90, 99).
ta(ta5, 'Normal', p5, 90, 120, 60, 80).

% -------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado teste: lista -> {V,F}
teste([]).
teste([H| T]):- H, teste(T).

% -------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado que permite a evolucao do conhecimento: Termo -> {V,F}
evolucao(Termo):-
    findall(Invariante, +Termo::Invariante, Lista),
    insercao(Termo),
    teste(Lista).

insercao(Termo) :- assert(Termo).
insercao(Termo) :- retract(Termo),!, fail.


% -------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado que permite a involucao do conhecimento: Termo -> {V,F}
involucao(Termo):-
    findall(Invariante, -Termo::Invariante, Lista),
    remocao(Termo),
    teste(Lista).

remocao(Termo) :- retract(Termo).
remocao(Termo) :- assert(Termo),!, fail.

% -------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do meta-predicado nao: Questao -> {V,F}
nao(Questao):- 
        Questao, !, fail.

nao(Questao).

% -------------------------------- - - - - - - - - - -  -  -  -  -   -
% Sistema de inferencia
si(Questao,verdadeiro):- Questao.

si(Questao, falso) :- -Questao.

si(Questao, desconhecido):-
    nao(Questao),
    nao(-Questao).

% Valores tabela de verdade conjucao para o sistema de inferencia

tabela_c(verdadeiro, verdadeiro, verdadeiro).
tabela_c(verdadeiro, falso, falso).
tabela_c(falso, verdadeiro, falso).
tabela_c(falso, falso, falso).
tabela_c(verdadeiro, desconhecido, desconhecido).
tabela_c(desconhecido, verdadeiro, desconhecido).
tabela_c(falso, desconhecido, falso).
tabela_c(desconhecido, falso, falso).
tabela_c(desconhecido, desconhecido, desconhecido).

%Nossa---------------------------------------------------------------
si_conjuncao(Q1, Q2, Resultado) :-
    si(Q1, R1),
    si(Q2, R2),
    tabela(R1, R2, Resultado). 
 
%Stor---------------------------------------------------------------   
sic(Q1 e Q2, R) :-
    sic(Q1, R1),
    sic(Q2, R2),
    conjuncaoo(R1, R2, R). 
 
conjuncao(C1, C2, (C1 e C2)).

siR(Q1 e Q2, CR,R) :- %????????
    siR(Q1, CR1, R1),
    siR(Q2, CR2, R2),
    conjuncao(CR1, CR2, CR),
    composicao(CR, R).

% Valores tabela de verdade disjuncao para o sistema de inferencia

tabela_d(verdadeiro, verdadeiro, verdadeiro).
tabela_d(verdadeiro, falso,    verdadeiro).
tabela_d(falso,    verdadeiro, verdadeiro).
tabela_d(falso,    falso,      falso).
tabela_d(verdadeiro, desconhecido, verdadeiro).
tabela_d(desconhecido, verdadeiro,  verdadeiro).
tabela_d(falso,    desconhecido, desconhecido).
tabela_d(desconhecido, falso,     desconhecido).
tabela_d(desconhecido, desconhecido, desconhecido).

%Nossa---------------------------------------------------------------
si_disjuncao(Q1, Q2, Resultado) :-
    si(Q1, R1),
    si(Q2, R2),
    tabela_disjuncao(R1, R2, Resultado).

%Stor---------------------------------------------------------------   
sic(Q1 ou Q2, R) :-
    sic(Q1, R1),
    sic(Q2, R2),
    dijuncao(R1, R2, R). 
    
dijuncao(C1, C2, (C1 ou C2)).

siR(Q1 ou Q2, CR,R) :-
    siR(Q1, CR1, R1),
    siR(Q2, CR2, R2),
    disjuncao(CR1, CR2, CR),
    composicao(CR, R).
% -------------------------------- - - - - - - - - - -  -  -  -  -   -
siR(Q,R,R):-
    si(Q,R).

composicao( C1 e C2,C ) :-
    aand( C1,C2,C ).
composicao( C1 ou C2,C ) :-
    oor( C1,C2,C ).

aand( verdadeiro,Valor,Valor ) :- !.
aand( Valor,verdadeiro,Valor ) :- !.
aand( desconhecido,desconhecido,desconhecido ).
aand( falso,Valor,falso ) :- !.
aand( Valor,falso,falso ).

oor( verdadeiro,Valor,verdadeiro ) :- !.
oor( Valor,verdadeiro,verdadeiro ) :- !.
oor( desconhecido,Valor,desconhecido ) :- !.
oor( Valor,desconhecido,desconhecido ).
oor( falso,falso,falso ).

% ...

