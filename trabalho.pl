% SICStus PROLOG: Definicoes iniciais
:- dynamic paciente/6. 
:- dynamic consulta/7.
:- dynamic ta/7.
:- op( 900,xfy,'::' ).
:- dynamic '-'/1.
:- op(500, xfy, e).
:- op(500, xfy, ou).
:- use_module(library(listing)).

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
paciente(p8, 'Anabela Martins', 01-05-2005, 20, feminino, '22 Rua da Pedreira').
paciente(p9, 'José Antunes', 11-07-2004, 21, masculino, '102 Rua doss Chãos').
paciente(p11, 'Raquel Freitas', 13-07-2004, 21, feminino, '5 rua do Poente').

% Conhecimento Imperfeito Incerto - - - - - - - - - - - - - - - - - - 

% Morada do paciente desconhecida
paciente(p6, 'Ana Franco', 13-05-1992, 32, feminino, morada_desconhecida).
excecao(paciente(Id, N, D, I, S, M)) :-
    paciente(Id, N, D, I, S, morada_desconhecida).
    
% Nome do paciente desconhecida
paciente(p7, nome_desconhecido, 10-01-1990, 35, masculino, 'Rua do Norte').
excecao(paciente(Id, N, D, I, S, M)) :-
    paciente(Id, nome_desconhecido, D, I, S, M).

% Conhecimento Imperfeito Impreciso - - - - - - - - - - - - - - - - - - 

% Nome do paciente desconhecido entre duas possibilidades
excecao(paciente(p10, 'Ana Antunes', 12-07-2004, 21, feminino, '5 Largo da Estação')).
excecao(paciente(p10, 'Joana Leite', 12-07-2004, 21, feminino, '5 Largo da Estação')).

%Conhecimento Imperfeito Interdito  - - - - - - - - - - - - - - - - - - 

% Morada do paciente interdita
paciente(p2, 'Carlos Pereira', 27-11-2001, 23, masculino, morada_interdita).
excecao( paciente(Id, N, D, I, S, M)) :- 
    paciente(Id, N, D, I, S, morada_interdita).
interdito(morada_interdita).
+paciente(Id, N, D, I, S, M) :: (findall( (Id, N, D, I, S, Ms),(paciente(p2, 'Carlos Pereira', 27-11-2001, 23, masculino, Ms),nao(interdito(Ms))),S ),
                  length( S,N ), N == 0).

% -------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado consulta: IdConsulta, Data, IdPaciente, Idade, Diastolica, Sistolica, Pulsacao -> {V,F,D}
consulta(c1, 02-02-2025, p1, 38, 70, 115, 70).
consulta(c3, 10-02-2025, p3, 30, 88, 134, 78).
consulta(c5, 14-02-2025, p5, 32, 74, 118, 72).
consulta(c6, 20-02-2025, p6, 32, 80, 120, 75).
consulta(c7, 22-02-2025, p7, 35, 87, 133, 70).

% Conhecimento Imperfeito Incerto - - - - - - - - - - - - - - - - - - 

% Pressão diastólica desconhecida
consulta(c8, 14-10-2025, p8, 20, p_diastolica, 121, 76).
excecao(consulta(IdC, D, IdP, I, D, S, P)) :-
    consulta(IdC, D, IdP, I, p_diastolica, S, P).
    
% Pulsação desconhecida
consulta(c9, 14-03-2025, p9, 21, 69, 118, pulsacao_des).
excecao(consulta(IdC, D, IdP, I, D, S, P)) :-
    consulta(IdC, D, IdP, I, D, S, pulsacao_des).

% Conhecimento Imperfeito Impreciso - - - - - - - - - - - - - - - - - - 

% A pulsação do paciente é desconhecida, mas pertence a um intervalo de valores
excecao(consulta(c2, 05-02-2025, p2, 23, 84, 128, X)):-
    X >= 77,
    X =< 85.
    
% O Id do paciente é desconhecido, mas tem duas possibilidades
excecao(consulta(c10, 02-03-2025, p10, 21, 70, 115, 70)).
excecao(consulta(c10, 02-03-2025, p11, 21, 70, 115, 70)).

% Conhecimento Imperfeito Interdito - - - - - - - - - - - - - - - - - - 

% Pulsação do paciente interdita
consulta(c4, 12-02-2025, p4, 47, 91, 140, pulsaca_inter).
excecao(consulta(IdC, D, IdP, I, D, S, P)) :- 
    consulta(IdC, D, IdP, I, D, S, pulsaca_inter).
interdito(pulsaca_inter).
+consulta(IdC, D, IdP, I, D, S, P) :: (findall( (IdC, D, IdP, I, D, S, Ps),(consulta(c4, 12-02-2025, p4, 47, 91, 140, Ps),nao(interdito(Ps))),S ),
                  length( S,N ), N == 0).

% -------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado tensao arterial: IdTA, Classifcacao, IdPaciente, Sistolica Inferior, Sistolica Superior, Diastolica Inferior, Diastolica Superior -> {V,F,D}

ta(ta1, 'Normal-baixa', p1, 85, 119, 55, 75).
ta(ta2, 'Normal', p2, 120, 129, 75, 85).
ta(ta3, 'Normal-alta', p3, 130, 139, 86, 89).
ta(ta4, 'Hipertensão', p4, 140, 159, 90, 99).
ta(ta7, 'Normal-alta', p7, 130, 139, 86, 89).
ta(ta8, 'Normal', p8, 120, 129, 75, 85).
ta(ta9, 'Normal-baixa', p9, 85, 119, 55, 75).

% Conhecimento Imperfeiro Incerto - - - - - - - - - - - - - - - - - - 

% A Classificação é desconhecida
ta(ta5, class_desconhecida, p5, 85, 119, 55, 75).
excecao(ta(IdT, C, IdP, SI, SS, DI, DS)) :-
    ta(IdT, class_desconhecida, IdP, SI, SS, DI, DS).

% O Id é desconhecida
ta(ta6, 'Normal', id_pac_desc, 120, 129, 75, 85).
excecao(ta(IdT, C, IdP, SI, SS, DI, DS)) :-
    ta(IdT, class_desconhecida, id_pac_desc, SI, SS, DI, DS).

% Conhecimento Imperfeito Impreciso - - - - - - - - - - - - - - - - - - 

% A Sistolica Inferior pertence a um interavlo de valores
exceção(ta(ta11, 'Normal-baixa', p11, X, 119, 55, 75)):-
    X >= 80,
    X =< 90.

% Conhecimento Imperfeito Interdito - - - - - - - - - - - - - - - - - - 

% Classificação do paciente interdita
ta(ta10, class_inter, p10, 85, 119, 55, 75).
excecao( ta(IdT, C, IdP, SI, SS, DI, DS)) :- 
    ta(IdT, class_inter, IdP, SI, SS, DI, DS).
interdito(class_inter).
+ta(IdT, C, IdP, SI, SS, DI, DS) :: (findall( (IdT, Cs, IdP, SI, SS, DI, DS),(ta(ta10, Cs, p10, 85, 119, 55, 75),nao(interdito(Cs))),S ),
                  length( S,N ), N == 0).

% -------------------------------- - - - - - - - - - -  -  -  -  -   -     
% Extensao do predicado medicamento: IdP, Medicamento, Dosagem -> {V,F,D}

medicamento(p2, 'Captopril', '25mg/dia').
medicamento(p3, 'Losartan', '50mg/dia').
medicamento(p4, 'Amlodipina', '5mg/dia').
medicamento(p5, 'Sem medicacao', '---').
medicamento(p9, 'Hidroclorotiazida', '12.5mg/dia').
medicamento(p11, 'Perindopril', '4mg/dia').

% Conhecimento Imperfeiro Incerto - - - - - - - - - - - - - - - - - - 

% O Medicamento é desconhecido
medicamento(p7, medic_desconhecido,'10mg/dia').
excecao(medicamento(IdP, M, D)) :-
    medicamento(IdP, medic_desconhecido, D).

% A Dosagem é desconhecida
medicamento(p6, 'Enalapril', dose_desconhecida).
excecao(medicamento(IdP, M, D)) :-
    medicamento(IdP, M, dose_desconhecida).

% Conhecimento Imperfeito Impreciso - - - - - - - - - - - - - - - - - - 

% O Medicamento tem duas possibilidades
excecao(medicamento(p1, 'Ramipril', '10mg/dia')).
excecao(medicamento(p1, 'Captopril', '10mg/dia')).

% O Medicamento tem duas possibilidades
excecao(medicamento(p10, 'Losartan', '50mg/dia')).
excecao(medicamento(p10, 'Amlodipina', '5mg/dia')).

% Conhecimento Imperfeito Interdito - - - - - - - - - - - - - - - - - - 

% Medicamento e dosagem do paciente interditos
medicamento(p8, medic_interdito, dose_interdita).
excecao(medicamento(IdP, M, D)) :- 
    medicamento(IdP, medic_interdito, dose_interdita).
interdito(medic_interdito).
interdito(dose_interdita).
+medicamento(IdP, M, D) :: (findall( (IdP, Ms, Ds),(medicamento(p8, Ms, Ds),nao(interdito(Ms)),nao(interdito(Ds))),S ),
                  length( S,N ), N == 0).

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
% Invariantes

% Não pode existir mais do que um registo do mesmo paciente
+paciente(Id, N, D, I, S, M)::(findall(Id, paciente(Id, _, _, _, _, _), L),length(L, X),X == 1).

% Não pode existir mais do que um registo da mesma consulta
+consulta(IdC, D, IdP, I, Dia, Sis, Pul)::(findall(IdC, consulta(IdC, _, _, _, _, _, _), L),length(L, X),X == 1).

% Não pode existir mais do que um registo da mesma tensão arterial
+ta(IdTA, C, IdP, SI, SS, DI, DS)::(findall(IdTA, ta(IdTA, _, _, _, _, _, _), L),length(L, X),X == 1).

% Não pode existir mais do que um registo do mesmo medicamento (por Id do paciente)
+medicamento(IdP, M, D)::(findall(IdP, medicamento(IdP, _, _), L),length(L, X),X == 1).

% Não pode ser registada uma consulta para um paciente inexistente
+consulta(IdC, D, IdP, I, Dia, Sis, Pul)::(findall(IdP, paciente(IdP, _, _, _, _, _), L),length(L, X),X >= 1).

% Não pode ser registada uma tensão arterial para um paciente inexistente
+ta(IdTA, C, IdP, SI, SS, DI, DS)::(findall(IdP, paciente(IdP, _, _, _, _, _), L),length(L, X),X >= 1).

% Não pode ser registado um medicamento para um paciente inexistente
+medicamento(IdP, M, D)::(findall(IdP, paciente(IdP, _, _, _, _, _), L),length(L, X),X >= 1).

% Não pode ser removido um paciente que tenha consultas associadas
-paciente(Id, N, D, I, S, M)::(findall(IdC, consulta(IdC, _, Id, _, _, _, _), L),length(L, X),X == 0).

% Não pode ser removido um paciente que tenha tensões arteriais associadas
-paciente(Id, N, D, I, S, M)::(findall(IdTA, ta(IdTA, _, Id, _, _, _, _), L),length(L, X),X == 0).

% Não pode ser removido um paciente que tenha medicação associada
-paciente(Id, N, D, I, S, M)::(findall(Med, medicamento(Id, Med, _), L),length(L, X),X == 0).

% Não pode ser removida uma consulta se o paciente ainda existir
-consulta(IdC, D, IdP, I, Dia, Sis, Pul)::(findall(IdP, paciente(IdP, _, _, _, _, _), L),length(L, X),X == 0).

% Não pode ser removida uma tensão arterial se o paciente ainda existir
-ta(IdTA, C, IdP, SI, SS, DI, DS)::(findall(IdP, paciente(IdP, _, _, _, _, _), L),length(L, X),X == 0).

% Não pode ser removido um medicamento se o paciente ainda existir
-medicamento(IdP, M, D)::(findall(IdP, paciente(IdP, _, _, _, _, _), L),length(L, X),X == 0).

% Um paciente não pode ter duas tensões arteriais com o mesmo tipo de classificação
+ta(IdTA, C, IdP, SI, SS, DI, DS)::(findall(C, ta(_, C, IdP, _, _, _, _), L),length(L, X),X == 1).

% Um paciente não pode ter dois registos de medicamento idênticos
+medicamento(IdP, M, D)::(findall(M, medicamento(IdP, M, _), L),length(L, X),X == 1).

% -------------------------------- - - - - - - - - - -  -  -  -  -   -
% Transições de conhecimento: V->F, F->V ...

% Operador =.. -> univ -> converter um termo composto numa lista
% Termo → Lista
% Lista → Termo
% paciente(p1, 'Ana', 38) =.. L. -> L = [paciente, p1, 'Ana', 38].
% T =.. [paciente, p1, 'Ana', 38]. -> T = paciente(p1, 'Ana', 38).

% V -> F : torna falso algo que era verdadeiro
v_para_f(Termo) :-
    retract(Termo),              % Retira o Termo verdadeiro
    assert(-Termo).              % Insere o falso

% F -> V : torna verdadeiro algo que era falso
f_para_v(Termo) :-
    retract(-Termo),             % Retira o falso
    assert(Termo).               % Insere o verdadeiro
    
% V -> D : torna desconhecido algo que era verdadeiro
v_para_d(Termo,P,ValorDesconhecido) :-
    Termo =.. [Pred | Args],
    substitui(P,Args,ValorDesconhecido,NovosArgs),
    NovoTermo =.. [Pred | NovoArgs],
    retract(Termo),
    assert(excecao(NovoTermo)).

% D -> V : torna verdadeiro algo que era desconhecido
d_para_v(Excecao,P,Valor) :-
    Excecao = excecao(Termo),
    Termo =.. [Pred | Args],
    substitui(P,Args,Valor,NovosArgs),
    NovoTermo =.. [Pred | NovosArgs],
    retract(Excecao),
    assert(NovoTermo).

% D -> F : torna falso algo que era desconhecido
d_para_f(Excecao,P,Valor) :-
    Excecao = excecao(Termo),
    Termo =.. [Pred | Args],
    substitui(P,Args,Valor,NovosArgs),
    NovoTermo =.. [Pred | NovosArgs],
    retract(Excecao),
    assert(-NovoTermo).

% F -> D : torna desconhecido algo que era falso
f_para_d(Termo,P,ValorDesconhecido) :-
    Termo =.. [Pred | Args],
    substitui(P,Args,ValorDesconhecido,NovosArgs),
    NovoTermo =.. [Pred | NovosArgs],
    retract(-Termo),
    assert(excecao(NovoTermo)).

substitui(1, [_|T], X, [X|T]).      % Se for na posição 1, substitui logo
substitui(N, [H|T], X, [H|R]) :-    % Se for noutra posição...
    N > 1,
    N1 is N - 1,
    substitui(N1, T, X, R).         % Retorna N1, a Tail, o Valor (desconhecido ou não) e o Resulado até N ser 1 e poder substituir na posição certa

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

% Nossa---------------------------------------------------------------
si_conjuncao(Q1, Q2, Resultado) :-
    si(Q1, R1),
    si(Q2, R2),
    tabela_c(R1, R2, Resultado). 
 
% Stor---------------------------------------------------------------   
si_conjuncao2(Q1 e Q2, R) :-
    si_conjuncao2(Q1, R1),
    si_conjuncao2(Q2, R2),
    conjuncao(R1, R2, R).
 
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
si_disjuncao2(Q1 ou Q2, R) :-
    si_disjuncao2(Q1, R1),
    si_disjuncao2(Q2, R2),
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

# Explicar o siR e o que é o CR1 e CR2
# Como funciona siC (como vai buscar as tabelas?)
# Não podemos usar !

% -------------------------------- - - - - - - - - - -  -  -  -  -   -
% Relatar 

% Paciente - - - - - - - - - - - - - - - - - - 

% Extensão do predicado nomePaciente: N, R -> {V,F}
nomePaciente(N, R) :-
    findall((Id, N, D, I, S, M), paciente(Id, N, D, I, S, M), R).

% Extensão do predicado idPaciente: Id, R -> {V,F}
idPaciente(Id, R) :-
    findall((Id, N, D, I, S, M), paciente(Id, N, D, I, S, M), R).

% Extensão do predicado sexoPaciente: S, R -> {V,F}
sexoPaciente(S, R) :-
    findall((Id, N, D, I, S, M), paciente(Id, N, D, I, S, M), R).

% Extensão do predicado numeroPacientes: R -> {V,F}
numeroPacientes(R) :-
    findall(Id, paciente(Id, _, _, _, _, _), L),
    comprimento(L, R).

% Extensão do predicado pacientesFaixaEtaria: Inf, Sup, R -> {V,F}
pacientesFaixaEtaria(Inf, Sup, R) :-
    findall(I, (consulta(_, _, _, I, _, _, _), I >= Inf, I =< Sup), L),
    comprimento(L, R).


% Consulta - - - - - - - - - - - - - - - - - - 

% Extensão do predicado idConsulta: IdC, R -> {V,F}
idConsulta(IdC, R) :-
    findall((IdC, D, IdP, I, Dia, Sis, Pul), consulta(IdC, D, IdP, I, Dia, Sis, Pul), R).

% Extensão do predicado idPacienteConsulta: IdP, R -> {V,F}
idPacienteConsulta(IdP, R) :-
    findall((IdC, D, IdP, I, Dia, Sis, Pul), consulta(IdC, D, IdP, I, Dia, Sis, Pul), R).

% Extensão do predicado dataConsulta: D, R -> {V,F}
dataConsulta(D, R) :-
    findall((IdC, D, IdP, I, Dia, Sis, Pul), consulta(IdC, D, IdP, I, Dia, Sis, Pul), R).

% Extensão do predicado numeroConsultas: R -> {V,F}
numeroConsultas(R) :-
    findall(IdC, consulta(IdC, _, _, _, _, _, _), L),
    comprimento(L, R).

% Tensão Arterial - - - - - - - - - - - - - - - - - - 

% Extensão do predicado taPaciente: IdP, R -> {V,F}
taPaciente(IdP, R) :-
    findall((IdT, C, IdP, SI, SS, DI, DS), ta(IdT, C, IdP, SI, SS, DI, DS), R).

% Extensão do predicado taClassificacao: C, R -> {V,F}
taClassificacao(C, R) :-
    findall((IdT, C, IdP, SI, SS, DI, DS), ta(IdT, C, IdP, SI, SS, DI, DS), R).

% Extensão do predicado numeroTA: R -> {V,F}
numeroTA(R) :-
    findall(IdT, ta(IdT, _, _, _, _, _, _), L),
    comprimento(L, R).
    
% Extensão do predicado pacientesHipotensos: R -> {V,F}
pacientesHipotensos(R) :-
    findall(IdP, ta(_, 'Hipotensão', IdP, _, _, _, _), L),
    sort(L, R).

% Extensão do predicado pacientesNormalBaixa: R -> {V,F}
pacientesNormalBaixa(R) :-
    findall(IdP, ta(_, 'Normal-baixa', IdP, _, _, _, _), L),
    sort(L, R).
    
% Extensão do predicado pacientesNormais: R -> {V,F}
pacientesNormais(R) :-
    findall(IdP, ta(_, 'Normal', IdP, _, _, _, _), L),
    sort(L, R).
    
% Extensão do predicado pacientesNormalAlta: R -> {V,F}
pacientesNormalAlta(R) :-
    findall(IdP, ta(_, 'Normal-alta', IdP, _, _, _, _), L),
    sort(L, R).
    
% Extensão do predicado pacientesHipertensos: R -> {V,F}
pacientesHipertensos(R) :-
    findall(IdP, ta(_, 'Hipertensão', IdP, _, _, _, _), L),
    sort(L, R).

% Mendicamento - - - - - - - - - - - - - - - - - - 

% Extensão do predicado medicamentoPaciente: IdP, R -> {V,F}
medicamentoPaciente(IdP, R) :-
    findall((IdP, M, D), medicamento(IdP, M, D), R).

% Extensão do predicado pacientesMedicamento: M, R -> {V,F}
pacientesMedicamento(M, R) :-
    findall((IdP, M, D), medicamento(IdP, M, D), R).

% Extensão do predicado numeroMedicados: R -> {V,F}
numeroMedicados(R) :-
    findall(IdP, medicamento(IdP, _, _), L),
    comprimento(L, R).

% Predicado auxiliar - - - - - - - - - - - - - - - - - - 

somaLista([], 0).
somaLista([H|T], R) :-
    somaLista(T, X),
    R is H + X.

% -------------------------------- - - - - - - - - - -  -  -  -  -   -
% Estatísticas gerais

% Estatísticas sobre os pacientes - - - - - - - - - - - - - - - - - - 
estatisticasPacientes :-
    numeroPacientes(NPac),
    numeroConsultas(NCons),
    numeroTA(NTA),
    numeroMedicados(NMed),
    pacientesHipotensos(Hipo), comprimento(Hipo, NumHipo),
    pacientesNormalBaixa(NB), comprimento(NB, NumNB),
    pacientesNormais(Norm), comprimento(Norm, NumNorm),
    pacientesNormalAlta(NA), comprimento(NA, NumNA),
    pacientesHipertensos(Hip), comprimento(Hip, NumHip),

    nl, write('===== ESTATÍSTICAS DE PACIENTES ====='), nl, nl,
    write('Número total de pacientes: '), write(NPac), nl, nl,
    write('Número total de consultas: '), write(NCons), nl,
    write('Número total de registos de tensão arterial: '), write(NTA), nl,
    write('Número total de pacientes medicados: '), write(NMed), nl, nl,
    write('Pacientes hipotensos: '), write(NumHipo), nl,
    write('Pacientes com tensão normal-baixa: '), write(NumNB), nl,
    write('Pacientes com tensão normal: '), write(NumNorm), nl,
    write('Pacientes com tensão normal-alta: '), write(NumNA), nl,
    write('Pacientes hipertensos: '), write(NumHip), nl, nl,
    write('====================================='), nl.

% Estatísticas sobre medicamentos - - - - - - - - - - - - - - - - - - 
estatisticasMedicamentos :-
    numeroMedicados(NMed),
    findall(M, medicamento(_, M, _), LMed),
    sort(LMed, MedsUnicos),
    comprimento(MedsUnicos, NumMeds),

    nl, write('===== ESTATÍSTICAS DE MEDICAÇÃO ====='), nl, nl,
    write('Número total de pacientes medicados: '), write(NMed), nl,
    write('Número de medicamentos diferentes prescritos: '), write(NumMeds), nl,
    write('Lista de medicamentos prescritos: '), write(MedsUnicos), nl, nl,
    write('====================================='), nl.








