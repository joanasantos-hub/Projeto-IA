% SICStus PROLOG: Definicoes iniciais
:- dynamic paciente/6. 
:- dynamic consulta/7.
:- dynamic ta/5.
:- dynamic historico/6.
:- dynamic medicamento/3.
:- dynamic '-'/1.
:- op( 900,xfy,'::' ).
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
+paciente(Id, N, D, I, S, M) :: (findall((Id, N, D, I, S, Ms),(paciente(p2, 'Carlos Pereira', 27-11-2001, 23, masculino, Ms),nao(interdito(Ms))),S ),
                  length( S,N ), N == 0).

% -------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado consulta: IdConsulta, Data, IdPaciente, Idade, Diastolica, Sistolica, Pulsacao -> {V,F,D}

% Paciente p1
consulta(c1, 02-02-2025, p1, 38, 70, 115, 70).

% Paciente p2
consulta(c12, 20-02-2025, p2, 23, 88, 138, 78).
consulta(c22, 10-03-2025, p2, 23, 92, 150, 82).

% Paciente p3
consulta(c3, 10-02-2025, p3, 30, 88, 134, 78).
consulta(c13, 25-02-2025, p3, 30, 82, 126, 75).

% Paciente p4
consulta(c14, 26-02-2025, p4, 47, 94, 152, 84).
consulta(c23, 15-03-2025, p4, 47, 90, 142, 80).

% Paciente p5
consulta(c5, 14-02-2025, p5, 32, 74, 118, 72).

% Paciente p6
consulta(c6, 20-02-2025, p6, 32, 80, 120, 75).

% Paciente p7
consulta(c7, 22-02-2025, p7, 35, 87, 133, 70).
consulta(c17, 16-03-2025, p7, 35, 85, 129, 72).

% Paciente p11
consulta(c21, 24-03-2025, p11, 21, 89, 132, 79).
consulta(c24, 10-04-2025, p11, 21, 87, 130, 77).

% Conhecimento Imperfeito Incerto - - - - - - - - - - - - - - - - - -
% Paciente p8 -> Pressão diastólica desconhecida
consulta(c8, 14-10-2025, p8, 20, p_diastolica, 121, 76).
excecao(consulta(IdC, D, IdP, I, D, S, P)) :-
    consulta(IdC, D, IdP, I, p_diastolica, S, P).

% Paciente p9 -> Pulsação desconhecida
consulta(c9, 14-03-2025, p9, 21, 69, 118, pulsacao_des).
excecao(consulta(IdC, D, IdP, I, D, S, P)) :-
    consulta(IdC, D, IdP, I, D, S, pulsacao_des).

% Conhecimento Imperfeito Impreciso - - - - - - - - - - - - - - - - - - 
% Paciente p2 -> A pulsação do paciente é desconhecida, mas pertence a um intervalo de valores
excecao(consulta(c2, 05-02-2025, p2, 23, 84, 128, X)):-
    X >= 77,
    X =< 85.
    
% Paciente p10 -> A pulsação do paciente é desconhecida, mas tem duas possibilidades
excecao(consulta(c10, 02-03-2025, p10, 21, 70, 115, 70)).
excecao(consulta(c10, 02-03-2025, p10, 21, 70, 115, 75)).

% Conhecimento Imperfeito Interdito - - - - - - - - - - - - - - - - - - 
% % Paciente p4 -> Pulsação do paciente interdita
consulta(c4, 12-02-2025, p4, 47, 91, 140, pulsaca_inter).
excecao(consulta(IdC, D, IdP, I, D, S, P)) :- 
    consulta(IdC, D, IdP, I, D, S, pulsaca_inter).
interdito(pulsaca_inter).
+consulta(IdC, D, IdP, I, D, S, P) :: (findall((IdC, D, IdP, I, D, S, Ps),(consulta(c4, 12-02-2025, p4, 47, 91, 140, Ps),nao(interdito(Ps))),S ),
                  length( S,N ), N == 0).

% -------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado tensao arterial: Classifcacao, Sistolica Inferior, Sistolica Superior, Diastolica Inferior, Diastolica Superior -> {V,F,D}

ta('Hipotensão', 0, 84, 0, 54).
ta('Normal-baixa', 85, 119, 55, 75).
ta('Normal', 120, 129, 75, 85).
ta('Normal-alta', 130, 139, 86, 89).
ta('Hipertensão', 140, 200, 90, 150).

% -------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado historico: IdPaciente, Data, Diastolica, Sistolica, Classifcacao, Pulsacao -> {V,F,D}

classifica_tensao(Sistolica, Diastolica, Classificacao) :-
    ta(Classificacao, SisInf, SisSup, DiaInf, DiaSup),
    Sistolica >= SisInf, Sistolica =< SisSup,
    Diastolica >= DiaInf, Diastolica =< DiaSup.

regista_consulta(IdC, Data, IdP, Idade, Diastolica, Sistolica, Pulsacao) :-
    assert(consulta(IdC, Data, IdP, Idade, Diastolica, Sistolica, Pulsacao)),
    classifica_tensao(Sistolica, Diastolica, Classificacao),
    assert(historico(IdP, Data, Diastolica, Sistolica, Classificacao, Pulsacao)).

% Paciente p1
historico(p1, 02-02-2025, 70, 115, 'Normal-baixa', 70).

% Paciente p2
historico(p2, 20-02-2025, 88, 138, 'Normal-alta', 78).
historico(p2, 10-03-2025, 92, 150, 'Hipertensão', 82).

% Paciente p3
historico(p3, 10-02-2025, 88, 134, 'Normal-alta', 78).
historico(p3, 25-02-2025, 82, 126, 'Normal', 75).

% Paciente p4
historico(p4, 26-02-2025, 94, 152, 'Hipertensão', 84).
historico(p4, 15-03-2025, 90, 142, 'Hipertensão', 80).

% Paciente p5
historico(p5, 14-02-2025, 74, 118, 'Normal-baixa', 72).

% Paciente p6
historico(p6, 20-02-2025, 80, 120, 'Normal', 75).

% Paciente p7
historico(p7, 22-02-2025, 87, 133, 'Normal-alta', 70).
historico(p7, 16-03-2025, 85, 129, 'Normal', 72).

% Paciente p11
historico(p11, 24-03-2025, 89, 132, 'Normal-alta', 79).
historico(p11, 10-04-2025, 87, 130, 'Normal', 77).

% Conhecimento Imperfeiro Incerto - - - - - - - - - - - - - - - - - - 
% Paciente p8 -> Pressão diastólica desconhecida
historico(p8, 14-10-2025, diastolica_des, 121, 'Normal', 76).
excecao(historico(IdP, D, DIA, SIS, C, P)) :-
    historico(IdP, D, diastolica_des, SIS, C, P).
    
% Paciente p9 -> A Pulsação é desconhecida
historico(p9, 14-03-2025, 69, 118, 'Normal-baixa', pulsacao_des).
excecao(historico(IdP, D, DIA, SIS, C, P)) :-
    historico(IdP, D, DIA, SIS, C, pulsacao_des).

% Conhecimento Imperfeito Impreciso - - - - - - - - - - - - - - - - - - 
% Paciente p2 -> A pulsação do paciente é desconhecida, mas pertence a um intervalo de valores
excecao(historico(p2, 05-02-2025, 90, 145, 'Hipertensão', X)):-
    X >= 77,
    X =< 85.

% Paciente p10 -> A pulsação do paciente é desconhecida, mas tem duas possibilidades
excecao(historico(p10, 02-03-2025, 70, 115, 'Normal-baixa', 70)).
excecao(historico(p10, 02-03-2025, 70, 115, 'Normal-baixa', 75)).

% Conhecimento Imperfeito Interdito - - - - - - - - - - - - - - - - - - 
% Paciente p4 -> Pulsação do paciente interdita
historico(p4, 12-02-2025, 91, 140, 'Hipertensão', pulsacao_inter).
excecao( historico(IdP, D, DIA, SIS, C, P)) :- 
     historico(IdP, D, DIA, SIS, C, pulsacao_inter).
interdito(pulsacao_inter).
+ historico(IdP, D, DIA, SIS, C, P) :: (findall((IdP, D, DIA, SIS, C, Ps),(historico(p4, 12-02-2025, 91, 140, 'Hipertensão', Ps).,nao(interdito(Ps))),S ),
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
% Paciente7 ->O Medicamento é desconhecido
medicamento(p7, medic_desconhecido,'10mg/dia').
excecao(medicamento(IdP, M, D)) :-
    medicamento(IdP, medic_desconhecido, D).

% Paciente6 ->A Dosagem é desconhecida
medicamento(p6, 'Enalapril', dose_desconhecida).
excecao(medicamento(IdP, M, D)) :-
    medicamento(IdP, M, dose_desconhecida).

% Conhecimento Imperfeito Impreciso - - - - - - - - - - - - - - - - - - 
% Paciente1 -> O Medicamento tem duas possibilidades
excecao(medicamento(p1, 'Ramipril', '10mg/dia')).
excecao(medicamento(p1, 'Captopril', '10mg/dia')).

% Paciente10 -> O Medicamento tem duas possibilidades
excecao(medicamento(p10, 'Losartan', '50mg/dia')).
excecao(medicamento(p10, 'Amlodipina', '5mg/dia')).

% Conhecimento Imperfeito Interdito - - - - - - - - - - - - - - - - - - 
% Paciente8 -> Medicamento e dosagem do paciente interditos
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

% Não pode existir mais do que um registo do mesmo historico
+historico(IdP, Data, Dia, Sis, Clas, Pul)::(findall((IdP, Data), historico(IdP, Data, _, _, _, _), L),length(L, X),X == 1).

% Um paciente não pode ter dois registos de medicamento idênticos
+medicamento(IdP, M, D)::(findall(M, medicamento(IdP, M, _), L),length(L, X),X == 1).

% Não pode ser registada uma consulta para um paciente inexistente
+consulta(IdC, D, IdP, I, Dia, Sis, Pul)::(findall(IdP, paciente(IdP, _, _, _, _, _), L),length(L, X),X >= 1).

% Não pode ser registado um historico para um paciente inexistente
+historico(IdP, Data, Dia, Sis, Clas, Pul)::(findall(IdP, paciente(IdP, _, _, _, _, _), L),length(L, X),X >= 1).

% Não pode ser registado um medicamento para um paciente inexistente
+medicamento(IdP, M, D)::(findall(IdP, paciente(IdP, _, _, _, _, _), L),length(L, X),X >= 1).

% Não pode ser removido um paciente que tenha consultas associadas
-paciente(Id, N, D, I, S, M)::(findall(IdC, consulta(IdC, _, Id, _, _, _, _), L),length(L, X),X == 0).

% Não pode ser removido um paciente que tenha hitoricos associados
-paciente(Id, N, D, I, S, M)::(findall(Id, historico(Id,_, _, _, _, _), L),length(L, X),X == 0).

% Não pode ser removido um paciente que tenha medicação associada
-paciente(Id, N, D, I, S, M)::(findall(Med, medicamento(Id, Med, _), L),length(L, X),X == 0).

% Não pode ser removida uma consulta se o paciente ainda existir
-consulta(IdC, D, IdP, I, Dia, Sis, Pul)::(findall(IdP, paciente(IdP, _, _, _, _, _), L),length(L, X),X == 0).

% Não pode ser removido um historico se o paciente ainda existir
-historico(IdP, Data, Dia, Sis, Clas, Pul)::(findall(IdP, paciente(IdP, _, _, _, _, _), L),length(L, X),X == 0).

% Não pode ser removido um medicamento se o paciente ainda existir
-medicamento(IdP, M, D)::(findall(IdP, paciente(IdP, _, _, _, _, _), L),length(L, X),X == 0).

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

% Conjunção - - - - - - - - - - - - - - - - - - 

% Valores da tabela de verdade da conjucao
tabela_c(verdadeiro, verdadeiro, verdadeiro).
tabela_c(verdadeiro, falso, falso).
tabela_c(falso, verdadeiro, falso).
tabela_c(falso, falso, falso).
tabela_c(verdadeiro, desconhecido, desconhecido).
tabela_c(desconhecido, verdadeiro, desconhecido).
tabela_c(falso, desconhecido, falso).
tabela_c(desconhecido, falso, falso).
tabela_c(desconhecido, desconhecido, desconhecido).

conjuncao(C1, C2, (C1 e C2)).

composicao_c(C1 e C2,C) :-
    tabela_c(C1,C2,C).

%si_conjuncao2(Q1 e Q2, R) :-
    %si_conjuncao2(Q1, R1),
    %si_conjuncao2(Q2, R2),
    %conjuncao(R1, R2, R).

siR(Q1 e Q2, CR,R) :-
    siR(Q1, CR1, R1),
    siR(Q2, CR2, R2),
    conjuncao(CR1, CR2, CR),
    composicao_c(CR, R).

% Disjunção - - - - - - - - - - - - - - - - - - 

% Valores da tabela de verdade da disjuncao
tabela_d(verdadeiro, verdadeiro, verdadeiro).
tabela_d(verdadeiro, falso,    verdadeiro).
tabela_d(falso,    verdadeiro, verdadeiro).
tabela_d(falso,    falso,      falso).
tabela_d(verdadeiro, desconhecido, verdadeiro).
tabela_d(desconhecido, verdadeiro,  verdadeiro).
tabela_d(falso,    desconhecido, desconhecido).
tabela_d(desconhecido, falso,     desconhecido).
tabela_d(desconhecido, desconhecido, desconhecido).

%si_disjuncao2(Q1 ou Q2, R) :-
    %si_disjuncao2(Q1, R1),
    %si_disjuncao2(Q2, R2),
    %dijuncao(R1, R2, R). 
    
dijuncao(C1, C2, (C1 ou C2)).

composicao_d(C1 ou C2,C) :-
    tabela_d(C1,C2,C).

siR(Q1 ou Q2, CR,R) :-
    siR(Q1, CR1, R1),
    siR(Q2, CR2, R2),
    disjuncao(CR1, CR2, CR),
    composicao_d(CR, R).

%siR(Q,R,R):- ????? TEMOS QUE VER SE É PRECISO
    %si(Q,R).

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

% Historico - - - - - - - - - - - - - - - - - - 

% Extensão do predicado historico_P: IdP, R -> {V,F}
historico_P(IdP, R) :-
    findall((IdP, Data, Dia, Sis, Clas, Pul), historico(IdP, Data, Dia, Sis, Clas, Pul), R).

% Extensão do predicado historico_Classificacao: C, R -> {V,F}
historico_Classificacao(C, R) :-
    findall((IdP, Data, Dia, Sis, Clas, Pul), historico(IdP, Data, Dia, Sis, Clas, Pul), R).

% Extensão do predicado historico_Data: R -> {V,F}
historico_Data(Data,R) :-
    findall((IdP, Data, Dia, Sis, Clas, Pul), historico(IdP, Data, Dia, Sis, Clas, Pul), L),
    comprimento(L, R).
    
% Extensão do predicado pacientesHipotensos: R -> {V,F}
pacientesHipotensos(R) :-
    findall(IdP, historico(IdP, _, _, _, 'Hipotensão', _), L),
    sort(L, R).

% Extensão do predicado pacientesNormalBaixa: R -> {V,F}
pacientesNormalBaixa(R) :-
    findall(IdP, historico(IdP, _, _, _, 'Normal-baixa', _), L),
    sort(L, R).
    
% Extensão do predicado pacientesNormais: R -> {V,F}
pacientesNormais(R) :-
    findall(IdP, historico(IdP, _, _, _, 'Normal', _), L),
    sort(L, R).
    
% Extensão do predicado pacientesNormalAlta: R -> {V,F}
pacientesNormalAlta(R) :-
    findall(IdP, historico(IdP, _, _, _, 'Normal-alta', _), L),
    sort(L, R).
    
% Extensão do predicado pacientesHipertensos: R -> {V,F}
pacientesHipertensos(R) :-
    findall(IdP, historico(IdP, _, _, _, 'Hipertensão', _), L),
    sort(L, R).

% Medicamento - - - - - - - - - - - - - - - - - - 

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
comprimento([], 0).
comprimento([_|T], R) :-
    comprimento(T, N),
    R is N + 1.


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


















