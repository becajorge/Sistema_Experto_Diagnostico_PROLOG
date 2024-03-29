
:- use_module(library(pce)).
:- use_module(library(pce_style_item)).
:-dynamic si/1,no/1. %predicados dinamicos


main:-
	new(Menu, dialog('SIST EXPER ENFER-RESPIRATORIAS', size(500,500))),
	new(L, label(nombre, 'Bienvenidos a su diagnostico')),
	new(@texto, label(nombre, 'Segun la respuestas dadas tendra su resultado:')),
	new(@respl, label(nombre, '')),
	new(Salir, button('Salir', and(message(Menu,destroy), message(Menu, free)))),
	new(@boton, button('Realizar test', message(@prolog, botones))),
	send(Menu, append(L)), new(@btncarrera, button('�Diagnostico?')),
	send(Menu,display,L,point(100,20)),
	send(Menu,display,@boton,point(130,150)),
	send(Menu,display,@texto,point(50,100)),
	send(Menu,display,Salir,point(20,400)),
	send(Menu,display,@respl,point(20,130)),
	send(Menu,open_centered).

%base de conocimientos

enfermedades(tuberculosis_pulmonar):- tuberculosis_pulmonar,!.
enfermedades(neumonia):- neumonia,!.
enfermedades(asma):- asma,!.
enfermedades(cancer_al_pulmon):- cancer_al_pulmon,!.
enfermedades('No estoy entrenado para darte ese diagnostico').

tuberculosis_pulmonar :-
	tiene_tuberculosis_pulmonar ,
pregunta('�Tiene dificultad al respirar?'),
pregunta('�Tiene dolor en el pecho?'),
pregunta('�Tiene tos persistente?'),
pregunta('�Tiene sudoracion en la noche?'),
pregunta('�Tiene fatiga?'),
pregunta('�Tiene perdida de peso?'),
pregunta('�Tiene debilidad?'),
pregunta('�Tiene falta de apetito?').

neumonia :-
  tiene_neumonia,
pregunta('�Tiene dolores articulares?'),
pregunta('�Tiene tos con flema?'),
pregunta('�Tiene dificuldad para respirar?'),
pregunta('�Tiene escalofrios?'),
pregunta('�Tiene fatiga o debilidad?').

asma:-
	tiene_asma,
pregunta('�Tiene dificultad para respirar?'),
pregunta('�Tiene tos seca?'),
pregunta('�Tienes opresion en el pecho?'),
pregunta('�Tienes dificultad para dormir?').

cancer_al_pulmon :-
	tiene_cancer_al_pulmon,
pregunta('�Tines tos persistente?'),
pregunta('�Te falta aliento?'),
pregunta('�Tienes dolor en el pecho?'),
pregunta('�Estas tociendo con sangre?'),
pregunta('�Perdiste mucho peso?'),
pregunta('�tienes infeciones respiratorias recurrentes?').


%desconocido :- se_desconoce_enfermedad.

tiene_tuberculosis_pulmonar:-  pregunta('�Tiene dificultad al respirar?'),
pregunta('�Tiene dolor en el pecho?'),
pregunta('�Tiene tos persistente?'),
pregunta('�Tiene sudoracion en la noche?'),
pregunta('�Tiene fatiga?'),
pregunta('�Tiene perdida de peso?'),
pregunta('�Tiene debilidad?'),
pregunta('�Tiene falta de apetito?').


tiene_neumonia:- pregunta('�Tiene dolores articulares?'),
pregunta('�Tiene tos con flema?'),
pregunta('�Tiene dificuldad para respirar?'),
pregunta('�Tiene escalofrios?'),
pregunta('�Tiene fatiga o debilidad?').

tiene_asma:- pregunta('�Tiene dificultad para respirar?'),
pregunta('�Tiene tos seca?'),
pregunta('�Tienes opresion en el pecho?'),
pregunta('�Tienes dificultad para dormir?').

tiene_cancer_al_pulmon:- pregunta('�Tines tos persistente?'),
pregunta('�Te falta aliento?'),
pregunta('�Tienes dolor en el pecho?'),
pregunta('�Estas tociendo con sangre?'),
pregunta('�Perdiste mucho peso?'),
pregunta('�tienes infeciones respiratorias recurrentes?').


preguntar(Problema):-new(Di, dialog('Examen Medico')),
	new(L2, label(texto,'Responde las siguientes preguntas')),
	new(La, label(prob,Problema)),

	new(B1,button(si,and(message(Di,return,si)))),
	new(B2,button(no,and(message(Di,return,no)))),

	send(Di,append(L2)),
	send(Di,append(La)),
	send(Di,append(B1)),
	send(Di,append(B2)),

	send(Di,default_button,si),
	send(Di,open_centered),
	get(Di,confirm,Answer),
	write(Answer),send(Di,destroy),


	((Answer==si)->assert(si(Problema)); assert(no(Problema)),fail).

pregunta(S):- (si(S)->true; (no(S)->fail;preguntar(S))).
limpiar:- retract(si(_)),fail.
limpiar:- retract(no(_)),fail.
limpiar :- \+ current_predicate(si/1), \+ current_predicate(no/1).


botones :-lim,
	send(@boton,free),
	send(@btncarrera,free),
	enfermedades(Enter),
	send(@texto, selection('De acuerdo con sus respuestas,usted padece de:')),
	send(@respl, selection(Enter)),
	new(@boton, button('Iniciar su evaluacion', message(@prolog, botones))),
	send(Menu,display,@boton,point(40,50)),
	send(Menu,display,@btncarrera,point(20,50)),
	limpiar.

lim:- send(@respl, selection('')).

limpiar2:-
	send(@texto,free),
	send(@respl,free),
	send(@btncarrera,free),
	send(@boton,free),
        (\+ current_predicate(si/1) -> assert(si(_)) ; retractall(si(_))),
        (\+ current_predicate(no/1) -> assert(no(_)) ; retractall(no(_))).
