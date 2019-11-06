:- use_module(library(tokenize)).

/***
 *    ██████╗  █████╗ ██████╗ ███████╗███████╗██████╗
 *    ██╔══██╗██╔══██╗██╔══██╗██╔════╝██╔════╝██╔══██╗
 *    ██████╔╝███████║██████╔╝███████╗█████╗  ██████╔╝
 *    ██╔═══╝ ██╔══██║██╔══██╗╚════██║██╔══╝  ██╔══██╗
 *    ██║     ██║  ██║██║  ██║███████║███████╗██║  ██║
 *    ╚═╝     ╚═╝  ╚═╝╚═╝  ╚═╝╚══════╝╚══════╝╚═╝  ╚═╝
 *
 */
%-------------------------------REGLAS SINTÁCTICAS------------------------------
oracion(tree(P0, PP, P1, P2, P3, P4)) --> saludo(P0), program(PP), articulo(P1), lugar(P2), pregunta(P3), comando(P4), !.
oracion(tree(P0, P1, P2, P3, P4)) --> saludo(P0), articulo(P1), lugar(P2), pregunta(P3), comando(P4), !.
oracion(tree(P0, PP, P1, P2)) --> saludo(P0), program(PP), pregunta(P1), comando(P2), !.
oracion(tree(P0, P1, P2)) --> saludo(P0), pregunta(P1), comando(P2), !.
oracion(tree(SN, SV, SS)) --> saludo(SN), program(SV), solicitud(SS), !.
oracion(tree(SN, SV)) --> saludo(SN), program(SV), !.
oracion(tree(SN, SV)) --> saludo(SN), solicitud(SV), !.
oracion(tree(SN)) --> saludo(SN), !.
oracion(tree(P1, P2, P3, P4)) --> articulo(P1), lugar(P2), pregunta(P3), comando(P4), !.

oracion(tree(PR, SU, D, SUS, ES, EL, M)) --> pronombre(PR), sustantivo(SU), articulo(D), sustantivo(SUS), articulo(ES), number(EL), medida(M), !.
oracion(tree(PR, SU, ES, POS1, POS2, POS3)) --> pronombre(PR), sustantivo(SU), articulo(ES), number(POS1), number(POS2), number(POS3), !.
oracion(tree(PR, SU, D, SUS, ES, EL)) --> pronombre(PR), sustantivo(SU), articulo(D), sustantivo(SUS), articulo(ES), direccion(EL), !.
oracion(tree(PR, SU, ES, EL)) --> pronombre(PR), sustantivo(SU), articulo(ES), elemento(EL), !.

oracion(tree(P1, POS1, POS2, POS3)) --> sustantivo(P1), number(POS1), number(POS2), number(POS3), !.
oracion(tree(P1, P2)) --> sustantivo(P1), elemento(P2).
oracion(tree(POS1, POS2, POS3)) --> number(POS1), number(POS2), number(POS3), !.
oracion(tree(P1, P2)) --> pregunta(P1), comando(P2), !.
oracion(tree(SV)) --> solicitud(SV), !.
oracion(tree(SV)) --> despedida(SV), !.
oracion(tree(SV)) --> ayuda(SV), !.
oracion(tree(VB, SS, AR, CM)) --> verbo(VB), sustantivo(SS), articulo(AR), comando(CM), !.
oracion(tree(DAT)) --> elemento(DAT).

number(elemento(E)) --> [number(E)].

direccion(elemento(oe)) --> [word(oe)].
direccion(elemento(eo)) --> [word(eo)].
direccion(elemento(ns)) --> [word(ns)].
direccion(elemento(sn)) --> [word(sn)].

medida(medida(km)) --> [word(km), word(h)].
medida(medida(milla)) --> [word(milla), word(h)].
medida(medida(kilogram)) --> [word(kg)].

pregunta(pregunta(puedo)) --> [word(puedo)].
pregunta(pregunta(debo)) --> [word(debo)].

lugar(lugar(donde)) --> [word(donde)].

ayuda(ayuda(mayday)) --> [word(mayday), word(mayday)].
ayuda(ayuda(7500)) --> [word('7500')].

pronombre(pronombre(mi)) --> [word(mi)].
pronombre(pronombre(la)) --> [word(la)].
pronombre(pronombre(el)) --> [word(el)].

sustantivo(sustantivo(avion)) --> [word(avion)].
sustantivo(sustantivo(placa)) --> [word(placa)].
sustantivo(sustantivo(direccion)) --> [word(direccion)].
sustantivo(sustantivo(velocidad)) --> [word(velocidad)].
sustantivo(sustantivo(viento)) --> [word(viento)].
sustantivo(sustantivo(peso)) --> [word(peso)].
sustantivo(sustantivo(permiso)) --> [word(permiso)].
sustantivo(sustantivo(posicion)) --> [word(posicion)].

despedida(despedida(ok)) --> [word(ok)].
despedida(despedida(gracias)) --> [word(gracias)].
despedida(despedida(adios)) --> [word(adios)].
despedida(despedida(S, D)) --> estado(S), adjetivo(D).
despedida(despedida(S, Y, D)) --> final(S), articulo(Y), final(D).

final(final(cambio)) --> [word(cambio)].
final(final(fuera)) --> [word(fuera)].

estado(estado(esta)) --> [word(esta)].

adjetivo(adjetivo(bien)) --> [word(bien)].
adjetivo(adjetivo(buenos)) --> [word(buenos)].
adjetivo(adjetivo(buenas)) --> [word(buenas)].

saludo(saludo(S, D)) --> adjetivo(S), saludo_tipo(D).
saludo(saludo(hola)) --> [word(hola)].

saludo_tipo(saludo_tipo(dias)) --> [word(dias)].
saludo_tipo(saludo_tipo(noches)) --> [word(noches)].

program(program(agata)) --> [word(agata)].
program(program(maycey)) --> [word(maycey)].

solicitud(solicitud(VB, CM)) --> verbo(VB), comando(CM).
solicitud(solicitud(VB, AR, CM)) --> verbo(VB), articulo(AR), comando(CM).

verbo(verbo(quiero)) --> [word(quiero)].
verbo(verbo(deseo)) --> [word(deseo)].
verbo(verbo(voy)) --> [word(voy)].
verbo(verbo(tengo)) --> [word(tengo)].
verbo(verbo(solicito)) --> [word(solicito)].

articulo(articulo(a)) --> [word(a)].
articulo(articulo(que)) --> [word(que)].
articulo(articulo(es)) --> [word(es)].
articulo(articulo(y)) --> [word(y)].
articulo(articulo(en)) --> [word(en)].
articulo(articulo(del)) --> [word(del)].
articulo(articulo(para)) --> [word(para)].


comando(comando(aterrizar)) --> [word(aterrizar)].
comando(comando(despegar)) --> [word(despegar)].

elemento(elemento(EL)) --> [word(EL)].



%-------------------------------ANALIZADOR SEMÁNTICO----------------------------

/*
Regla principal para inciar el añalisis de la entrada del usuario.

Argumentos:
  Input: Entrada del usuario a analizar.
  Result: valores extraídos de la entrada, unificada con [] si la entrada no posee datos.

*/
go(Input, Result) :-
  generarAST(Input, Tree), !, semantica(Tree, Result).

/*
Regla para seguir obteniendo entrada del usuario en caso de error.

Argumentos:
  Result: valores extraídos de la entrada, unificada con [] si la entrada no posee datos.

*/
go(_, Result) :-
  nl, read(IN), go(IN, Result).

/*
Regla para iniciar análisis sintáctico.

Argumentos:
  String: Entrada del usuario a analizar.
  Tree: Se unifica con el arbol generado en el análisis.

*/
generarAST(String, Tree) :-
  %Convierte la entrada en tokens.
  tokenize(String, Tokens, [spaces(false), cntrl(false), punct(false)]),
  delete(Tokens, space(_), SpaceClean),
  delete(SpaceClean, punct(_), PunctClean),
  delete(PunctClean, cntrl(_), CntrlClean),
  oracion(Tree, CntrlClean, []), !.

/*
Regla para informar al usuario en caso de error.

Argumentos:
  Sin importancia.

*/
generarAST(_, _):-
  write("Lo lamento, no entendí su petición. ¿Podría repetirla por favor?"), fail.

/*
Regla para realizar análisis semántico.

Argumentos:
  Tree: AST.
  Result: valores extraídos de la entrada, unificada con [] si la entrada no posee datos.

*/
semantica(Tree, Result) :-
  Tree =.. [_, Head|Args],
  Head =.. [Tipo|Valores],
  (Tipo = saludo -> (expr_saludo(Valores, Args), Result = []) ;
    Tipo = solicitud -> (expr_solicitud(Valores, Args), Result = []) ;
      Tipo = despedida -> (expr_despedida(Valores, Args), Result = []) ;
        Tipo = ayuda -> (expr_ayuda(Valores), Result = []) ;
          Tipo = pronombre -> expr_datos(Valores, Args, Result) ;
            Tipo = sustantivo -> expr_datos(Valores, Args, Result) ;
              Tipo = articulo -> expr_solicitud(Valores, Args) ;
                Tipo = elemento -> expr_datos(Valores, Args, Result) ;
                  (write_ln("Error, el texto no tiene sentido, repita por favor"), read(IN), go(IN, _))).

/*
Reglas para analizar expresiones de saludos.

Argumentos:
  Primer parámetro: Datos acerca del saludo, sin importancia.
  Segundo parámetro: Valores que acompañan al saludo, sin importancia,
                     excepto en los casos que le sigue un comando(aterrizar, despegar, etc).

*/
expr_saludo(_, []) :-
  write("Bienvenido, ¿en que puedo ayudarle?"), nl, read(Input), go(Input, _), !.

expr_saludo(_, [program(_)]) :-
  write("Bienvenido, ¿en que puedo ayudarle?"), nl, read(Input), go(Input, _), !.

expr_saludo(_, [program(_),S]) :-
  S =.. [_|Rest],
  expr_solicitud(Rest, []), !.

expr_saludo(_, [solicitud(verbo(_), comando(C))]) :-
  expr_solicitud([verbo(_), comando(C)], []), !.

expr_saludo(_, [solicitud(verbo(_), articulo(_), comando(C))]) :-
  expr_solicitud([verbo(_), articulo(_), comando(C)], []), !.

expr_saludo(_, [articulo(_), lugar(_), pregunta(_), comando(C)]) :-
  expr_solicitud([verbo(_), articulo(_), comando(C)], []), !.

expr_saludo(_, [program(_), articulo(_), lugar(_), pregunta(_), comando(C)]) :-
  expr_solicitud([verbo(_), articulo(_), comando(C)], []), !.

expr_saludo(_, [pregunta(_), comando(C)]) :-
  expr_solicitud([verbo(_), articulo(_), comando(C)], []), !.

expr_saludo(_, [program(_), pregunta(_), comando(C)]) :-
  expr_solicitud([verbo(_), articulo(_), comando(C)], []), !.

/*
Reglas para analizar comandos.

Argumentos:
  Primer parámetro: Datos del comando, extrae el comando del AST.
  Segundo parámetro: Valores que acompañan al comando, utilizado al analizar preguntas.

*/
expr_solicitud([verbo(_), articulo(_), comando(C)], []) :-
  call(C), !.

expr_solicitud([verbo(_), comando(C)], []) :-
  call(C), !.

expr_solicitud([_], [lugar(_), pregunta(_), comando(C)]) :-
  call(C), !.

/*
Reglas para analizar expresiones de despedidas.

Argumentos:
  Primer parámetro: Datos de la despedida, permite chequear si se realizó un cambio y fuera.

*/
expr_despedida([final(_), articulo(_), final(_)], _) :-
  write("Llamar a despedida de cambio y fuera\n"), !.

expr_despedida([ok], _) :-
  write_ln("Adios"), !.

expr_despedida([gracias], _) :-
  write_ln("Con gusto"), !.

expr_despedida([adios], _) :-
  write_ln("Adios, que tenga buen viaje"), !.

expr_despedida([estado(_), adjetivo(_)], _) :-
  write_ln("Adios, que tenga buen viaje"), !.

/*
Reglas para analizar expresiones de ayuda.

Argumentos:
  Primer parámetro: Datos acerca de la ayuda, identifica el tipo de ayuda solicitada.

*/
expr_ayuda([mayday]) :-
  write("Llamar a ayuda Mayday\n"), !.

expr_ayuda([7500]) :-
  write("Llamar a ayuda 7500\n"), !.

/*
Reglas para analizar entradas con datos del usuario.

Argumentos:
  Primer parámetro: Identificador para analizar si es un dato del usuario o un dato general.
  Segundo parámetro: Expresión sintáctica de la oración.

*/
expr_datos([mi], [sustantivo(_), articulo(_), elemento(EL)], Result) :-
  Result = [EL], !.

expr_datos([mi], [sustantivo(_), articulo(_), elemento(POS1), elemento(POS2), elemento(POS3)], Result) :-
  Result = [POS1, POS2, POS3], !.

expr_datos([POS1], [elemento(POS2), elemento(POS3)], Result) :-
  Result = [POS1, POS2, POS3], !.

expr_datos([posicion], [elemento(POS1), elemento(POS2), elemento(POS3)], Result) :-
  Result = [POS1, POS2, POS3], !.

expr_datos([_], [sustantivo(_), articulo(_), sustantivo(_), articulo(_), elemento(EL), medida(_)], Result) :-
  Result = [EL], !.

expr_datos([_], [sustantivo(_), articulo(_), sustantivo(_), articulo(_), elemento(EL)], Result) :-
  Result = [EL], !.

expr_datos([_], [elemento(EL)], Result) :-
  Result = [EL], !.

expr_datos([Val], [], Result) :-
  Result = [Val], !.


/***
 *    ███████╗██╗███████╗████████╗███████╗███╗   ███╗ █████╗     ███████╗██╗  ██╗██████╗ ███████╗██████╗ ████████╗ ██████╗
 *    ██╔════╝██║██╔════╝╚══██╔══╝██╔════╝████╗ ████║██╔══██╗    ██╔════╝╚██╗██╔╝██╔══██╗██╔════╝██╔══██╗╚══██╔══╝██╔═══██╗
 *    ███████╗██║███████╗   ██║   █████╗  ██╔████╔██║███████║    █████╗   ╚███╔╝ ██████╔╝█████╗  ██████╔╝   ██║   ██║   ██║
 *    ╚════██║██║╚════██║   ██║   ██╔══╝  ██║╚██╔╝██║██╔══██║    ██╔══╝   ██╔██╗ ██╔═══╝ ██╔══╝  ██╔══██╗   ██║   ██║   ██║
 *    ███████║██║███████║   ██║   ███████╗██║ ╚═╝ ██║██║  ██║    ███████╗██╔╝ ██╗██║     ███████╗██║  ██║   ██║   ╚██████╔╝
 *    ╚══════╝╚═╝╚══════╝   ╚═╝   ╚══════╝╚═╝     ╚═╝╚═╝  ╚═╝    ╚══════╝╚═╝  ╚═╝╚═╝     ╚══════╝╚═╝  ╚═╝   ╚═╝    ╚═════╝
 *
 */

avion_p(cessna).
avion_p(beechcraft).
avion_p(embraer_phenom).

avion_m(boeing_717).
avion_m(embraer_190).
avion_m(airbus_a220).

avion_g(boeing_747).
avion_g(airbus_a340).
avion_g(airbus_a380).


maq(A,B) :- A > B.

ocupada(algunaPista).
:- dynamic
        ocupada/1.

pista(Tipo, _) :- not(ocupada(p1)), avion_p(Tipo), write("Permiso concedido. Aterrice en la pista P1."),
                  assertz(ocupada(p1)), !.

pista(Tipo, eo) :- not(ocupada(p21)), avion_m(Tipo), write("Permiso concedido. Aterrice en la pista P2-1."),
                   assertz(ocupada(p21));

                   not(ocupada(p21)), avion_p(Tipo), write("Permiso concedido. Aterrice en la pista P2-1."),
                   assertz(ocupada(p21)), !.

pista(Tipo, oe) :- not(ocupada(p22)), avion_m(Tipo), write("Permiso concedido. Aterrice en la pista P2-2."),
                   assertz(ocupada(p22));
                   not(ocupada(p22)), avion_p(Tipo), write("Permiso concedido. Aterrice en la pista P2-2."),
                   assertz(ocupada(p22)), !.

pista(Tipo, _) :- not(ocupada(p3)), avion_g(Tipo), write("Permiso concedido. Aterrice en la pista P3."),
                  assertz(ocupada(p3));
                  not(ocupada(p3)), avion_m(Tipo), write("Permiso concedido. Aterrice en la pista P3."),
                  assertz(ocupada(p3));
                  not(ocupada(p3)), avion_p(Tipo), write("Permiso concedido. Aterrice en la pista P3."),
                  assertz(ocupada(p3)), !.


/*  Función general de aterrizaje,
 *  llama a todas las demás funciones para
 *  verificar si el aterrizaje es permitido y asigna una pista.
 */
aterrizar :- at_nombre, at_tipo(Tipo), at_velocidad, at_direccion(Dir),
             revisar_viento, revisar_peso, pista(Tipo, Dir).


revisar_viento :- write_ln("¿Cuál es la velocidad del viento actual?"), read(Vel), go(Vel, R), first(R, N), velocidad_viento(N).

velocidad_viento(Vel) :- maq(Vel, 250) -> write("Velocidad del viento mayor a la permitida. Abortar aterrizaje."), fail ;
true.


revisar_peso :- write_ln("¿Cuál es el peso de su vehículo?"), read(Pes), go(Pes, R), first(R, N), cantidad_peso(N).

cantidad_peso(Pes) :- maq(Pes, 50000) -> write("Peso excede capacidad del aeropuerto. Abortar aterrizaje."), fail ;
true.

first([E|_], E).

/*  Obtiene el nombre del piloto.
 */
at_nombre :- write_ln("¿Cuál es su nombre?"),
             read(Nombre), go(Nombre, Result), write("¡BIenvenido "),
             first(Result, Name), write(Name), write_ln("!").

/*  Verifica que el tipo de avión que está usando el
 *  piloto existe.
 */
at_tipo(Tipo) :- write_ln("¿Qué tipo de avión está usando?"),
           read(Input), go(Input, R), first(R, Elm), tipo(Elm), Tipo = Elm.

tipo(Tipo) :- avion_p(Tipo), !;
              avion_m(Tipo), !;
              avion_g(Tipo), !;
              write("Permiso denegado: Avión no compatible con este aeropuerto."), !, fail.

/*  Obtiene la velocidad del avión. Si, es alta, le pide que la reduzca.
 */
at_velocidad :- write_ln("¿Cuál es su velocidad actual?"),
                read(Vel), go(Vel, R), first(R, Elm), velocidad(Elm).

velocidad(Vel) :- maq(Vel, 100) -> write("Reduzca su velocidad a 100"), fail ;
                  true.


/*  Obtiene la dirección del avión..
 */
at_direccion(Dir) :- write_ln("¿Cuál es su dirección actual?"),
                     read(Input), go(Input, Result), first(Result, R), Dir = R.

/*  Función general de despegue,
 *  llama a todas las demás funciones para
 *  verificar si el despegue es permitido y asigna una pista.
 */
despegar :- at_nombre, revisar_viento, revisar_peso,
            at_tipo(Tipo), pista(Tipo).

pista(Tipo) :- not(ocupada(p1)), avion_p(Tipo), write("Permiso concedido. Despegue desde la pista P1."),
                  assertz(ocupada(p1)), !.

pista(Tipo) :- not(ocupada(p21)), avion_m(Tipo), write("Permiso concedido. Despegue desde la pista P2-1."),
                   assertz(ocupada(p21));

                   not(ocupada(p21)), avion_p(Tipo), write("Permiso concedido. Despegue desde la pista P2-1."),
                   assertz(ocupada(p21)), !.

pista(Tipo) :- not(ocupada(p22)), avion_m(Tipo), write("Permiso concedido. Despegue desde la pista P2-2."),
                   assertz(ocupada(p22));
                   not(ocupada(p22)), avion_p(Tipo), write("Permiso concedido. Despegue desde la pista P2-2."),
                   assertz(ocupada(p22)), !.

pista(Tipo) :- not(ocupada(p3)), avion_g(Tipo), write("Permiso concedido. Despegue desde la pista P3."),
                  assertz(ocupada(p3));
                  not(ocupada(p3)), avion_m(Tipo), write("Permiso concedido. Despegue desde la pista P3."),
                  assertz(ocupada(p3));
                  not(ocupada(p3)), avion_p(Tipo), write("Permiso concedido. Despegue desde la pista P3."),
                  assertz(ocupada(p3)), !.

mayday :- write("Identifíquese."),
          read(_),
          write_ln("¿Cuál es su emergencia?"),
          read(_),
          write_ln("¿Cuál es su velocidad acutal?"),
          read(_),
          write_ln("¿Cuál es su posición acutal?"),
          read(_),
          write_ln("¿Cuál es el tipo de su avión?"),
          read(_),
          write("Pista despejada y servicios de emergencia  llamados, aterrice en la pista P3").


secuestro :- write("Identifíquese."),
             read(_),
             write_ln("¿Cuál es su velocidad acutal?"),
             read(_),
             write_ln("¿Cuál es su posición acutal?"),
             read(_),
             write_ln("¿Cuál es el tipo de su avión?"),
             read(_),
             write("Dirjase a las siguientes coordenadas: "),
             read(_),
             write_ln("ALGO").
