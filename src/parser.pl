oracion(tree(PR, SU, ES, EL)) --> pronombre(PR), sustantivo(SU), articulo(ES), elemento(EL), !.
oracion(tree(SN, SV, SS)) --> saludo(SN), program(SV), solicitud(SS), !.
oracion(tree(SN, SV)) --> saludo(SN), program(SV), !.
oracion(tree(SN, SV)) --> saludo(SN), solicitud(SV), !.
oracion(tree(SN)) --> saludo(SN), !.
oracion(tree(SV)) --> solicitud(SV), !.
oracion(tree(SV)) --> despedida(SV), !.
oracion(tree(SV)) --> ayuda(SV), !.

ayuda(ayuda("mayday")) --> ["mayday"].
ayuda(ayuda("7500")) --> ["7500"].

pronombre(pronombre("mi")) --> ["mi"].

sustantivo(sustantivo("avion")) --> ["avion"].
sustantivo(sustantivo("placa")) --> ["placa"].
sustantivo(sustantivo("direccion")) --> ["direccion"].
sustantivo(sustantivo("velocidad")) --> ["velocidad"].


despedida(despedida("ok")) --> ["ok"].
despedida(agradecimiento("gracias")) --> ["gracias"].
despedida(despedida(S, D)) --> estado(S), adjetivo(D).
despedida(despedida(S, Y, D)) --> final(S), articulo(Y), final(D).

final(final("cambio")) --> ["cambio"].
final(final("fuera")) --> ["fuera"].


estado(estado("esta")) --> ["esta"].


adjetivo(adjetivo("bien")) --> ["bien"].
adjetivo(adjetivo("buenos")) --> ["buenos"].
adjetivo(adjetivo("buenas")) --> ["buenas"].

saludo(saludo(S, D)) --> adjetivo(S), saludo_tipo(D).
saludo(saludo("hola")) --> ["hola"].

saludo_tipo(saludo_tipo("dias")) --> ["dias"].
saludo_tipo(saludo_tipo("noches")) --> ["noches"].

program(program("agata")) --> ["agata"].
program(program("maycey")) --> ["maycey"].

solicitud(solicitud(VB, CM)) --> verbo(VB), comando(CM).
solicitud(solicitud(VB, AR, CM)) --> verbo(VB), articulo(AR), comando(CM).

verbo(verbo("quiero")) --> ["quiero"].
verbo(verbo("deseo")) --> ["deseo"].
verbo(verbo("voy")) --> ["voy"].
verbo(verbo("tengo")) --> ["tengo"].

articulo(articulo("a")) --> ["a"].
articulo(articulo("que")) --> ["que"].
articulo(articulo("es")) --> ["es"].
articulo(articulo("y")) --> ["y"].


comando(comando("aterrizar")) --> ["aterrizar"].
comando(comando("despegar")) --> ["despegar"].

elemento(elemento(EL)) --> [EL].

pre_proc(String, List) :-
  split_string(String, ' ', '', L),
  delete(L, ',', List).

generateTree(String, Tree) :-
  pre_proc(String, R),
  oracion(Tree, R, []).
