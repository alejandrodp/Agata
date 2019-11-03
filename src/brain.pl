avion_p(cessna).
avion_p(beechcraft).
avion_p(embraer_phenom).

avion_m(boeing_717).
avion_m(embraer_190).
avion_m(airbus_A220).

avion_g(boeing_747).
avion_g(airbus_A340).
avion_g(airbus_A380).


maq(A,B) :- A > B.

ocupada(pista).

pista(Tipo, _) :- not(ocupada(p1)), avion_p(Tipo), write("Permiso concedido. Aterrice en la pista P1."),
                  assert(ocupada(p1)), !.

pista(Tipo, eo) :- not(ocupada(p21)), avion_m(Tipo), write("Permiso concedido. Aterrice en la pista P2-1."),
                   assert(ocupada(p21));

                   not(ocupada(p21)), avion_p(Tipo), write("Permiso concedido. Aterrice en la pista P2-1."),
                   assert(ocupada(p21)), !.

pista(Tipo, oe) :- not(ocupada(p22)), avion_m(Tipo), write("Permiso concedido. Aterrice en la pista P2-2."),
                   assert(ocupada(p22));
                   not(ocupada(p22)), avion_p(Tipo), write("Permiso concedido. Aterrice en la pista P2-2."),
                   assert(ocupada(p22)), !.

pista(Tipo, _) :- not(ocupada(p3)), avion_g(Tipo), write("Permiso concedido. Aterrice en la pista P3."),
                  assert(ocupada(p3));
                  not(ocupada(p3)), avion_m(Tipo), write("Permiso concedido. Aterrice en la pista P3."),
                  assert(ocupada(p3));
                  not(ocupada(p3)), avion_p(Tipo), write("Permiso concedido. Aterrice en la pista P3."),
                  assert(ocupada(p3)), !.


/*  Función general de aterrizaje,
 *  llama a todas las demás funciones para
 *  verificar si el aterrizaje es permitido y asigna una pista.
 */
aterrizar :- at_nombre, at_tipo(Tipo), at_velocidad, at_direccion(Dir),
             at_pista(Tipo, Dir).

/*  Función general de despegue,
 *  llama a todas las demás funciones para
 *  verificar si el despegue es permitido y asigna una pista.
 */
despegar :- at_nombre, at_tipo(Tipo), de_pista(Tipo).


/*  Obtiene el nombre del piloto.
 */ 
at_nombre :- write_ln("¿Cuál es su nombre?"),
             read(Nombre), write("¡BIenvenido "),
             write(Nombre), write_ln("!").

/*  Verifica que el tipo de avión que está usando el
 *  piloto existe.
 */
at_tipo(Tipo) :- write_ln("¿Qué tipo de avión está usando?"),
           read(Input), tipo(Input), Tipo = Input.

tipo(Tipo) :- avion_p(Tipo);
              avion_m(Tipo);
              avion_g(Tipo);
              write("Permiso denegado: Avión no compatible con este aeropuerto."), !, fail.

/*  Obtiene la velocidad del avión. Si, es alta, le pide que la reduzca.
 */ 
at_velocidad :- write_ln("¿Cuál es su velocidad actual?"),
                read(Vel), velocidad(Vel).

velocidad(Vel) :- maq(Vel, 100) -> write("Reduzca su velocidad a 100"), fail ;
                  true.


/*  Obtiene la dirección del avión..
 */ 
at_direccion(Dir) :- write_ln("¿Cuál es su dirección actual?"),
                     read(Input), Dir = Input.

at_pista(Tipo, Dir) :- pista(Tipo, Dir).

de_pista(Tipo) :- pista(Tipo, _).


