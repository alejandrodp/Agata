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


revisar_viento :- write_ln("¿Cuál es la velocidad del viento actual?"), read(Vel), velocidad_viento(Vel).

velocidad_viento(Vel) :- maq(Vel, 250) -> write("Velocidad del viento mayor a la permitida. Abortar aterrizaje."), fail ;
true.


revisar_peso :- write_ln("¿Cuál es el peso de su vehículo?"), read(Pes), cantidad_peso(Pes).

cantidad_peso(Pes) :- maq(Pes, 50000) -> write("Peso excede capacidad del aeropuerto. Abortar aterrizaje."), fail ;
true.


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

tipo(Tipo) :- avion_p(Tipo), !;
              avion_m(Tipo), !;
              avion_g(Tipo), !;
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

/*  Función general de despegue,
 *  llama a todas las demás funciones para
 *  verificar si el despegue es permitido y asigna una pista.
 */
despegar :- at_nombre, revisar_viento, revisar_peso,
            at_tipo(Tipo), pista(Tipo).

pista(Tipo) :- not(ocupada(p1)), avion_p(Tipo), write("Permiso concedido. Despeje desde la pista P1."),
                  assertz(ocupada(p1)), !.

pista(Tipo) :- not(ocupada(p21)), avion_m(Tipo), write("Permiso concedido. Despeje desde la pista P2-1."),
                   assertz(ocupada(p21));

                   not(ocupada(p21)), avion_p(Tipo), write("Permiso concedido. Despeje desde la pista P2-1."),
                   assertz(ocupada(p21)), !.

pista(Tipo) :- not(ocupada(p22)), avion_m(Tipo), write("Permiso concedido. Despeje desde la pista P2-2."),
                   assertz(ocupada(p22));
                   not(ocupada(p22)), avion_p(Tipo), write("Permiso concedido. Despeje desde la pista P2-2."),
                   assertz(ocupada(p22)), !.

pista(Tipo) :- not(ocupada(p3)), avion_g(Tipo), write("Permiso concedido. Despeje desde la pista P3."),
                  assertz(ocupada(p3));
                  not(ocupada(p3)), avion_m(Tipo), write("Permiso concedido. Despeje desde la pista P3."),
                  assertz(ocupada(p3));
                  not(ocupada(p3)), avion_p(Tipo), write("Permiso concedido. Despeje desde la pista P3."),
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
          write("Pistas despejadas y servicios de emergencia  llamados, aterrice en la pista P3").


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



