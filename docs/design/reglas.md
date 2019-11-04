# Funciones lógicas

**(bCEj name_list)**
- recibe: una lista con los nombres de cada jugador de la partida
- retorna: inicio de interfaz gráfica (llamada a show frame)
- descripción: inicia la GUI


**go(Input, Result)**
- descripción: Regla principal para inciar el añalisis de la entrada del usuario.
- Argumentos: input: entrada del usuario a analizar. Result: valores extraídos de la entrada, unificada con [] si la entrada no posee datos.

**generarAST(String, Tree)**
- descripción: Regla para iniciar análisis sintáctico.
- Argumentos: String: Entrada del usuario a analizar. Tree: Se unifica con el arbol generado en el análisis.

**semantica(Tree, Result)**
- descripción: Regla para realizar análisis semántico.
- Argumentos: Tree: AST. Result: valores extraídos de la entrada, unificada con [] si la entrada no posee datos.

**expr_saludo(_, [])**
- descripción: Regla para realizar análisis semántico.
- Argumentos: Tree: AST. Result: valores extraídos de la entrada, unificada con [] si la entrada no posee datos.

