**go(Input, Result)**
- descripción: Regla principal para inciar el añalisis de la entrada del usuario.
- Argumentos: input: entrada del usuario a analizar. Result: valores extraídos de la entrada, unificada con [] si la entrada no posee datos.

**generarAST(String, Tree)**
- descripción: Regla para iniciar análisis sintáctico.
- Argumentos: String: Entrada del usuario a analizar. Tree: Se unifica con el arbol generado en el análisis.

**semantica(Tree, Result)**
- descripción: Regla para realizar análisis semántico.
- Argumentos: Tree: AST. Result: valores extraídos de la entrada, unificada con [] si la entrada no posee datos.

**expr_saludo(Valores, Args)**
- descripción: Reglas para analizar expresiones de saludos.
- Argumentos: Valores: Datos acerca del saludo, sin importancia. Args: Valores que acompañan al saludo, sin importancia, excepto en los casos que le sigue un comando(aterrizar, despegar, etc).

**expr_solicitud(Valores, Args)**
- descripción: Reglas para analizar comandos.
- Argumentos: Valores: Datos del comando, extrae el comando del AST. Args: Valores que acompañan al comando, utilizado al analizar preguntas.

**expr_despedida(Valores)**
- descripción: Reglas para analizar expresiones de despedidas.
- Argumentos: Valores: Datos de la despedida, permite chequear si se realizó un cambio y fuera.

**expr_ayuda(Valores)**
- descripción: Reglas para analizar expresiones de ayuda.
- Argumentos: Datos acerca de la ayuda, identifica el tipo de ayuda solicitada.

**expr_datos(Valores, Args, Result)**
- descripción: Reglas para analizar entradas con datos del usuario.
- Argumentos: Valores: Identificador para analizar si es un dato del usuario o un dato general. Args: Expresión sintáctica de la oración. Result: Variable para unificar con los datos del usuario en una lista.