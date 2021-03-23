#Se importan las librerias necesarias
import ply.lex as lex
import ply.yacc as yacc

# Se declaran los tokens que se utilizarán
tokens  = (
    'ID',
    'STRING',
    'INT',
    'DOUBLE',
    'FLOAT',
    'BOOLEAN',
    'PARIZQ',
    'PARDER',
    'CORIZQ',
    'CORDER',
    'IGUAL',
    'MAS',
    'MENOS',
    'POR',
    'DIVIDIDO',
    'MENQUE',
    'MAYQUE',
    'IGUALQUE',
    'NIGUALQUE',
    'TRUE',
    'FALSE',
    'CADENA',
    'DECIMAL',
    'ENTERO',
    'PTCOMA'
)

#Tokens y sus asignaciones
t_PARIZQ    = r'\('
t_PARDER    = r'\)'
t_CORIZQ    = r'\['
t_CORDER    = r'\]'
t_IGUAL     = r'='
t_MAS       = r'\+'
t_MENOS     = r'-'
t_POR       = r'\*'
t_DIVIDIDO  = r'/'
t_PTCOMA    = r';'
t_MENQUE    = r'<'
t_MAYQUE    = r'>'
t_IGUALQUE  = r'=='
t_NIGUALQUE = r'!='

#Función para cadenas de texto
def t_CADENA(t):
    r'\".*?\"'
    t.value = t.value[1:-1] # remuevo las comillas
    return t 

#Función para la palabra int
def t_INT(t):
     r'int'
     return t

#Función para la palabra string
def t_STRING(t):
     r'String'
     return t

#Función para la palabra double
def t_DOUBLE(t):
     r'double'
     return t

#Función para la palabra float
def t_FLOAT(t):
     r'float'
     return t

#Función para la palabra boolean
def t_BOOLEAN(t):
     r'boolean'
     return t

#Función para la palabra true
def t_TRUE(t):
     r'true'
     return t

#Función para la palabra false
def t_FALSE(t):
     r'false'
     return t

#Definimos esta función para nombre de variables
def t_ID(t):
     r'[a-zA-Z_][a-zA-Z_0-9]*'
     return t

#Definimos esta función unicamente para decimales
def t_DECIMAL(t):
    r'\d+\.\d+'
    try:
        t.value = float(t.value)
    except ValueError:
        print("Floaat value too large %d", t.value)
        t.value = 0
    return t

#Definimos esta función unicamente para números enteros
def t_ENTERO(t):
    r'\d+'
    try:
        t.value = int(t.value)
    except ValueError:
        print("Integer value too large %d", t.value)
        t.value = 0
    return t

#Caracteres ignorados
t_ignore = " \t"

#Definimos esta función para comentarios simples con: // ...
def t_COMENTARIO_SIMPLE(t):
    r'//.*\n'
    t.lexer.lineno += 1

#Definimos esta función para salto de línea
def t_newline(t):
    r'\n+'
    t.lexer.lineno += t.value.count("\n")

#Definimos esta función para carácteres ilegales
def t_error(t):
    print("Carácter Ilegal '%s'" % t.value[0])
    t.lexer.skip(1)
    
#Asociación de operadores y precedencia
precedence = (
    ('left','MAS','MENOS'),
    ('left','POR','DIVIDIDO'),
    ('right','UMENOS'),
    )

#A continuación se declaran varias funciones las cuales nos servirán para poder validar los errores sintacticos.
#En este apartado se hace uso de los tokens que fueron declarados al inicio.

def p_instrucciones_lista(t):
    '''instrucciones    : instruccion instrucciones
                        | instruccion '''

#Definimos esta función para la declaración y asignación de variables
def p_instrucciones_variables(t):
    '''instruccion : INT ID IGUAL expresion2 PTCOMA
                   | INT ID IGUAL ENTERO PTCOMA
                   | STRING ID IGUAL CADENA PTCOMA
                   | STRING ID IGUAL CADENA MAS CADENA PTCOMA
                   | DOUBLE ID IGUAL DECIMAL PTCOMA
                   | DOUBLE ID IGUAL expresion PTCOMA
                   | FLOAT ID IGUAL DECIMAL PTCOMA
                   | FLOAT ID IGUAL expresion PTCOMA
                   | BOOLEAN ID IGUAL expresion3 PTCOMA
                   | INT ID PTCOMA 
                   | STRING ID PTCOMA
                   | DOUBLE ID PTCOMA
                   | FLOAT ID PTCOMA 
                   | BOOLEAN ID PTCOMA '''
                
#Definimos esta función para expresiones binarias, ya sea con enteros o decimales
def p_expresion_binaria(t):
    '''expresion : expresion MAS expresion
                  | expresion MENOS expresion
                  | expresion POR expresion
                  | expresion DIVIDIDO expresion'''
    if t[2] == '+'  : t[0] = t[1] + t[3]
    elif t[2] == '-': t[0] = t[1] - t[3]
    elif t[2] == '*': t[0] = t[1] * t[3]
    elif t[2] == '/': t[0] = t[1] / t[3]

#Definimos esta función para expresiones binarias unicamente con números enteros
def p_expresion_binariaInt(t):
    '''expresion2 : ENTERO MAS ENTERO
                  | ENTERO MENOS ENTERO
                  | ENTERO POR ENTERO
                  | ENTERO DIVIDIDO ENTERO'''
    if t[2] == '+'  : t[0] = t[1] + t[3]
    elif t[2] == '-': t[0] = t[1] - t[3]
    elif t[2] == '*': t[0] = t[1] * t[3]
    elif t[2] == '/': t[0] = t[1] / t[3]

#Definimos esta función para expresiones booleanas con signos
def p_expresion_booleana(t):
    '''expresion3 : expresion MAYQUE expresion
                  | expresion MENQUE expresion
                  | expresion IGUALQUE expresion
                  | expresion NIGUALQUE expresion'''
    if t[2] == '>'  : t[0] = t[1] > t[3]
    elif t[2] == '<': t[0] = t[1] < t[3]
    elif t[2] == '==': t[0] = t[1] == t[3]
    elif t[2] == '!=': t[0] = t[1] != t[3]

#Definimos esta función para expresiones booleanas de tipo true y false
def p_expresion_booleana2(t):
    '''expresion3 : TRUE
                  | FALSE'''

#Definimos esta función para expresiones unarias
def p_expresion_unaria(t):
    'expresion : MENOS expresion %prec UMENOS'
    t[0] = -t[2]

#Definimos esta función para expresiones entre paréntesis
def p_expresion_agrupacion(t):
    'expresion : PARIZQ expresion PARDER'
    t[0] = t[2]

#Definimos esta función para expresiones de números enteros entre paréntesis
def p_expresion_agrupacion2(t):
    'expresion2 : PARIZQ expresion2 PARDER'
    t[0] = t[2]

#Definimos esta función para valores numéricos de tipo entero y decimal
def p_expresion_number(t):
    '''expresion    : ENTERO
                    | DECIMAL'''
    t[0] = t[1]

#Definimos esta función para los posibles errores detectados durante el análisis
def p_error(t):
    print("Error de asignación en '%s'" % t.value)

#Construimos el analizador léxico y sintáctico
lexer = lex.lex()
parser = yacc.yacc() 

f = open("./entrada.txt", "r") #Se abre el archivo de entrada
input = f.read() #Se lee el archivo de entrada

print (chr(27)+"[0;36m"+"INICIA ANALISIS SEMÁNTICO"+chr(27)+"[0m") #Mensaje inicio de análisis
parser.parse(input, tracking=False) #En esta parte se mostrarían los errores en caso de existir alguno
print (chr(27)+"[0;36m"+"TERMINA ANALISIS SEMÁNTICO"+chr(27)+"[0m") #Mensaje fin de análisis

#Se imprimen los integrantes del equipo
print("Programa elaborado por:"+"\n"
        +"<<Mis Oy Cristina de Jesus>>"+"\n"
        +"<<Poot Can Gener Emmanuel>>"+"\n"
        +"<<Tun Tun José Natividad>>"+"\n"
        +"<<Uicab Balam Nanci Arai>>")