import ply.lex as lex
import ply.yacc as yacc



# -------------- ANALISE LEXICA -----------------------
reserved = {
    'for': 'FOR',
    'entre':'ENTRE',
    'faixa':'FAIXA',
    'se': 'SE',
    'entao': 'ENTAO',
    'enquanto': 'ENQUANTO',
    'escreva': 'ESCREVA',
    'leia': 'LEIA'
}

tokens = [
    'INTEIRO',
    'DECIMAL',
    'FRASE',
    'ADICAO',
    'SUBTRACAO',
    'MULTIPLICACAO',
    'DIVISAO',
    'ESQPARENTESE',
    'DIRPARENTESE',
    'VARIAVEL',
    'ESQCHAVE',
    'DIRCHAVE',
    'VIRGULA',
    'MENOR',
    'MAIOR',
    'IGUAL',
    'IGUALDADE',
    'DIFERENTE',
    'MENORIGUAL',
    'MAIORIGUAL'
] + list(reserved.values())

t_ADICAO = r'\+'
t_SUBTRACAO = r'-'
t_MULTIPLICACAO = r'\*'
t_DIVISAO = r'/'
t_ESQPARENTESE = r'\('
t_DIRPARENTESE = r'\)'
t_ESQCHAVE = r'\{'
t_DIRCHAVE = r'\}'
t_VIRGULA = r','


def t_IGUALDADE(t):
    r'=='
    return t

def t_MENORIGUAL(t):
    r'<='
    return t

def t_MAIORIGUAL(t):
    r'>='
    return t
    
def t_DIFERENTE(t):
    r'!='
    return t

def t_MENOR(t):
    r'<'
    return t

def t_MAIOR(t):
    r'>'
    return t

def t_IGUAL(t):
    r'='
    return t

def t_DECIMAL(t):
    r'\d+\.\d+'
    t.value = float(t.value)
    return t

def t_INTEIRO(t):
    r'\d+'
    t.value = int(t.value)
    return t

def t_FRASE(t):
    r'\"([^\\"]|\\.)*\"|\'([^\\\']|\\.)*\''
    t.value = t.value[1:-1] #remove as aspas
    return t

def t_IDENTIFIER (t):
    r'[a-zA-Z_][a-zA-Z0-9_]*'
    t.type = reserved.get(t.value, 'VARIAVEL')
    return t

def t_newline(t):
    r'\n+'
    t.lexer.lineno += len(t.value)

t_ignore = ' \t'

def t_error(t):
    print("Caracter Ilegal! '%s'" % t.value[0])
    t.lexer.skip(1)

lexer = lex.lex()


teste = ''' 
escreva x
enquanto x < 10 {
    escreva y
}
'''

lexer.input(teste)

while True:
    tok= lexer.token()
    if not tok:
        break
    print(tok.type, tok.value)