import ply.lex as lex
import ply.yacc as yacc



# -------------- ANALISE LEXICA ----------------------------------------
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
    'MAIORIGUAL',
    'FINALEXPRESSAO'
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


def t_FINALEXPRESSAO(t):
    r';'
    return t

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

def t_VARIAVEL (t):
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

#---------Análise Sintática e Semântica-------------------------------------
# Regra para declarar e atribuir variáveis (com múltiplas declarações)
def p_programa(p):
    'programa : programa expression'
    p[0] = p[1] + [p[2]]  # Adiciona a nova expressão na lista de resultados

def p_programa_vazio(p):
    'programa : '
    p[0] = []  # Retorna uma lista vazia para o caso de não haver mais expressões

# Regra para declaração e atribuição de variáveis
def p_declaracao_variavel(p):
    'expression : VARIAVEL IGUAL valor FINALEXPRESSAO'
    p[0] = (p[1], p[3])  # Armazena o nome da variável e seu valor

# Regra para os valores possíveis (inteiro, decimal, ou string)
def p_valor(p):
    '''valor : INTEIRO
             | DECIMAL
             | FRASE'''
    p[0] = p[1]  # O valor da variável será o valor do tipo correspondente

# Regra para expressões de adição
def p_expression_adicao(p):
    'expression : expression ADICAO term'
    p[0] = p[1] + p[3]

# Regra para um termo simples
def p_expression_term(p):
    'expression : term'
    p[0] = p[1]

# Regra para um número (inteiro)
def p_term_number(p):
    'term : INTEIRO'
    p[0] = p[1]

# Regra para um número decimal
def p_term_decimal(p):
    'term : DECIMAL'
    p[0] = p[1]

# Regra para uma string
def p_term_string(p):
    'term : FRASE'
    p[0] = p[1]

def p_error(p):
    if p:
        print(f"Erro de sintaxe próximo ao token: {p.value}")
    else:
        print("Erro de sintaxe: EOF inesperado!")

parser = yacc.yacc()

teste = '''
a = 2;
teste = "teste";
b = 3;

'''

# Etapa 1: Análise Léxica
print("Tokens gerados pela análise léxica:")
lexer.input(teste)
for tok in lexer:
    print(f"{tok.type}: {tok.value}")

# Etapa 2: Análise Sintática
print("\nResultado da análise sintática:")
resultado = parser.parse(teste)
print(f"Resultado: {resultado}")