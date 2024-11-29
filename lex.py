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

precedence = (
    ('left', 'ADICAO', 'SUBTRACAO'),
    ('left', 'MULTIPLICACAO', 'DIVISAO'),
)

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
             | FRASE
             | expression'''
    p[0] = p[1]  # O valor da variável será o valor do tipo correspondente

# Regra para expressões de adição
def p_expression_adicao(p):
    'expression : expression ADICAO expression'
    p[0] = p[1] + p[3]

# Regra para expressões de adição de variaveis
def p_expression_adicao_variaveis(p):
    'expression : VARIAVEL ADICAO VARIAVEL'
    p[0] = p[1] + p[3]

# Regra para expressões de subtração
def p_expression_subtracao(p):
    'expression : expression SUBTRACAO expression'
    p[0] = p[1] - p[3]

# Regra para expressões de subtração de variaveis
def p_expression_subtracao_variaveis(p):
    'expression : VARIAVEL SUBTRACAO VARIAVEL'
    p[0] = p[1] + p[3]

# Regra para expressões de multiplicacao
def p_expression_multiplicacao(p):
    'expression : expression MULTIPLICACAO expression'
    p[0] = p[1] * p[3]

# Regra para expressões de multiplicacao de variaveis
def p_expression_multiplicacao_variaveis(p):
    'expression : VARIAVEL MULTIPLICACAO VARIAVEL'
    p[0] = p[1] + p[3]

# Regra para expressões de divisao
def p_expression_divisao(p):
    'expression : expression DIVISAO expression'
    p[0] = p[1] / p[3]

# Regra para expressões de divisao de variaveis
def p_expression_divisao_variaveis(p):
    'expression : VARIAVEL DIVISAO VARIAVEL'
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

def p_parentese_expr(p):
    'term : ESQPARENTESE expression DIRPARENTESE'
    p[0] = p[2]

# Regra para o laço 'for'
def p_for(p):
    '''expression : FOR VARIAVEL ENTRE FAIXA ESQPARENTESE INTEIRO DIRPARENTESE FINALEXPRESSAO bloco'''
    p[0] = ('for', p[2], p[6])  # Armazena o laço: variável, valor inicial, valor final, e o bloco do corpo

# Regra para o corpo do 'for', que pode ser uma ou mais expressões
def p_bloco(p):
    '''bloco : expression
             | bloco expression'''
    p[0] = p[1:]  # Armazena o corpo do bloco com suas expressõe

def p_enquanto(p):
    '''expression : ENQUANTO VARIAVEL MENOR INTEIRO FINALEXPRESSAO bloco
                  | ENQUANTO VARIAVEL MAIOR INTEIRO FINALEXPRESSAO bloco
                  | ENQUANTO VARIAVEL IGUAL INTEIRO FINALEXPRESSAO bloco
                  | ENQUANTO VARIAVEL IGUALDADE INTEIRO FINALEXPRESSAO bloco
                  | ENQUANTO VARIAVEL DIFERENTE INTEIRO FINALEXPRESSAO bloco
                  | ENQUANTO VARIAVEL MENORIGUAL INTEIRO FINALEXPRESSAO bloco
                  | ENQUANTO VARIAVEL MAIORIGUAL INTEIRO FINALEXPRESSAO bloco'''
    p[0] = ('while', p[2], p[4])  # Armazena a condição e o bloco do laço


# Regra para o comando 'escreva'
def p_escreva_expressao(p):
    'expression : ESCREVA ESQPARENTESE expression DIRPARENTESE FINALEXPRESSAO'
    p[0] = (p[3])  # Armazena o comando 'escreva' e a expressão a ser impressa

def p_escreva_variavel(p):
    'expression : ESCREVA ESQPARENTESE VARIAVEL DIRPARENTESE FINALEXPRESSAO'
    p[0] = (p[3])  # Armazena o comando 'escreva' e a expressão a ser impressa


def p_error(p):
    if p:
        print(f"Erro de sintaxe próximo ao token: {p.value}")
    else:
        print("Analise concluida, resultado nulo")

parser = yacc.yacc()

teste = '''
enquanto i >= 5;
escreva(i);

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