import ply.lex as lex
import ply.yacc as yacc



# -------------- ANALISE LEXICA ----------------------------------------
reserved = {
    'for': 'FOR',
    'entre':'ENTRE',
    'faixa':'FAIXA',
    'se': 'SE',
    'senao': 'SENAO',
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
    'DOISPONTOS',
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

def t_DOISPONTOS(t):
    r':'
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
    p[0] = ('atribuir', p[1], p[3])  # Ajuste para indicar que é uma atribuição

def p_declaracao_variavel_expressao(p):
    'expression : VARIAVEL IGUAL expression FINALEXPRESSAO'
    p[0] = ('atribuir',p[1], p[3])  # Armazena o nome da variável e seu valor

# Regra para declaração e atribuição de variáveis
def p_declaracao_variavel_leia(p):
    'expression : VARIAVEL IGUAL LEIA ESQPARENTESE DIRPARENTESE FINALEXPRESSAO'
    p[0] = ('atribuir',p[1], p[3])  # Armazena o nome da variável e seu valor


# Regra para os valores possíveis (inteiro, decimal, ou string)
def p_valor(p):
    '''valor : INTEIRO
             | DECIMAL
             | FRASE
             | expression'''
    p[0] = p[1]  # O valor da variável será o valor do tipo correspondente

# Regra para expressões de adição
def p_expression_adicao(p):
    '''expression : expression ADICAO expression'''
    p[0] = (f"{p[1]} + {p[3]}")

# Regra para expressões de adição de variaveis
def p_expression_adicao_variaveis(p):
    'expression : VARIAVEL ADICAO VARIAVEL'
    p[0] = (f"{p[1]} + {p[3]}")

# Regra para expressões de subtração
def p_expression_subtracao(p):
    'expression : expression SUBTRACAO expression'
    p[0] = (f"{p[1]} - {p[3]}")

# Regra para expressões de subtração de variaveis
def p_expression_subtracao_variaveis(p):
    'expression : VARIAVEL SUBTRACAO VARIAVEL'
    p[0] = (f"{p[1]} - {p[3]}")

# Regra para expressões de multiplicacao
def p_expression_multiplicacao(p):
    'expression : expression MULTIPLICACAO expression'
    p[0] = (f"{p[1]} * {p[3]}")

# Regra para expressões de multiplicacao de variaveis
def p_expression_multiplicacao_variaveis(p):
    'expression : VARIAVEL MULTIPLICACAO VARIAVEL'
    p[0] = (f"{p[1]} * {p[3]}")

# Regra para expressões de divisao
def p_expression_divisao(p):
    'expression : expression DIVISAO expression'
    p[0] = (f"{p[1]} / {p[3]}")

# Regra para expressões de divisao de variaveis
def p_expression_divisao_variaveis(p):
    'expression : VARIAVEL DIVISAO VARIAVEL'
    p[0] = (f"{p[1]} / {p[3]}")

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

# Regra para o laço 'for' com apenas um valor inteiro
def p_for(p):
    '''expression : FOR VARIAVEL ENTRE FAIXA ESQPARENTESE INTEIRO DIRPARENTESE DOISPONTOS bloco'''
    p[0] = ('for', p[2], p[6], p[9])  # 'for', variável, valor único (início e fim igual), corpo do bloco



# Regra para o bloco (aceitar apenas uma linha de instrução)
def p_bloco(p):
    '''bloco : expression'''
    p[0] = [p[1]]  # Um bloco agora é uma lista com uma única expressão


def p_enquanto(p):
    '''expression : ENQUANTO VARIAVEL MENOR INTEIRO DOISPONTOS bloco
                  | ENQUANTO VARIAVEL MAIOR INTEIRO DOISPONTOS bloco
                  | ENQUANTO VARIAVEL IGUAL INTEIRO DOISPONTOS bloco
                  | ENQUANTO VARIAVEL IGUALDADE INTEIRO DOISPONTOS bloco
                  | ENQUANTO VARIAVEL DIFERENTE INTEIRO DOISPONTOS bloco
                  | ENQUANTO VARIAVEL MENORIGUAL INTEIRO DOISPONTOS bloco
                  | ENQUANTO VARIAVEL MAIORIGUAL INTEIRO DOISPONTOS bloco'''
    p[0] = ('while', (p[3], p[2], p[4]), p[6])  # ('while', (condição, variável, valor), bloco)

def p_if_else(p):
    '''expression : SE VARIAVEL IGUAL VARIAVEL DOISPONTOS bloco SENAO DOISPONTOS bloco
                  | SE VARIAVEL DIFERENTE VARIAVEL DOISPONTOS bloco SENAO DOISPONTOS bloco
                  | SE VARIAVEL MENOR VARIAVEL DOISPONTOS bloco SENAO DOISPONTOS bloco
                  | SE VARIAVEL MAIOR VARIAVEL DOISPONTOS bloco SENAO DOISPONTOS bloco
                  | SE VARIAVEL MENORIGUAL VARIAVEL DOISPONTOS bloco SENAO DOISPONTOS bloco
                  | SE VARIAVEL MAIORIGUAL VARIAVEL DOISPONTOS bloco SENAO DOISPONTOS bloco
                  | SE VARIAVEL IGUALDADE VARIAVEL DOISPONTOS bloco SENAO DOISPONTOS bloco'''
    # Realizar a comparação manualmente para diferentes operadores

    if p[3] == '=':
        p[0] = ('if-else', p[2], '=', p[4], p[6], p[9])
    elif p[3] == '!=':
        p[0] = ('if-else', p[2], '!=', p[4], p[6], p[9])
    elif p[3] == '<':
        p[0] = ('if-else', p[2], '<', p[4], p[6], p[9])
    elif p[3] == '>':
        p[0] = ('if-else', p[2], '>', p[4], p[6], p[9])
    elif p[3] == '<=':
        p[0] = ('if-else', p[2], '<=', p[4], p[6], p[9])
    elif p[3] == '>=':
        p[0] = ('if-else', p[2], '>=', p[4], p[6], p[9])
    elif p[3] == '==':
        p[0] = ('if-else', p[2], '==', p[4], p[6], p[9])
    elif p[3] == '!=':
        p[0] = ('if-else', p[2], '!=', p[4], p[6], p[9])

# Regra para o comando 'if' sem 'else'
def p_if(p):
    '''expression : SE VARIAVEL IGUAL VARIAVEL DOISPONTOS bloco
                  | SE VARIAVEL DIFERENTE VARIAVEL DOISPONTOS bloco
                  | SE VARIAVEL MENOR VARIAVEL DOISPONTOS bloco
                  | SE VARIAVEL MAIOR VARIAVEL DOISPONTOS bloco
                  | SE VARIAVEL MENORIGUAL VARIAVEL DOISPONTOS bloco
                  | SE VARIAVEL MAIORIGUAL VARIAVEL DOISPONTOS bloco
                  | SE VARIAVEL IGUALDADE VARIAVEL DOISPONTOS bloco'''
    # Comparação manual entre as duas variáveis
    if p[3] == '=':
        p[0] = ('if', p[2], '=', p[4], p[6])
    elif p[3] == '!=':
        p[0] = ('if', p[2], '!=', p[4], p[6])
    elif p[3] == '<':
        p[0] = ('if', p[2], '<', p[4], p[6])
    elif p[3] == '>':
        p[0] = ('if', p[2], '>', p[4], p[6])
    elif p[3] == '<=':
        p[0] = ('if', p[2], '<=', p[4], p[6])
    elif p[3] == '>=':
        p[0] = ('if', p[2], '>=', p[4], p[6])
    elif p[3] == '==':
        p[0] = ('if', p[2], '==', p[4], p[6])


# Regra para o comando 'escreva'
def p_escreva(p):
    '''expression : ESCREVA ESQPARENTESE expression DIRPARENTESE FINALEXPRESSAO
                  | ESCREVA ESQPARENTESE VARIAVEL DIRPARENTESE FINALEXPRESSAO'''
    p[0] = ('escreva', p[3])  # Armazena o comando 'escreva' e a expressão ou variável a ser impressa

def p_leia(p):
    'expression : LEIA ESQPARENTESE DIRPARENTESE FINALEXPRESSAO'
    p[0] = ('leia')


def traduzir_para_python(arvore, nivel=0):
    """Função de tradução que leva em consideração a indentação corretamente."""
    if isinstance(arvore, list):
        return "\n".join(traduzir_para_python(exp, nivel) for exp in arvore)

    if isinstance(arvore, tuple):
        comando = arvore[0]

         # Atribuição
        if comando == 'atribuir':
            # Tratamento especial para o comando 'leia'
            if isinstance(arvore[2], str) and arvore[2] == 'leia':
                return f"{'    ' * nivel}{arvore[1]} = input()"
            else:
                return f"{'    ' * nivel}{arvore[1]} = {traduzir_para_python(arvore[2], nivel)}"
        
        # While
        elif comando == 'while':
            condicao = f"{arvore[1][1]} {arvore[1][0]} {traduzir_para_python(arvore[1][2], nivel)}"
            bloco = traduzir_para_python(arvore[2], nivel + 1)
            return f"{'    ' * nivel}while {condicao}:\n{bloco}"
        
         # For
        elif comando == 'for':
            variavel = arvore[1]
            faixa_final = traduzir_para_python(arvore[2], nivel)
            faixa_inicial = faixa_final
            bloco = traduzir_para_python(arvore[3], nivel + 1)
            return f"{'    ' * nivel}for {variavel} in range({faixa_inicial}):\n{bloco}"

        # If com comparação manual
        elif comando == 'if':
            var1 = arvore[1]
            operador = arvore[2]
            var2 = arvore[3]
            bloco_if = traduzir_para_python(arvore[4], nivel + 1)
            return f"{'    ' * nivel}if {var1} {operador} {var2}:\n{bloco_if}"
        
         # If-else com comparação manual
        elif comando == 'if-else':
            var1 = arvore[1]
            operador = arvore[2]
            var2 = arvore[3]
            bloco_if = traduzir_para_python(arvore[4], nivel + 1)
            bloco_else = traduzir_para_python(arvore[5], nivel + 1)
            return f"{'    ' * nivel}if {var1} {operador} {var2}:\n{bloco_if}\n{'    ' * nivel}else:\n{bloco_else}"

        # Escreva (print)
        elif comando == 'escreva':
            return f"{'    ' * nivel}print({traduzir_para_python(arvore[1], nivel)})"
        
        # Leia (input)
        elif comando == 'leia':
            return f"{'    ' * nivel}{arvore[1]} = input()"
    return str(arvore)

# Função auxiliar para indentar blocos
def indentar(texto):
    return "\n".join(f"    {linha}" for linha in texto.splitlines())

def p_error(p):
    if p:
        print(f"Erro de sintaxe próximo ao token: {p.value}")
    else:
        print("Ocorreu um erro ao final do código!")

parser = yacc.yacc()

teste = '''
escreva("Digite A: ");
a = leia();
escreva("Digite B: ");
b = leia();

se a < b:
    c = a + b;
senao:
    c = a - b;

enquanto a <= 10:
    escreva("Menor que 10");

for i entre faixa (5):
    escreva("teste");


escreva("C é igual a");
escreva(c);

d = a + b;


'''

# Etapa 1: Análise Léxica
print("Tokens gerados pela análise léxica:")
lexer.input(teste)
for tok in lexer:
    print(f"{tok.type}: {tok.value}")

# Etapa 2: Análise Sintática
print("\nResultado da análise sintática e semântica:")
resultado = parser.parse(teste)
print(f"Resultado: {resultado}")

# Etapa 1: Gera a árvore sintática
arvore = parser.parse(teste)

# Etapa 2: Traduza a árvore para Python
codigo_python = traduzir_para_python(arvore)

# Etapa 3: Imprima o código traduzido
print("Código traduzido para Python:")
print(codigo_python)