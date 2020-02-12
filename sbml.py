# NAME : MILES FOYDL
# ID# : 111528174



import sys
import ply.lex as lex
import ply.yacc as yacc
import operator

''' *************************************************************************************************
    *************************************************************************************************
                                                TOKENIZING
    *************************************************************************************************
    *************************************************************************************************'''




# List of token names
reserved = {
    'True' : 'BOOLEANT',
    'False' : 'BOOLEANF',
    'andalso' : "ANDALSO",
    'orelse' : 'ORELSE',
    'div' : "IDIV",
    'mod' : "MOD",
    'in': "IN",
    'not': "NOT",
    'print': "PRINT",
    'if': "IF",
    'else':"ELSE",
    'while':"WHILE"
}
tokens = [
          'VARI',
          'ASN',
          'INTEGER',
          'REAL',
          'STRING',
          'LPAREN',
          'RPAREN',
          'LBRAK',
          'RBRAK',
          'COMMA',
          'EXP',
          'MULT',
          'DIV',
          'PLUS',
          'MINUS',
          'CONS',
          'LT',
          'LTE',
          'ET',
          'NET',
          'GTE',
          'GT',
          'POUND',
          'SEMI',
          'RBRACE',
          'LBRACE'
         ] + list(reserved.values())




t_ASN = r'='
t_LPAREN = r'\('
t_RPAREN = r'\)'
t_RBRAK = r'\]'
t_LBRAK = r'\['
t_COMMA = r','
t_EXP = r'\*\*'
t_MULT = r'\*'
t_DIV = r"/"
#t_IDIV = r'div'
#t_MOD = r'mod'
t_PLUS = r'\+'
t_MINUS = r'-'
#t_IN = r'in'
t_CONS = r'::'
#t_NOT = r'not'
#t_ANDALSO = r'andalso'
#t_ORELSE = r'orelse'
t_LTE = r'<='
t_LT = r'<'
t_ET = r'=='
t_NET = r'<>'
t_GTE = r'>='
t_GT = r'\>'
t_POUND = r'\#'
t_SEMI = r';'
t_RBRACE = r'}'
t_LBRACE = r'{'

t_ignore = ' \t\n\r'

variables = {}

def t_VARI(t):
    r'[a-zA-Z_][a-zA-Z_0-9]*'
    t.type = reserved.get(t.value,'VARI')    # Check for reserved words
    if t.type is "BOOLEANT":
        t.value= True
    if t.type is "BOOLEANF":
        t.value = False
    return t

def t_REAL(t):
    r'(-)?(\.)?((\d+\.\d*)|(\d*\.\d+))(e-?\d+)?'
    #t.value=t.value
    try:
        t.value= float(t.value)
    except Exception as e:
        #print(str(e),t.value)
        t.value=0
    
    return t

def t_INTEGER(t):
    r'\d+'
    #t.value=t.value
    try:
        t.value= int(t.value)
    except Exception as e:
        #print(str(e),t.value)
        t.value=0
    
    return t

def t_STRING(t):
    r'(\")(.)*?(\")|(\')(.)*?(\')'
    #r'(\"|\')(\w|\d|\s)+(\"|\')'
    t.value = t.value.replace("\'","").replace("\"","")
    return t
    
def t_newline(t):
    r'\n+'
    t.lexer.lineno += len(t.value)
    
def t_error(t):
    #print("SYNTAX ERROR")
    syntax_error_f = True
    t.lexer.skip(1)
    
lexer = lex.lex()







''' *************************************************************************************************
    *************************************************************************************************
                                                PARSING
    *************************************************************************************************
    *************************************************************************************************'''

def p_block(p):
    ''' block : LBRACE block_item RBRACE '''
    p[0] = Block([p[2]])

def p_block_item(p):
    ''' block_item : block_item block_item '''
    p[0] = Block([p[1],p[2]])

def p_block_tail2(p):
    ''' block_item : section 
                   | ifblock
                   | ifelse
                   | while 
                   | block '''
    p[0] = Block([p[1]])

def p_whilecond(p):
    ''' whilecond : WHILE LPAREN expr RPAREN '''
    p[0] = p[3]

def p_while(p):
    ''' while : whilecond LBRACE block_item RBRACE '''
    p[0] = While(p[1],p[3])

def p_ifcond(p):
    '''ifcond : IF LPAREN expr RPAREN '''
    p[0] =p[3]
def p_ifblock(p):
    ''' ifblock : ifcond LBRACE block_item RBRACE '''
    p[0] = If(p[1],p[3])

def p_elseblock(p):
    ''' elseblock : ELSE LBRACE block_item RBRACE '''
    p[0] = p[3]

def p_ifelse(p):
    ''' ifelse : ifblock elseblock '''
    try:
        p[0] = Elif(p[1],p[2])
    except Exception as e:
        raise Exception()
        #print(str(e))

def p_section(p):
    ''' section : statement section_tail '''
    try:
        p[0] = Section([p[1],p[2]])
    except Exception as e:
        #print(str(e))
        raise Exception()

def p_section_tail(p):
    ''' section_tail : statement '''
    try:
        p[0] = Section([p[1]])
    except Exception as e:
        #print(str(e))
        raise Exception()

def p_section_tail_e(p):
    ''' section_tail : '''
    pass

def p_statement(p):
    ''' statement : expr SEMI '''
    p[0] = Node(p[1])

def p_expr(p):
    ''' expr : INTEGER
                | REAL
                | STRING
                | BOOLEANF
                | BOOLEANT 
                | list
                | index
                | vindex
                | tuple'''
    p[0] = Node(p[1])

def p_expr_op(p):
    ''' expr : expr PLUS expr 
                | expr MINUS expr 
                | expr MULT expr
                | expr DIV expr
                | expr IDIV expr
                | expr EXP expr
                | expr MOD expr
                | expr LT expr 
                | expr LTE expr
                | expr ET expr
                | expr NET expr
                | expr GT expr
                | expr GTE expr'''
    try:
        p[0] = BinOp(p[1],p[2],p[3])
    except:
        raise Exception()

def p_expr_uminus(p):
    'expr : MINUS expr %prec UMINUS'
    p[0] = U_Minus(p[2])

def p_list_cons(p):
    ''' expr : expr CONS expr '''
    try:
        p[0] = Cons(p[1],p[3])
    except:
        raise Exception()

def p_expr_bool(p):
    ''' expr : expr ANDALSO expr
             | expr ORELSE expr '''
    try:
        p[0] = BoolOp(p[3],p[1],p[2])
    except:
        return Exception()

def p_expr_not(p):
    ''' expr : NOT expr '''
    try:
        p[0] = BoolNot(p[2])
    except:
        raise Exception()

def p_list(p):
    ''' list : LBRAK expr RBRAK '''
    p[0] = List(p[2])

def p_empty_list(p):
    ''' list : LBRAK RBRAK '''
    p[0] = List()

def p_tuple(p):
    ''' tuple : LPAREN list_item RPAREN '''
    p[0] = Tuple(p[2])

def p_list_b(p):
    ''' list : LBRAK list_item RBRAK '''
    p[0] = List(p[2])

def p_list_item(p):
    ''' list_item : expr COMMA expr '''
    p[0] = [p[1],p[3]]

def p_list_itemb(p):
    ''' list_item : list_item COMMA expr '''
    p[0] = p[1]+[p[3]]

def p_list_index(p):
    ''' index : list LBRAK expr RBRAK 
              | index LBRAK expr RBRAK '''
    try:
        p[0] = IndexList(p[1],p[3])
    except:
        raise Exception()

def p_string_index(p):
    ''' expr : STRING LBRAK expr RBRAK '''
    try:
        p[0] = IndexString(p[1],p[3])
    except:
        raise Exception()

def p_tuple_index(p):
    ''' expr : POUND INTEGER tuple '''
    try:
        p[0] = IndexTuple(p[3],p[2])
    except:
        raise Exception()

def p_in(p):
    ''' expr : expr IN expr'''
    try:
        p[0] = InOp(p[3],p[1])
    except:
        raise Exception()

def p_parenthetical_expr(p):
    ''' expr : LPAREN expr RPAREN '''
    p[0] = Parenthetical(p[2])

def p_assignment(p):
    ''' statement : VARI ASN statement '''
    try:
        p[0] = Assignment(p[1],p[3])
    except:
        raise Exception()

def p_var_index(p):
    ''' vindex : VARI LBRAK expr RBRAK 
              | vindex LBRAK expr RBRAK'''
    try:
        p[0] = VarIndex(p[1],p[3])
    except:
        raise Exception()

def p_index_assignment(p):
    ''' statement : VARI LBRAK expr RBRAK ASN statement'''
    try:
        p[0] = IndexAsn(p[1],p[3],p[6])
    except:
        raise Exception()

    
def p_vexpr(p):
    ''' expr : VARI '''
    p[0] = Var(p[1])

def p_print(p):
    ''' expr : PRINT LPAREN expr RPAREN'''
    p[0] = Print(p[3])
  
def p_error(t):
    raise Exception("SYNTAX ERROR")



precedence = (
    ('left','ORELSE'),
    ('left','ANDALSO'),
    ('left',"NOT"),
    ('left',"LT","LTE","ET","NET","GTE","GT"),
    ('right',"CONS"),
    ('left','IN'),
    ('left',"PLUS","MINUS"),
    ('left',"MULT","DIV","IDIV","MOD"),
    ('right',"EXP"),
    ('right','UMINUS'),
    ('left', 'LBRAK','RBRAK'),
    ('left', 'POUND'),
    ('left', 'LBRACE',"RBRACE"),
    ('left','LPAREN','RPAREN'),
    )

parser = yacc.yacc()






''' *************************************************************************************************
    *************************************************************************************************
                                                AST CLASSES
    *************************************************************************************************
    ************************************************************************************************* '''

class Node():
    def __init__(self,val):
        self.val=val
    def eval(self):
        tval = self.val
        if isinstance(tval,Node): tval = tval.eval()
        return tval

class BinOp(Node):
    def __init__(self,left,op,right):
        self.type = "binop"
        self.left = left
        self.right = right
        self.op = op
        
    def eval(self):
        try:
            leftt = self.left
            rightt= self.right
            while isinstance(leftt,Node):leftt=leftt.eval()
            while isinstance(rightt,Node):rightt=rightt.eval()
            if (type(leftt)== int) or (type(leftt) == float) and ((type(rightt)== int) or (type(rightt) == float)):
                num_ops = {
                    "+": leftt + rightt,
                    "-": leftt - rightt,
                    "*": leftt * rightt,
                    "**": leftt ** rightt,
                    "<": leftt < rightt,
                    "<=": leftt <= rightt,
                    "==": leftt == rightt,
                    "<>": leftt != rightt,
                    ">": leftt > rightt,
                    ">=": leftt >= rightt
                }
                if self.op not in num_ops:
                    if(self.op == "/" and rightt != 0):
                         return leftt / rightt
                    elif self.op == "div" and rightt !=0:
                        return leftt // rightt
                    elif self.op == "mod" and rightt !=0:
                        return leftt % rightt
                    raise Exception()
                return num_ops.get(self.op)
            elif (type(leftt) == str) and (type(rightt)==str):
                str_ops = {
                    "+": leftt + rightt,
                    "<": leftt < rightt,
                    "<=": leftt <= rightt,
                    "==": leftt == rightt,
                    "<>": leftt != rightt,
                    ">": leftt > rightt,
                    ">=": leftt >= rightt
                }
                if self.op not in str_ops:
                    raise Exception()
                return str_ops.get(self.op)
            elif (type(leftt) == list) and (type(rightt)==list):
                list_ops = {
                    "+": leftt + rightt,
                }
                if self.op not in list_ops:
                    raise Exception()
                return list_ops.get(self.op)
            else:
                raise Exception()
        except Exception as e:
            #print(str(e))
            raise Exception()

class U_Minus(Node):
    def __init__(self,num):
        self.num=num
    def eval(self):
        tnum=self.num
        if isinstance(tnum,Node): tnum=tnum.eval()
        return tnum*-1

class Cons(Node):
    def __init__(self,left,right):
        self.left= left
        self.right=right
    def eval(self):
        try:
            leftt= self.left
            rightt=self.right
            while isinstance(leftt,Node):leftt=leftt.eval()
            while isinstance(rightt,Node):rightt=rightt.eval()
            if (type(rightt) is Var) and (rightt.name() in variables.keys()):
                return [leftt]+variables[rightt.name()]
            else:
                return [leftt]+rightt
        except Exception as e:
            #print(str(e))
            raise Exception()

class BoolOp(Node):
    def __init__(self,right,left,op):
        self.left=left
        self.right=right
        self.op=op
    def eval(self):
        leftt= self.left
        rightt=self.right
        if isinstance(leftt,Node):leftt=leftt.eval()
        if isinstance(rightt,Node):rightt=rightt.eval()
        if(type(leftt) is bool) and (type(rightt) is bool):
            if self.op == "andalso":
                return leftt and rightt
            else:
                return leftt or rightt
        else:
            raise Exception()

class BoolNot(Node):
    def __init__(self,b):
        self.b = b
    def eval(self):
        tb= self.b
        if isinstance(tb,Node):tb=tb.eval()
        if type(tb) is bool:
            return not tb
        else:
            raise Exception()

class List(Node):
    def __init__(self,l=None):
        self.l=l
    def eval(self):
        templ=self.l
        if templ:
            if isinstance(templ,Node):templ=templ.eval()
            if type(templ) is list:
                rlist = []
                for item in templ:
                    rlist.append(item.eval())
                return rlist
            else:
                return [templ]
        else:
            return []

class IndexList(Node):
    def __init__(self,l,i):
        self.l=l
        self.i=i
    def eval(self):
        templ=self.l
        tempi=self.i
        if isinstance(templ,Node):templ=templ.eval()
        if isinstance(tempi,Node):tempi=tempi.eval()
        if type(tempi) is bool:
            raise Exception()
        if isinstance(templ[tempi],Node): return templ[tempi].eval()
        return templ[tempi]

class ListItem(Node):
    def __init__(self,a,b):
        self.a=a
        self.b=b
    def eval(self):
        tempa=self.a
        tempb=self.b
        if isinstance(tempa,Node):tempa=tempa.eval()
        if isinstance(tempb,Node):tempb=tempb.eval()
        if type(tempa) is list:
            return tempa + [tempb]
        else:
            return [tempa] + [tempb]

class Tuple(Node):
    def __init__(self,t):
        self.t=t
    def eval(self):
        tempt=self.t
        if isinstance(tempt,Node):tempt=tempt.eval()
        rlist = []
        for item in tempt:
            rlist.append(item.eval())
        return tuple(rlist)

class IndexTuple(Node):
    def __init__(self,t,i):
        self.t=t
        self.i=i
    def eval(self):
        tempt=self.t
        tempi=self.i
        if isinstance(tempt,Node):tempt=tempt.eval()
        if isinstance(tempi,Node):tempi=tempi.eval()
        if isinstance(tempt[tempi-1],Node): return tempt[tempi-1].eval()
        return tempt[tempi-1]

class IndexString(Node):
    def __init__(self,s,i):
        self.s=s
        self.i=i
    def eval(self):
        temps=self.s
        tempi=self.i
        if isinstance(temps,Node):temps=temps.eval()
        if isinstance(tempi,Node):tempi=tempi.eval()
        return temps[tempi]

class InOp(Node):
    def __init__(self,l,o):
        self.l=l
        self.o = o
    def eval(self):
        try:
            templ=self.l
            tempo=self.o
            if isinstance(templ,Node):templ=templ.eval()
            if isinstance(tempo,Node):tempo=tempo.eval()
            return tempo in templ
        except:
            raise Exception()

class Parenthetical(Node):
    def __init__(self,expr):
        self.expr=expr
    def eval(self):
        texpr=self.expr
        if isinstance(texpr,Node):texpr=texpr.eval()
        return texpr

class Assignment(Node):
    def __init__(self,l,r):
        self.l=l
        self.r=r
    def eval(self):
        templ=self.l
        tempr=self.r
        if isinstance(templ,Node):templ=templ.eval()
        if isinstance(tempr,Node):tempr=tempr.eval()
        variables.update({templ:tempr})
        return

class VarIndex(Node):
    def __init__(self,var,i):
        self.var=var
        self.i=i
    def eval(self):
        try:
            tvar = self.var
            tempi=self.i
            if isinstance(tvar,Node):tvar=tvar.eval()
            if isinstance(tempi,Node):tempi=tempi.eval()
            if type(tvar) is str:
                return variables[tvar][tempi]
            elif type(tvar) == list:
                return tvar[tempi]
        except Exception as e:
            #print(str(e))
            raise Exception()

class IndexAsn(Node):
    def __init__(self,l,i,r):
        self.l=l
        self.i=i
        self.r=r
    def eval(self):
        try:
            templ=self.l
            tempi=self.i
            tempr=self.r
            if isinstance(templ,Node):templ=templ.eval()
            if isinstance(tempi,Node):tempi=tempi.eval()
            if isinstance(tempr,Node):tempr=tempr.eval()
            variables[templ][tempi] = tempr
            return
        except:
            raise Exception()

class Var(Node):
    def __init__(self,var):
        self.var=var
    def eval(self):
        return variables[self.var]
    def name(self):
        return self.var

class Print(Node):
    def __init__(self,val):
        self.val=val
    def eval(self):
        try:
            tval=self.val
            if(isinstance(tval,Node)):tval=tval.eval()
            print(tval)
        except Exception as e:
            raise Exception()

class Section(Node):
    def __init__(self,block):
        self.block=block
    def eval(self):
        try:
            tempblock=self.block
            for section in tempblock:
                if section != None:
                    section = section.eval()
        except Exception as e:
            #print("Section: ",str(e))
            raise Exception()

class Block(Node):
    def __init__(self,block):
        self.block=block
    def eval(self):
        try:
            tempblock=self.block
            for section in tempblock:
                if section != None:
                    section.eval()
        except Exception as e:
            #print("Block: ",str(e))
            raise Exception()

class While(Node):
    def __init__(self,cond,block):
        self.cond=cond
        self.block=block
    def eval(self):
        try:
            tcond=self.cond
            tempblock=self.block
            if (isinstance(tcond,Node)) and (type(tcond.eval()) is bool):
                while tcond.eval():
                    #print("Loop:",tcond.eval())
                    tempblock.eval()
            else:
                #print("error2")
                raise Exception()
        except Exception as e:
            #print("Error3:",str(e))
            raise Exception()

class If(Node):
    def __init__(self,cond,block):
        self.cond=cond
        self.block=block
    def eval(self):
        try:
            tcond=self.cond
            tempblock=self.block
            if isinstance(tcond,Node):tcond = tcond.eval()
            if(type(tcond) is bool):
                if(tcond is True):
                    tempblock.eval()
            else:
                raise Exception()
        except Exception as e:
            #print(str(e))
            raise Exception()
    def getCond(self):
        return self.cond

class Elif(Node):
    def __init__(self,ifb,elseb):
        self.cond=ifb.getCond()
        self.ifb=ifb
        self.elseb=elseb
    def eval(self):
        try:
            tcond=self.cond
            tempifb=self.ifb
            telseb=self.elseb
            tcond = tcond.eval()
            if tcond == True:
                tempifb.eval()
            else:
                telseb.eval()
        except Exception as e:
            #print("Elif Exception2",str(e))
            raise Exception()


''' *************************************************************************************************
    *************************************************************************************************
                                                MAIN
    *************************************************************************************************
    ************************************************************************************************* '''

name = sys.argv[1]

try:
    with open(name, 'r') as file:
        
        data = file.read()
        try:
            result = parser.parse(data, debug=False)
            result.eval()
            #print(variables['miles'])
            #print("DONE")
        except Exception as e:
            if(str(e)=="SYNTAX ERROR"):
                print(str(e))
            else:
                print("SEMANTIC ERROR")
        file.close()
except:
    print("Error")

#print("Gucci Gang")