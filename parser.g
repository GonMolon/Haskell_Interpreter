#header
<<
#include <string>
#include <iostream>
#include <map>
using namespace std;

// struct to store information about tokens
typedef struct {
    string kind;
    string text;
} Attrib;

// function to fill token information (predeclaration)
void zzcr_attr(Attrib *attr, int type, char *text);

// fields forAST nodes
#define AST_FIELDS string kind; string text;
#include "ast.h"

// macro to create a new AST node (and function predeclaration)
#define zzcr_ast(as,attr,ttype,textt) as=createASTnode(attr,ttype,textt)
AST* createASTnode(Attrib* attr,int ttype, char *textt);
>>

<<
#include <cstdlib>
#include <cmath>

//global structures
AST *root;

// function to fill token information
void zzcr_attr(Attrib *attr, int type, char *text) {
    if(type == ID) {
        attr->kind = "ID";
        attr->text = text;
    } else if(type == NUM) {
        attr->kind = "NUM";
        attr->text = text;
    } else {
        attr->kind = text;
        attr->text = "";
    }
}

// function to create a new AST node
AST* createASTnode(Attrib* attr, int type, char* text) {
    AST* as = new AST;
    as->kind = attr->kind;
    as->text = attr->text;
    as->right = NULL;
    as->down = NULL;
    return as;
}

/// create a new "list" AST node with one element
AST* createASTlist(AST *child) {
    AST *as = new AST;
    as->kind = "list";
    as->right = NULL;
    as->down = child;
    return as;
}

AST* child(AST* a, int n) {
    AST *c = a->down;
    for(int i = 0; c != NULL && i < n; i++) {
        c = c->right;
    }
    return c;
}

/// print AST, recursively, with indentation
void ASTPrintIndent(AST *a, string s) {
    if(a == NULL) {
        return;
    }
    cout << a->kind;
    if(a->text != "") {
        cout << "(" << a->text << ")";
    }
    cout << endl;
    AST *i = a->down;
    while(i != NULL && i->right != NULL) {
        cout<<s+"  \\__";
        ASTPrintIndent(i, s + "  |" + string(i->kind.size() + i->text.size(), ' '));
        i=i->right;
    }
    if(i != NULL) {
        cout<<s+"  \\__";
        ASTPrintIndent(i, s + "   " + string(i->kind.size() + i->text.size(), ' '));
        i = i->right;
    }
}

/// print AST 
void ASTPrint(AST *a) {
    while(a != NULL) {
        cout << " ";
        ASTPrintIndent(a, "");
        a = a->right;
    }
}


int main() {
    root = NULL;
    ANTLR(program(&root), stdin);
    ASTPrint(root);
}
>>

#lexclass START

#token WHILE "WHILE"
#token DO "DO"
#token END "END"
#token IF "IF"
#token THEN "THEN"
#token ELSE "ELSE"
#token ASIG ":="
#token INPUT "INPUT"
#token PRINT "PRINT"
#token EMPTY "EMPTY"
#token PUSH "PUSH"
#token POP "POP"
#token SIZE "SIZE"
#token EQUAL "\="
#token OR "OR"
#token AND "AND"
#token NOT "NOT"
#token BIGGER "\>"
#token MINUS "\-"
#token SUM "\+"
#token MULT "\*"
#token NUM "[0-9]+"
#token ID "[a-zA-Z][0-9a-zA-Z]*"
#token SPACE "[\ \n]" << zzskip();>>

program: ops;
ops: (op)* <<#0=createASTlist(_sibling);>>;
op: whileLoop | ifCond | input | print | empty | size | pop | push | asig;

input: INPUT^ ID;
print: PRINT^ ID;
empty: EMPTY^ ID;
pop: POP^ ID ID;
push: PUSH^ ID ID;
size: SIZE^ ID ID;
asig: ID ASIG^ expr;
whileLoop: WHILE^ expr DO! ops END!;
ifCond: IF^ expr THEN! ops (END! | elseCond);
elseCond: ELSE^ ops END!;

expr: termBool ((AND^ | OR^) termBool)*;
termBool: (NOT^ termBool) | (termNum ((BIGGER^ | EQUAL^) termNum)*);
termNum: operand ((SUM^ | MINUS^ | MULT^) operand)*;
operand: NUM | ID;


/*
INPUT X
INPUT Y
IF X > 0 OR X = 0 OR NOT 0 > Y THEN
    Z := 1
    WHILE X > Y DO
        X := X - 1
        Z := Z * Z
    END
ELSE
    Z := 0
END 
PRINT Z
*/

/*
INPUT X
EMPTY P
WHILE X > 0 OR X = 0
DO
INPUT Y
PUSH P Y END
S := 0
SIZE P L
WHILE L > 0
DO
  POP P Y
  S := S + Y
  L := L - 1
END PRINT S
*/