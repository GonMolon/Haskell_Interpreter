/*
 * A n t l r  T r a n s l a t i o n  H e a d e r
 *
 * Terence Parr, Will Cohen, and Hank Dietz: 1989-2001
 * Purdue University Electrical Engineering
 * With AHPCRC, University of Minnesota
 * ANTLR Version 1.33MR33
 *
 *   antlr -gt parser.g
 *
 */

#define ANTLR_VERSION	13333
#include "pcctscfg.h"
#include "pccts_stdio.h"

#include <string>
#include <iostream>
#include <map>
using namespace std;

// struct to store inprintion about tokens
typedef struct {
  string kind;
  string text;
} Attrib;

// function to fill token inprintion (predeclaration)
void zzcr_attr(Attrib *attr, int type, char *text);

// fields forAST nodes
#define AST_FIELDS string kind; string text;
#include "ast.h"

// macro to create a new AST node (and function predeclaration)
#define zzcr_ast(as,attr,ttype,textt) as=createASTnode(attr,ttype,textt)
AST* createASTnode(Attrib* attr,int ttype, char *textt);
#define GENAST

#include "ast.h"

#define zzSET_SIZE 4
#include "antlr.h"
#include "tokens.h"
#include "dlgdef.h"
#include "mode.h"

/* MR23 In order to remove calls to PURIFY use the antlr -nopurify option */

#ifndef PCCTS_PURIFY
#define PCCTS_PURIFY(r,s) memset((char *) &(r),'\0',(s));
#endif

#include "ast.c"
zzASTgvars

ANTLR_INFO

#include <cstdlib>
#include <cmath>

//global structures
AST* root;

// function to fill token inprintion
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
AST* createASTlist(AST* child) {
  AST* as = new AST;
  as->kind = "list";
  as->right = NULL;
  as->down = child;
  return as;
}

AST* child(AST* a, int n) {
  AST* c = a->down;
  for(int i = 0; c != NULL && i < n; i++) {
    c = c->right;
  }
  return c;
}

void printExpr(AST* a) {
  cout << "(";
  if(a->kind == "NOT") {
    cout << "NOT ";
    printExpr(child(a, 0));
  } else if(a->kind == "AND" || a->kind == "OR") {
    cout << a->kind << " ";
    printExpr(child(a, 0));
    printExpr(child(a, 1));
  } else if(a->kind == ">") {
    cout << "Gt ";
    printExpr(child(a, 0));
    printExpr(child(a, 1));
  } else if(a->kind == "=") {
    cout << "Eq ";
    printExpr(child(a, 0));
    printExpr(child(a, 1));
  } else if(a->kind == "+") {
    cout << "Plus ";
    printExpr(child(a, 0));
    printExpr(child(a, 1));
  } else if(a->kind == "-") {
    cout << "Minus ";
    printExpr(child(a, 0));
    printExpr(child(a, 1));
  } else if(a->kind == "*") {
    cout << "Times ";
    printExpr(child(a, 0));
    printExpr(child(a, 1));
  } else if(a->kind == "ID") {
    cout << "Var \"" << a->text << "\"";
  } else if(a->kind == "NUM") {
    cout << "Const " << a->text;
  }
  cout << ")";
}

void print(AST* a) {
  cout << "(";
  if(a->kind == "list") {
    cout << "Seq [";
    AST* b = child(a, 0);
    bool first = true;
    while(b != NULL) {
      if(!first) {
        cout << ", ";
      } else {
        first = false;
      }
      print(b);
      b = b->right;
    }
    cout << "]";
  } else if(a->kind == "INPUT") {
    cout << "Input ";
    print(child(a, 0));
  } else if(a->kind == ":=") {
    cout << "Assign ";
    print(child(a, 0));
    printExpr(child(a, 1));
  } else if(a->kind == "PRINT") {
    cout << "Print ";
    printExpr(child(a, 0));
  } else if(a->kind == "POP") {
    cout << "Pop ";
    print(child(a, 0));
    print(child(a, 1));
  } else if(a->kind == "PUSH") {
    cout << "Push ";
    print(child(a, 0));
    printExpr(child(a, 1));
  } else if(a->kind == "SIZE") {
    cout << "Size ";
    print(child(a, 0));
    print(child(a, 1));
  } else if(a->kind == "EMPTY") {
    cout << "Empty ";
    print(child(a, 0));
  } else if(a->kind == "WHILE") {
    cout << "Loop ";
    printExpr(child(a, 0));
    print(child(a, 1));
  } else if(a->kind == "IF") {
    cout << "Cond ";
    printExpr(child(a, 0));
    print(child(a, 1));
    AST* aux = child(a, 2);
    if(aux != NULL) {
      print(child(a, 2));
    } else {
      cout << "(Seq [])";
    }
  } else if(a->kind == "ID") {
    cout << "\"" << a->text << "\"";
  }
  cout << ")";
}

int main() {
  root = NULL;
  ANTLR(program(&root), stdin);
  print(root);
}

void
#ifdef __USE_PROTOS
program(AST**_root)
#else
program(_root)
AST **_root;
#endif
{
  zzRULE;
  zzBLOCK(zztasp1);
  zzMake0;
  {
  ops(zzSTR); zzlink(_root, &_sibling, &_tail);
  zzEXIT(zztasp1);
  return;
fail:
  zzEXIT(zztasp1);
  zzsyn(zzMissText, zzBadTok, (ANTLRChar *)"", zzMissSet, zzMissTok, zzErrk, zzBadText);
  zzresynch(setwd1, 0x1);
  }
}

void
#ifdef __USE_PROTOS
ops(AST**_root)
#else
ops(_root)
AST **_root;
#endif
{
  zzRULE;
  zzBLOCK(zztasp1);
  zzMake0;
  {
  {
    zzBLOCK(zztasp2);
    zzMake0;
    {
    while ( (setwd1[LA(1)]&0x2) ) {
      op(zzSTR); zzlink(_root, &_sibling, &_tail);
      zzLOOP(zztasp2);
    }
    zzEXIT(zztasp2);
    }
  }
  (*_root)=createASTlist(_sibling);
  zzEXIT(zztasp1);
  return;
fail:
  zzEXIT(zztasp1);
  zzsyn(zzMissText, zzBadTok, (ANTLRChar *)"", zzMissSet, zzMissTok, zzErrk, zzBadText);
  zzresynch(setwd1, 0x4);
  }
}

void
#ifdef __USE_PROTOS
op(AST**_root)
#else
op(_root)
AST **_root;
#endif
{
  zzRULE;
  zzBLOCK(zztasp1);
  zzMake0;
  {
  if ( (LA(1)==WHILE) ) {
    whileLoop(zzSTR); zzlink(_root, &_sibling, &_tail);
  }
  else {
    if ( (LA(1)==IF) ) {
      ifCond(zzSTR); zzlink(_root, &_sibling, &_tail);
    }
    else {
      if ( (LA(1)==INPUT) ) {
        input(zzSTR); zzlink(_root, &_sibling, &_tail);
      }
      else {
        if ( (LA(1)==PRINT) ) {
          print(zzSTR); zzlink(_root, &_sibling, &_tail);
        }
        else {
          if ( (LA(1)==EMPTY) ) {
            empty(zzSTR); zzlink(_root, &_sibling, &_tail);
          }
          else {
            if ( (LA(1)==SIZE) ) {
              size(zzSTR); zzlink(_root, &_sibling, &_tail);
            }
            else {
              if ( (LA(1)==POP) ) {
                pop(zzSTR); zzlink(_root, &_sibling, &_tail);
              }
              else {
                if ( (LA(1)==PUSH) ) {
                  push(zzSTR); zzlink(_root, &_sibling, &_tail);
                }
                else {
                  if ( (LA(1)==ID) ) {
                    assign(zzSTR); zzlink(_root, &_sibling, &_tail);
                  }
                  else {zzFAIL(1,zzerr1,&zzMissSet,&zzMissText,&zzBadTok,&zzBadText,&zzErrk); goto fail;}
                }
              }
            }
          }
        }
      }
    }
  }
  zzEXIT(zztasp1);
  return;
fail:
  zzEXIT(zztasp1);
  zzsyn(zzMissText, zzBadTok, (ANTLRChar *)"", zzMissSet, zzMissTok, zzErrk, zzBadText);
  zzresynch(setwd1, 0x8);
  }
}

void
#ifdef __USE_PROTOS
input(AST**_root)
#else
input(_root)
AST **_root;
#endif
{
  zzRULE;
  zzBLOCK(zztasp1);
  zzMake0;
  {
  zzmatch(INPUT); zzsubroot(_root, &_sibling, &_tail); zzCONSUME;
  zzmatch(ID); zzsubchild(_root, &_sibling, &_tail); zzCONSUME;
  zzEXIT(zztasp1);
  return;
fail:
  zzEXIT(zztasp1);
  zzsyn(zzMissText, zzBadTok, (ANTLRChar *)"", zzMissSet, zzMissTok, zzErrk, zzBadText);
  zzresynch(setwd1, 0x10);
  }
}

void
#ifdef __USE_PROTOS
print(AST**_root)
#else
print(_root)
AST **_root;
#endif
{
  zzRULE;
  zzBLOCK(zztasp1);
  zzMake0;
  {
  zzmatch(PRINT); zzsubroot(_root, &_sibling, &_tail); zzCONSUME;
  termNum(zzSTR); zzlink(_root, &_sibling, &_tail);
  zzEXIT(zztasp1);
  return;
fail:
  zzEXIT(zztasp1);
  zzsyn(zzMissText, zzBadTok, (ANTLRChar *)"", zzMissSet, zzMissTok, zzErrk, zzBadText);
  zzresynch(setwd1, 0x20);
  }
}

void
#ifdef __USE_PROTOS
empty(AST**_root)
#else
empty(_root)
AST **_root;
#endif
{
  zzRULE;
  zzBLOCK(zztasp1);
  zzMake0;
  {
  zzmatch(EMPTY); zzsubroot(_root, &_sibling, &_tail); zzCONSUME;
  zzmatch(ID); zzsubchild(_root, &_sibling, &_tail); zzCONSUME;
  zzEXIT(zztasp1);
  return;
fail:
  zzEXIT(zztasp1);
  zzsyn(zzMissText, zzBadTok, (ANTLRChar *)"", zzMissSet, zzMissTok, zzErrk, zzBadText);
  zzresynch(setwd1, 0x40);
  }
}

void
#ifdef __USE_PROTOS
pop(AST**_root)
#else
pop(_root)
AST **_root;
#endif
{
  zzRULE;
  zzBLOCK(zztasp1);
  zzMake0;
  {
  zzmatch(POP); zzsubroot(_root, &_sibling, &_tail); zzCONSUME;
  zzmatch(ID); zzsubchild(_root, &_sibling, &_tail); zzCONSUME;
  zzmatch(ID); zzsubchild(_root, &_sibling, &_tail); zzCONSUME;
  zzEXIT(zztasp1);
  return;
fail:
  zzEXIT(zztasp1);
  zzsyn(zzMissText, zzBadTok, (ANTLRChar *)"", zzMissSet, zzMissTok, zzErrk, zzBadText);
  zzresynch(setwd1, 0x80);
  }
}

void
#ifdef __USE_PROTOS
push(AST**_root)
#else
push(_root)
AST **_root;
#endif
{
  zzRULE;
  zzBLOCK(zztasp1);
  zzMake0;
  {
  zzmatch(PUSH); zzsubroot(_root, &_sibling, &_tail); zzCONSUME;
  zzmatch(ID); zzsubchild(_root, &_sibling, &_tail); zzCONSUME;
  termNum(zzSTR); zzlink(_root, &_sibling, &_tail);
  zzEXIT(zztasp1);
  return;
fail:
  zzEXIT(zztasp1);
  zzsyn(zzMissText, zzBadTok, (ANTLRChar *)"", zzMissSet, zzMissTok, zzErrk, zzBadText);
  zzresynch(setwd2, 0x1);
  }
}

void
#ifdef __USE_PROTOS
size(AST**_root)
#else
size(_root)
AST **_root;
#endif
{
  zzRULE;
  zzBLOCK(zztasp1);
  zzMake0;
  {
  zzmatch(SIZE); zzsubroot(_root, &_sibling, &_tail); zzCONSUME;
  zzmatch(ID); zzsubchild(_root, &_sibling, &_tail); zzCONSUME;
  zzmatch(ID); zzsubchild(_root, &_sibling, &_tail); zzCONSUME;
  zzEXIT(zztasp1);
  return;
fail:
  zzEXIT(zztasp1);
  zzsyn(zzMissText, zzBadTok, (ANTLRChar *)"", zzMissSet, zzMissTok, zzErrk, zzBadText);
  zzresynch(setwd2, 0x2);
  }
}

void
#ifdef __USE_PROTOS
assign(AST**_root)
#else
assign(_root)
AST **_root;
#endif
{
  zzRULE;
  zzBLOCK(zztasp1);
  zzMake0;
  {
  zzmatch(ID); zzsubchild(_root, &_sibling, &_tail); zzCONSUME;
  zzmatch(ASSIGN); zzsubroot(_root, &_sibling, &_tail); zzCONSUME;
  expr(zzSTR); zzlink(_root, &_sibling, &_tail);
  zzEXIT(zztasp1);
  return;
fail:
  zzEXIT(zztasp1);
  zzsyn(zzMissText, zzBadTok, (ANTLRChar *)"", zzMissSet, zzMissTok, zzErrk, zzBadText);
  zzresynch(setwd2, 0x4);
  }
}

void
#ifdef __USE_PROTOS
whileLoop(AST**_root)
#else
whileLoop(_root)
AST **_root;
#endif
{
  zzRULE;
  zzBLOCK(zztasp1);
  zzMake0;
  {
  zzmatch(WHILE); zzsubroot(_root, &_sibling, &_tail); zzCONSUME;
  expr(zzSTR); zzlink(_root, &_sibling, &_tail);
  zzmatch(DO);  zzCONSUME;
  ops(zzSTR); zzlink(_root, &_sibling, &_tail);
  zzmatch(END);  zzCONSUME;
  zzEXIT(zztasp1);
  return;
fail:
  zzEXIT(zztasp1);
  zzsyn(zzMissText, zzBadTok, (ANTLRChar *)"", zzMissSet, zzMissTok, zzErrk, zzBadText);
  zzresynch(setwd2, 0x8);
  }
}

void
#ifdef __USE_PROTOS
ifCond(AST**_root)
#else
ifCond(_root)
AST **_root;
#endif
{
  zzRULE;
  zzBLOCK(zztasp1);
  zzMake0;
  {
  zzmatch(IF); zzsubroot(_root, &_sibling, &_tail); zzCONSUME;
  expr(zzSTR); zzlink(_root, &_sibling, &_tail);
  zzmatch(THEN);  zzCONSUME;
  ops(zzSTR); zzlink(_root, &_sibling, &_tail);
  {
    zzBLOCK(zztasp2);
    zzMake0;
    {
    if ( (LA(1)==ELSE) ) {
      {
        zzBLOCK(zztasp3);
        zzMake0;
        {
        zzmatch(ELSE);  zzCONSUME;
        ops(zzSTR); zzlink(_root, &_sibling, &_tail);
        zzEXIT(zztasp3);
        }
      }
    }
    else {
      if ( (LA(1)==END) ) {
      }
      else {zzFAIL(1,zzerr2,&zzMissSet,&zzMissText,&zzBadTok,&zzBadText,&zzErrk); goto fail;}
    }
    zzEXIT(zztasp2);
    }
  }
  zzmatch(END);  zzCONSUME;
  zzEXIT(zztasp1);
  return;
fail:
  zzEXIT(zztasp1);
  zzsyn(zzMissText, zzBadTok, (ANTLRChar *)"", zzMissSet, zzMissTok, zzErrk, zzBadText);
  zzresynch(setwd2, 0x10);
  }
}

void
#ifdef __USE_PROTOS
expr(AST**_root)
#else
expr(_root)
AST **_root;
#endif
{
  zzRULE;
  zzBLOCK(zztasp1);
  zzMake0;
  {
  termBool(zzSTR); zzlink(_root, &_sibling, &_tail);
  {
    zzBLOCK(zztasp2);
    zzMake0;
    {
    while ( (setwd2[LA(1)]&0x20) ) {
      {
        zzBLOCK(zztasp3);
        zzMake0;
        {
        if ( (LA(1)==AND) ) {
          zzmatch(AND); zzsubroot(_root, &_sibling, &_tail); zzCONSUME;
        }
        else {
          if ( (LA(1)==OR) ) {
            zzmatch(OR); zzsubroot(_root, &_sibling, &_tail); zzCONSUME;
          }
          else {zzFAIL(1,zzerr3,&zzMissSet,&zzMissText,&zzBadTok,&zzBadText,&zzErrk); goto fail;}
        }
        zzEXIT(zztasp3);
        }
      }
      termBool(zzSTR); zzlink(_root, &_sibling, &_tail);
      zzLOOP(zztasp2);
    }
    zzEXIT(zztasp2);
    }
  }
  zzEXIT(zztasp1);
  return;
fail:
  zzEXIT(zztasp1);
  zzsyn(zzMissText, zzBadTok, (ANTLRChar *)"", zzMissSet, zzMissTok, zzErrk, zzBadText);
  zzresynch(setwd2, 0x40);
  }
}

void
#ifdef __USE_PROTOS
termBool(AST**_root)
#else
termBool(_root)
AST **_root;
#endif
{
  zzRULE;
  zzBLOCK(zztasp1);
  zzMake0;
  {
  if ( (LA(1)==NOT) ) {
    {
      zzBLOCK(zztasp2);
      zzMake0;
      {
      zzmatch(NOT); zzsubroot(_root, &_sibling, &_tail); zzCONSUME;
      termBool(zzSTR); zzlink(_root, &_sibling, &_tail);
      zzEXIT(zztasp2);
      }
    }
  }
  else {
    if ( (setwd2[LA(1)]&0x80) ) {
      {
        zzBLOCK(zztasp2);
        zzMake0;
        {
        termNum(zzSTR); zzlink(_root, &_sibling, &_tail);
        {
          zzBLOCK(zztasp3);
          zzMake0;
          {
          while ( (setwd3[LA(1)]&0x1) ) {
            {
              zzBLOCK(zztasp4);
              zzMake0;
              {
              if ( (LA(1)==BIGGER) ) {
                zzmatch(BIGGER); zzsubroot(_root, &_sibling, &_tail); zzCONSUME;
              }
              else {
                if ( (LA(1)==EQUAL) ) {
                  zzmatch(EQUAL); zzsubroot(_root, &_sibling, &_tail); zzCONSUME;
                }
                else {zzFAIL(1,zzerr4,&zzMissSet,&zzMissText,&zzBadTok,&zzBadText,&zzErrk); goto fail;}
              }
              zzEXIT(zztasp4);
              }
            }
            termNum(zzSTR); zzlink(_root, &_sibling, &_tail);
            zzLOOP(zztasp3);
          }
          zzEXIT(zztasp3);
          }
        }
        zzEXIT(zztasp2);
        }
      }
    }
    else {zzFAIL(1,zzerr5,&zzMissSet,&zzMissText,&zzBadTok,&zzBadText,&zzErrk); goto fail;}
  }
  zzEXIT(zztasp1);
  return;
fail:
  zzEXIT(zztasp1);
  zzsyn(zzMissText, zzBadTok, (ANTLRChar *)"", zzMissSet, zzMissTok, zzErrk, zzBadText);
  zzresynch(setwd3, 0x2);
  }
}

void
#ifdef __USE_PROTOS
termNum(AST**_root)
#else
termNum(_root)
AST **_root;
#endif
{
  zzRULE;
  zzBLOCK(zztasp1);
  zzMake0;
  {
  operand(zzSTR); zzlink(_root, &_sibling, &_tail);
  {
    zzBLOCK(zztasp2);
    zzMake0;
    {
    while ( (setwd3[LA(1)]&0x4) ) {
      {
        zzBLOCK(zztasp3);
        zzMake0;
        {
        if ( (LA(1)==SUM) ) {
          zzmatch(SUM); zzsubroot(_root, &_sibling, &_tail); zzCONSUME;
        }
        else {
          if ( (LA(1)==MINUS) ) {
            zzmatch(MINUS); zzsubroot(_root, &_sibling, &_tail); zzCONSUME;
          }
          else {
            if ( (LA(1)==MULT) ) {
              zzmatch(MULT); zzsubroot(_root, &_sibling, &_tail); zzCONSUME;
            }
            else {zzFAIL(1,zzerr6,&zzMissSet,&zzMissText,&zzBadTok,&zzBadText,&zzErrk); goto fail;}
          }
        }
        zzEXIT(zztasp3);
        }
      }
      operand(zzSTR); zzlink(_root, &_sibling, &_tail);
      zzLOOP(zztasp2);
    }
    zzEXIT(zztasp2);
    }
  }
  zzEXIT(zztasp1);
  return;
fail:
  zzEXIT(zztasp1);
  zzsyn(zzMissText, zzBadTok, (ANTLRChar *)"", zzMissSet, zzMissTok, zzErrk, zzBadText);
  zzresynch(setwd3, 0x8);
  }
}

void
#ifdef __USE_PROTOS
operand(AST**_root)
#else
operand(_root)
AST **_root;
#endif
{
  zzRULE;
  zzBLOCK(zztasp1);
  zzMake0;
  {
  if ( (LA(1)==NUM) ) {
    zzmatch(NUM); zzsubchild(_root, &_sibling, &_tail); zzCONSUME;
  }
  else {
    if ( (LA(1)==ID) ) {
      zzmatch(ID); zzsubchild(_root, &_sibling, &_tail); zzCONSUME;
    }
    else {zzFAIL(1,zzerr7,&zzMissSet,&zzMissText,&zzBadTok,&zzBadText,&zzErrk); goto fail;}
  }
  zzEXIT(zztasp1);
  return;
fail:
  zzEXIT(zztasp1);
  zzsyn(zzMissText, zzBadTok, (ANTLRChar *)"", zzMissSet, zzMissTok, zzErrk, zzBadText);
  zzresynch(setwd3, 0x10);
  }
}
