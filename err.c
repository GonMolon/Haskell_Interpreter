/*
 * A n t l r  S e t s / E r r o r  F i l e  H e a d e r
 *
 * Generated from: parser.g
 *
 * Terence Parr, Russell Quong, Will Cohen, and Hank Dietz: 1989-2001
 * Parr Research Corporation
 * with Purdue University Electrical Engineering
 * With AHPCRC, University of Minnesota
 * ANTLR Version 1.33MR33
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
#define zzSET_SIZE 4
#include "antlr.h"
#include "ast.h"
#include "tokens.h"
#include "dlgdef.h"
#include "err.h"

ANTLRChar *zztokens[26]={
	/* 00 */	"Invalid",
	/* 01 */	"@",
	/* 02 */	"WHILE",
	/* 03 */	"DO",
	/* 04 */	"END",
	/* 05 */	"IF",
	/* 06 */	"THEN",
	/* 07 */	"ELSE",
	/* 08 */	"ASSIGN",
	/* 09 */	"INPUT",
	/* 10 */	"PRINT",
	/* 11 */	"EMPTY",
	/* 12 */	"PUSH",
	/* 13 */	"POP",
	/* 14 */	"SIZE",
	/* 15 */	"EQUAL",
	/* 16 */	"OR",
	/* 17 */	"AND",
	/* 18 */	"NOT",
	/* 19 */	"BIGGER",
	/* 20 */	"MINUS",
	/* 21 */	"SUM",
	/* 22 */	"MULT",
	/* 23 */	"NUM",
	/* 24 */	"VAR",
	/* 25 */	"SPACE"
};
SetWordType zzerr1[4] = {0x24,0x7e,0x0,0x1};
SetWordType setwd1[26] = {0x0,0xfd,0xfa,0x0,0xfc,0xfa,0x0,
	0xfc,0x0,0xfa,0xfa,0xfa,0xfa,0xfa,0xfa,
	0x0,0x0,0x0,0x0,0x0,0x0,0x0,0x0,
	0x0,0xfa,0x0};
SetWordType zzerr2[4] = {0x0,0x0,0x3,0x0};
SetWordType setwd2[26] = {0x0,0x5f,0x5f,0x40,0x5f,0x5f,0x40,
	0x5f,0x0,0x5f,0x5f,0x5f,0x5f,0x5f,0x5f,
	0x0,0x20,0x20,0x0,0x0,0x0,0x0,0x0,
	0x80,0xdf,0x0};
SetWordType zzerr3[4] = {0x0,0x80,0x8,0x0};
SetWordType zzerr4[4] = {0x0,0x0,0x84,0x1};
SetWordType zzerr5[4] = {0x0,0x0,0x70,0x0};
SetWordType zzerr6[4] = {0x0,0x0,0x80,0x1};
SetWordType setwd3[26] = {0x0,0x1a,0x1a,0x1a,0x1a,0x1a,0x1a,
	0x1a,0x0,0x1a,0x1a,0x1a,0x1a,0x1a,0x1a,
	0x19,0x1a,0x1a,0x0,0x19,0x14,0x14,0x14,
	0x0,0x1a,0x0};
