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
#define zzSET_SIZE 4
#include "antlr.h"
#include "ast.h"
#include "tokens.h"
#include "dlgdef.h"
#include "err.h"

ANTLRChar *zztokens[27]={
	/* 00 */	"Invalid",
	/* 01 */	"@",
	/* 02 */	"WHILE",
	/* 03 */	"DO",
	/* 04 */	"END",
	/* 05 */	"IF",
	/* 06 */	"THEN",
	/* 07 */	"ELSE",
	/* 08 */	"ASIG",
	/* 09 */	"INPUT",
	/* 10 */	"PRINT",
	/* 11 */	"EMPTY",
	/* 12 */	"PUSH",
	/* 13 */	"POP",
	/* 14 */	"SIZE",
	/* 15 */	"EQUAL",
	/* 16 */	"AND",
	/* 17 */	"OR",
	/* 18 */	"NOT",
	/* 19 */	"BIGGER",
	/* 20 */	"LESS",
	/* 21 */	"MINUS",
	/* 22 */	"SUM",
	/* 23 */	"MULT",
	/* 24 */	"NUM",
	/* 25 */	"ID",
	/* 26 */	"SPACE"
};
SetWordType zzerr1[4] = {0x24,0x7e,0x0,0x2};
SetWordType setwd1[27] = {0x0,0xfd,0xfa,0x0,0xfc,0xfa,0x0,
	0xfc,0x0,0xfa,0xfa,0xfa,0xfa,0xfa,0xfa,
	0x0,0x0,0x0,0x0,0x0,0x0,0x0,0x0,
	0x0,0x0,0xfa,0x0};
SetWordType zzerr2[4] = {0x90,0x0,0x0,0x0};
SetWordType zzerr3[4] = {0x0,0x0,0x3,0x0};
SetWordType setwd2[27] = {0x0,0xbf,0xbf,0x80,0xbf,0xbf,0x80,
	0xbf,0x0,0xbf,0xbf,0xbf,0xbf,0xbf,0xbf,
	0x0,0x40,0x40,0x0,0x0,0x0,0x0,0x0,
	0x0,0x0,0xbf,0x0};
SetWordType zzerr4[4] = {0x0,0x0,0x18,0x0};
SetWordType zzerr5[4] = {0x0,0x0,0xe0,0x0};
SetWordType zzerr6[4] = {0x0,0x0,0x0,0x3};
SetWordType setwd3[27] = {0x0,0x1a,0x12,0x12,0x12,0x12,0x12,
	0x12,0x0,0x12,0x12,0x12,0x12,0x12,0x12,
	0x0,0x12,0x12,0x0,0x11,0x11,0x14,0x14,
	0x14,0x0,0x12,0x0};
