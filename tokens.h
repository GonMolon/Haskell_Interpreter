#ifndef tokens_h
#define tokens_h
/* tokens.h -- List of labelled tokens and stuff
 *
 * Generated from: parser.g
 *
 * Terence Parr, Will Cohen, and Hank Dietz: 1989-2001
 * Purdue University Electrical Engineering
 * ANTLR Version 1.33MR33
 */
#define zzEOF_TOKEN 1
#define WHILE 2
#define DO 3
#define END 4
#define IF 5
#define THEN 6
#define ELSE 7
#define ASSIGN 8
#define INPUT 9
#define PRINT 10
#define EMPTY 11
#define PUSH 12
#define POP 13
#define SIZE 14
#define EQUAL 15
#define OR 16
#define AND 17
#define NOT 18
#define BIGGER 19
#define MINUS 20
#define SUM 21
#define MULT 22
#define NUM 23
#define ID 24
#define SPACE 25

#ifdef __USE_PROTOS
void program(AST**_root);
#else
extern void program();
#endif

#ifdef __USE_PROTOS
void ops(AST**_root);
#else
extern void ops();
#endif

#ifdef __USE_PROTOS
void op(AST**_root);
#else
extern void op();
#endif

#ifdef __USE_PROTOS
void input(AST**_root);
#else
extern void input();
#endif

#ifdef __USE_PROTOS
void print(AST**_root);
#else
extern void print();
#endif

#ifdef __USE_PROTOS
void empty(AST**_root);
#else
extern void empty();
#endif

#ifdef __USE_PROTOS
void pop(AST**_root);
#else
extern void pop();
#endif

#ifdef __USE_PROTOS
void push(AST**_root);
#else
extern void push();
#endif

#ifdef __USE_PROTOS
void size(AST**_root);
#else
extern void size();
#endif

#ifdef __USE_PROTOS
void assign(AST**_root);
#else
extern void assign();
#endif

#ifdef __USE_PROTOS
void whileLoop(AST**_root);
#else
extern void whileLoop();
#endif

#ifdef __USE_PROTOS
void ifCond(AST**_root);
#else
extern void ifCond();
#endif

#ifdef __USE_PROTOS
void expr(AST**_root);
#else
extern void expr();
#endif

#ifdef __USE_PROTOS
void termBool(AST**_root);
#else
extern void termBool();
#endif

#ifdef __USE_PROTOS
void termNum(AST**_root);
#else
extern void termNum();
#endif

#ifdef __USE_PROTOS
void operand(AST**_root);
#else
extern void operand();
#endif

#endif
extern SetWordType zzerr1[];
extern SetWordType setwd1[];
extern SetWordType zzerr2[];
extern SetWordType zzerr3[];
extern SetWordType setwd2[];
extern SetWordType zzerr4[];
extern SetWordType zzerr5[];
extern SetWordType zzerr6[];
extern SetWordType zzerr7[];
extern SetWordType setwd3[];
