%{
	#include <stdio.h>
	#include <stdlib.h>
	#include "t2c.h"
	#include "t_parse.h"
%}

%token lWRITE lREAD lIF lASSIGN
%token lRETURN lBEGIN lEND
%left  lEQU lNEQ lGT lLT lGE lLE
%left  lADD lMINUS
%left  lTIMES lDIVIDE
%token lLP lRP
%token lINT lREAL lSTRING
%token lELSE
%token lMAIN
%token lSEMI lCOMMA
%token lID lINUM lRNUM lQSTR

%union { int iv;
         float rv;
         char* sr;
       }

%type <sr> lID
%type <iv> lINUM
%type <rv> lRNUM
%type <sr> lQSTR

%expect 1

%%
prog	:	mthdcls
		{ printf("Program -> MethodDecls\n");
		printf("Parsed OK!\n"); }
	|
		{ printf("****** Parsing failed!\n"); }	
	;

mthdcls	:	mthdcl mthdcls
		{ printf("MethodDecls -> MethodDecl MethodDecls\n"); }	
	|	mthdcl
		{ printf("MethodDecls -> MethodDecl\n"); }	
	;

type	:	lINT
		{ printf("Type -> INT\n"); }	
	|	lREAL
		{ printf("Type -> REAL\n"); }	
	;

mthdcl	:	type lMAIN lID lLP formals lRP Block
		{ printf("MethodDecl -> Type MAIN ID LP Formals RP Block\n"); }	
	|	type lID lLP formals lRP Block
		{ printf("MethodDecl -> Type ID LP Formals RP Block\n"); }	
	;

formals	:	formal oformal
		{ printf("Formals -> Formal OtherFormals\n"); }	
	|
		{ printf("Formals -> \n"); }	
	;

formal	:	type lID
		{ printf("Formal -> Type ID\n"); }	
	;

oformal	:	lCOMMA formal oformal
		{ printf("OtherFormals -> COMMA Formal OtherFormals\n"); }	
	|
		{ printf("OtherFormals -> \n"); }	
	;

        
Block   :   lBEGIN stmts lEND
        { printf("block ok!\n"); }    
        ;

stmts   :   stmt stmts
        |   stmt
        ;

stmt    :   Block
        |   vardcl
        |   astm
        |   rstm
        |   istm
        |   wstm
        |   dstm
        ;

vardcl: type lID lSEMI
           { printf("LocalVarDecl -> Type ID SEMI\n"); }
           | type lID lASSIGN expr lSEMI
           { printf("LocalVarDecl -> Type ID ASSIGN Expr SEMI\n"); }
           ;


astm: lID lASSIGN expr lSEMI
          { printf("AssignStmt -> ID ASSIGN Expr SEMI\n"); }
          ;

rstm: lRETURN expr lSEMI
          { printf("ReturnStmt -> RETURN Expr SEMI\n"); }
          ;

istm: lIF lLP bexpr lRP stmt
       { printf("IfStmt -> IF LP BoolExpr RP Stmt\n"); }
       | lIF lLP bexpr lRP stmt lELSE stmt
       { printf("IfStmt -> IF LP BoolExpr RP Stmt ELSE Stmt\n"); }
       ;

wstm: lWRITE lLP expr lCOMMA lQSTR lRP lSEMI
         { printf("WriteStmt -> WRITE LP Expr COMMA QSTR RP SEMI\n"); }
         ;

dstm: lREAD lLP lID lCOMMA lQSTR lRP lSEMI
        { printf("ReadStmt -> READ LP ID COMMA QSTR RP SEMI\n"); }
        ;

expr: mexpr mexprs
    { printf("Expr -> MExpr MoreExprs\n"); }
    ;

mexprs: lADD mexpr mexprs
      { printf("MExprs -> ADD MExpr MExprs\n"); }
      | lMINUS mexpr mexprs
      { printf("MExprs -> MINUS MExpr MExprs\n"); }
      |
      { printf("MExprs -> \n"); }
      ;

mexpr: pexpr pexprs
     { printf("MExpr -> PExpr PExprs\n"); }
     ;

pexprs: lTIMES pexpr pexprs
      { printf("PExprs -> TIMES PExpr PExprs\n"); }
      | lDIVIDE pexpr pexprs
      { printf("PExprs -> DIVIDE PExpr PExprs\n"); }
      |
      { printf("PExprs -> \n"); }
      ;

pexpr: lINUM
      { printf("PExpr -> INUM\n"); }
      | lRNUM
      { printf("PExpr -> RNUM\n"); }
      | lID
      { printf("PExpr -> ID\n"); }
      | lLP expr lRP
      { printf("PExpr -> LP Expr RP\n"); }
      | lID lLP aparams lRP
      { printf("PExpr -> ID LP ActualParams RP\n"); }
        ;

bexpr: expr lEQU expr
        { printf("BoolExpr -> Expr EQU Expr\n"); }
        | expr lNEQ expr
        { printf("BoolExpr -> Expr NEQ Expr\n"); }
        | expr lGT expr
        { printf("BoolExpr -> Expr GT Expr\n"); }
        | expr lGE expr
        { printf("BoolExpr -> Expr GE Expr\n"); }
        | expr lLT expr
        { printf("BoolExpr -> Expr LT Expr\n"); }
        | expr lLE expr
        { printf("BoolExpr -> Expr LE Expr\n"); }
        ;

aparams: expr oparams
            { printf("ActualParams -> Expr MoreExprs\n"); }
            |
            { printf("ActualParams -> \n"); }
            ;

oparams: lCOMMA expr oparams
         { printf("MoreExprs -> COMMA Expr MoreExprs\n"); }
         |
         { printf("MoreExprs -> \n"); }
         ;
%%

int yyerror(char *s)
{
    printf("%s\n", s);
    return 1;
}
