%{

(*
	RunRadis: Run RadiScript screens
	Copyright (C) 2009, 2010, 2011  James Hahn

	This file is part of RunRadis.

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *)

%}

%token <int> INTCON
%token <float> FLOATCON PCTCON
%token <string> STRCON
%token <string> VAR SCRNM AGGVAR
%token DEFINE END USES DEBLANK KEEP SORT TOP PLUSTIES CREATE REPLACE SET
%token UNIQUE FIRST LAST PRINT BLANK ASCENDING DESCENDING WITH NOT COLON
%token ADD SOS OVERLAP PAD ADDNOPAD TO SCORE
%token AVERAGE MEDIAN SUM COUNT
%token LPAREN RPAREN OR AND IF COMMA EQ NEQ LT GT LE GE PLUS MINUS
%token TIMES DIV POW UMINUS AMPER
%token MIN MAX NOW NOW_VAR SIGN ABS LEN LEFT RIGHT MID MATCH
%token EOF

%right COMMA
%left EQ NEQ LT GT LE GE
%left PLUS MINUS AMPER
%left TIMES DIV
%left POW
%nonassoc UMINUS

%start main
%type <Op.op> main

%%

main:
		screens					{ Op.Screens(List.rev $1) }
;

screens:
		screens screen_def		{ $2::$1 }
	|	screen_def				{ [$1] }
;

screen_def:
		DEFINE SCRNM actions END	{ Op.ScreenDef($2, $3) }
;

actions:
		actions action			{ Op.Actions($1,$2) }
	|	action					{ $1 }
;

action:
		USES variables			{ Op.Uses(List.rev $2) }
	|	DEBLANK variables		{ Op.Deblank(List.rev $2) }
	|	KEEP COLON expr			{ Op.Keep($3) }
	|	SORT sort_ops			{ Op.Sort(List.rev $2) }
	|	TOP COLON INTCON tie_opt	{ Op.Top(float_of_int $3,false,$4) }
	|	TOP COLON FLOATCON tie_opt	{ Op.Top($3,true,$4) }
	|	CREATE VAR COLON expr	{ Op.Create($2,$4) }
	|	repl_stmt				{ $1 }
	|	SET VAR COLON expr		{ Op.Set($2,$4) }
	|	UNIQUE FIRST VAR		{ Op.Unique($3,true) }
	|	UNIQUE LAST VAR			{ Op.Unique($3,false) }
	|	ADD add_ops				{ $2 }
	|	ADDNOPAD add_nopad_ops	{ $2 }
	|	SOS						{ Op.Sos(1) }
	|	SOS COLON INTCON		{ Op.Sos($3) }
	|	OVERLAP					{ Op.Overlap }
	|	PAD pad_ops				{ $2 }
	|	PRINT args				{ Op.Print(List.rev $2) }
;

repl_stmt:
		REPLACE VAR COLON expr
			WITH COLON expr		{ Op.Replace($2,$4,$7) }
	|	REPLACE VAR BLANK
			WITH COLON expr		{ Op.Replace($2,Op.StrCon(""),$6) }
;

sort_ops:
		sort_ops sort_op		{ $2::$1 }
	|	sort_op					{ [$1] }
;

sort_op:
		ASCENDING VAR			{ Op.Ascending($2) }
	|	DESCENDING VAR			{ Op.Descending($2) }
;

add_ops:
		SCRNM COLON INTCON			{ Op.AddScreen($1, 1, $3, $3, true); }
	|	SCRNM COLON INTCON TO COLON INTCON	{ Op.AddScreen($1, $3, $6, $6, true); }
	|	SCRNM COLON INTCON SCORE COLON INTCON	{ Op.AddScreen($1, 1, $3, $6, true); }
	|	SCRNM COLON INTCON TO COLON INTCON SCORE COLON INTCON	{ Op.AddScreen($1, $3, $6, $9, true); }
;

add_nopad_ops:
		SCRNM COLON INTCON			{ Op.AddScreen($1, 1, $3, $3, false); }
	|	SCRNM COLON INTCON TO COLON INTCON	{ Op.AddScreen($1, $3, $6, $6, false); }
	|	SCRNM COLON INTCON SCORE COLON INTCON	{ Op.AddScreen($1, 1, $3, $6, false); }
	|	SCRNM COLON INTCON TO COLON INTCON SCORE COLON INTCON	{ Op.AddScreen($1, $3, $6, $9, false); }
;

pad_ops:
		COLON INTCON pad_list	{ Op.Pad($2, List.rev $3) }
;

pad_list:
		SCRNM COLON INTCON		{ [($1,$3)] }
	|	pad_list SCRNM COLON INTCON		{ ($2,$4)::$1 }
;

tie_opt:
		PLUSTIES				{ true }
	|							{ false }
;

variables:
		variables VAR			{ $2::$1 }
	|	VAR						{ [$1] }
;

expr:
		LPAREN expr RPAREN					{ $2 }
	|	NOT LPAREN expr RPAREN				{ Op.Not($3) }
	|	OR LPAREN args RPAREN				{ Op.opify (fun a b -> Op.Or(a,b)) (List.rev $3) }
	|	AND LPAREN args RPAREN				{ Op.opify (fun a b -> Op.And(a,b)) (List.rev $3) }
	|	IF LPAREN expr COMMA expr COMMA expr RPAREN
											{ Op.If($3,$5,$7) }

	|	MIN LPAREN args RPAREN				{ Op.opify (fun a b -> Op.Min(a,b)) (List.rev $3) }
	|	MAX LPAREN args RPAREN				{ Op.opify (fun a b -> Op.Max(a,b)) (List.rev $3) }
	|	LEFT LPAREN expr COMMA expr RPAREN	{ Op.Left($3, $5) }
	|	RIGHT LPAREN expr COMMA expr RPAREN	{ Op.Right($3, $5) }
	|	MID LPAREN expr COMMA expr COMMA expr RPAREN		{ Op.Mid($3, $5, $7) }
	|	MATCH LPAREN expr COMMA STRCON COMMA INTCON RPAREN	{ Op.Match($3, $5, $7 > 0) }
	|	MATCH LPAREN expr COMMA STRCON RPAREN	{ Op.Match($3, $5, true) }

	|	expr EQ expr						{ Op.Eq($1,$3) }
	|	expr NEQ expr						{ Op.Neq($1,$3) }
	|	expr LT expr						{ Op.Lt($1,$3) }
	|	expr GT expr						{ Op.Gt($1,$3) }
	|	expr LE expr						{ Op.Le($1,$3) }
	|	expr GE expr						{ Op.Ge($1,$3) }
	|	expr AMPER expr						{ Op.Concat($1,$3) }
	|	expr PLUS expr						{ Op.Add($1,$3) }
	|	expr MINUS expr						{ Op.Sub($1,$3) }
	|	expr TIMES expr						{ Op.Mult($1,$3) }
	|	expr DIV expr						{ Op.Div($1,$3) }
	|	expr POW expr						{ Op.Pow($1,$3) }
	|	MINUS expr %prec UMINUS				{ Op.Neg($2) }
	|	VAR									{ Op.Sym($1) }
	|	STRCON								{ Op.StrCon($1) }
	|	INTCON								{ Op.NumCon(float_of_int $1) }
	|	FLOATCON							{ Op.NumCon($1) }
	|	NOW_VAR								{ Op.CurTime }
	|	NOW LPAREN RPAREN					{ Op.CurTime }
	|	AVERAGE LPAREN AGGVAR RPAREN		{ Op.Average($3) }
	|	MEDIAN LPAREN AGGVAR RPAREN			{ Op.Median($3) }
	|	SUM LPAREN AGGVAR RPAREN			{ Op.Sum($3) }
	|	COUNT LPAREN AGGVAR RPAREN			{ Op.Count($3) }
	|	SIGN LPAREN expr RPAREN				{ Op.Sign($3) }
	|	ABS LPAREN expr RPAREN				{ Op.Abs($3) }
	|	LEN LPAREN expr RPAREN				{ Op.Length($3) }
;

args:
		args COMMA expr				{ $3::$1 }
	|	expr						{ [$1] }
;
