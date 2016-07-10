
{

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

open Parser;;

exception Syntax_error of string;;

let pos_msg lexbuf =
	let p = Lexing.lexeme_start_p lexbuf in
	p.Lexing.pos_fname
		^ ":" ^ (string_of_int p.Lexing.pos_lnum)
	;;

let syntax_error lexbuf msg =
	raise (Syntax_error ((pos_msg lexbuf) ^ ": " ^ msg));;

let keyword_table = Hashtbl.create 53
  let _ =
    List.iter
		(fun (kwd, tok) ->
			Hashtbl.add keyword_table (String.uppercase kwd) tok)
      [ "Define", DEFINE;
		"End", END;
		"Uses", USES;
		"Deblank", DEBLANK;
		"Keep", KEEP;
		"Sort", SORT;
		"Top", TOP;
		"PlusTies", PLUSTIES;
		"Create", CREATE;
		"Replace", REPLACE;
		"Set", SET;
		"Add", ADD;
		"AddNoPad", ADDNOPAD;
		"Sos", SOS;
		"Overlap", OVERLAP;
		"Pad", PAD;
		"Unique", UNIQUE;
		"To", TO;
		"Score", SCORE;
		"First", FIRST;
		"Last", LAST;
		"Print", PRINT;
		"Blank", BLANK;
		"Ascending", ASCENDING;
		"Descending", DESCENDING;
		"With", WITH;
		"NOW", NOW;
		"OR", OR;
		"AND", AND;
		"NOT", NOT;
		"IF", IF;
		"AVERAGE", AVERAGE;
		"MEDIAN", MEDIAN;
		"SUM", SUM;
		"COUNT", COUNT;
		"MIN", MIN;
		"MAX", MAX;
		"SIGN", SIGN;
		"ABS", ABS;
		"LEN", LEN;
		"LEFT", LEFT;
		"RIGHT", RIGHT;
		"MID", MID;
		"MATCH", MATCH ];;

let spec_var_table = Hashtbl.create 53
  let _ =
    List.iter
		(fun (varnm, tok) ->
			Hashtbl.add spec_var_table (Util.normalize_name varnm) tok)
      [ "[SI Weekly data date]", NOW_VAR ];;

(*
 * This wraps an input channel and discards the text from every line that
 * does not appear between a Define/End block
 *)

class def_reader chnl =
	let defbeg = Str.regexp_case_fold "^[ ]*Define" in
	let defend = Str.regexp_case_fold "^[ ]*End" in

	object

		val mutable buf = ""		(* latest line read *)
		val mutable len = 0			(* buffer length *)
		val mutable idx = 0			(* next place to read from buf *)
		val mutable in_def = false	(* whether or not we're in a def block *)

		method read s n =
			try
				if idx >= len then	(* need to read more *)
					begin
						if in_def && Str.string_match defend buf 0 then
							in_def <- false;
								(* found "End" - no longer in a definition *)

						if in_def then	(* in a def - want the whole line *)
							buf <- (Util.chomp (input_line chnl)) ^ "\n"
						else
							begin
								buf <- (Util.chomp (input_line chnl)) ^ "\n";
								if Str.string_match defbeg buf 0 then
									in_def <- true
										(* line starts a Definition *)
								else
									buf <- "\n"
										(* discard - just return newline *)
							end;

						len <- String.length buf;
						idx <- 0
					end;

				let nr = min n (len - idx) in
				String.blit buf idx s 0 nr;
				idx <- idx + nr;
				nr

			with End_of_file -> 0
	end;;

let def_read chnl =
	let rdr = new def_reader chnl in
	rdr#read;;
}

rule token = parse

		';' [^ '\n']* '\n'	
			{
				(* skip comments *)
				Lexing.new_line lexbuf;
				token lexbuf
			}

	|	['\n']
			{
				(* skip blanks *)
				Lexing.new_line lexbuf;
				token lexbuf
			}

	|	[' ' '\t' '\r']		{ token lexbuf }		(* skip blanks *)

	|	(['0'-'9']+ '.' ['0'-'9']* as lxm) '%'
			{
				FLOATCON((float_of_string lxm) /. 100.0)
			}

	|	['0'-'9']+ '.' ['0'-'9']* ('e' ('-')? ['0'-'9']+)?	as lxm
			{
				FLOATCON(float_of_string lxm)
			}

	|	['0'-'9']+ 'e' ('-')? ['0'-'9']+	as lxm
			{
				FLOATCON(float_of_string lxm)
			}

	|	(['0'-'9']+ as lxm) '%'
			{
				FLOATCON((float_of_string lxm) /. 100.0)
			}

	|	['0'-'9']+ as lxm
			{
				INTCON(int_of_string lxm)
			}

	|	('.' ['0'-'9']* as lxm) '%'
			{
				FLOATCON((float_of_string lxm) /. 100.0)
			}

	|	'.' ['0'-'9']* ('e' ('-')? ['0'-'9']+)?	as lxm
			{
				FLOATCON(float_of_string lxm)
			}

	|	'[' [^ ']' '\t' '\n']+ ']' as lxm
			{
				let normvar = Util.normalize_name lxm in
				try
					Hashtbl.find spec_var_table normvar
				with Not_found -> VAR(normvar)
			}

	|	'{' ([^ '}' '\t' '\n']+ as lxm) '}'
			{
				SCRNM(lxm)
			}

	|	'[' ('[' [^ ']' '\t' '\n']+ ']' as lxm) ']'
			{
				AGGVAR(Util.normalize_name lxm)
			}

	|	'"' (['0'-'9']+ as lxm) '-' [^ '"']* '"'
			{
				INTCON(int_of_string lxm)
			}

	|	'"' (['0'-'9']+ '.' ['0'-'9']* as lxm) [^ '"']* '"'
			{
				FLOATCON(float_of_string lxm)
			}

	|	'"' ([^ '"']* as lxm) '"'	{ STRCON(lxm) }

	|	['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm
			{
				try
					Hashtbl.find keyword_table (String.uppercase lxm)
				with Not_found ->
					syntax_error lexbuf ("'" ^ lxm ^ "' not a keyword")
			}

	(* #VALUE!, etc. ??? *)

	|	'('				{ LPAREN }
	|	')'				{ RPAREN }
	|	'='				{ EQ }
	|	"<>"			{ NEQ }
	|	'<'				{ LT }
	|	'>'				{ GT }
	|	"<="			{ LE }
	|	">="			{ GE }
	|	'+'				{ PLUS }
	|	'-'				{ MINUS }
	|	'*'				{ TIMES }
	|	'/'				{ DIV }
	|	'^'				{ POW }
	|	'&'				{ AMPER }
	|	','				{ COMMA }
	|	':'				{ COLON }
	|	eof				{ EOF }
