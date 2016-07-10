
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

open Fieldindex;;
open Passdata;;
open Op;;

exception Recursive_screen_error of string;;
	(* indicates that a screen is called recursively *)

exception Undefined_screen_error of string;;

exception Syntax_error of string;;

type tscrfunc = Equity.equity list -> Equity.equity list;;


(*
 * Maps screen-name to its parsed actions.  Populated during parse.  Screen
 * names are in lower case
 *)

let screen2act = Hashtbl.create 20;;


(*
 * Maps screen-name to its function.  Populated during compile.  When the
 * compiler begins to compile the actions for a screen, it initially
 * populates this with a function that raises a Recursive_screen_error
 * exception.  Once compilation completes, this is overwritten by the real
 * function
 *)

let screen2func = Hashtbl.create 20;;


(*
 * Determines if a number is "ok".  Returns true if it is, false if a
 * floating-point error occurred
 *)

let numok v =
	match classify_float v with
		FP_normal -> true
	|	FP_subnormal -> true
	|	FP_zero -> true
	|	_ -> false;;


(*
 * Returns a function to get an indirect value for an equity, for example,
 * gets the industry code from the equity record, and then extracts the
 * relevant field from the associated industry record.
 *
 *	sym			symbol representing the field to be retrieved (low-level name)
 *	get_field	function to extract the field from the industry/sector record
 *	get_rec		function to extract the associated record from the
 *				industry/sector data
 *)

let get_indir sym get_field get_rec =
	let fa = get_field sym in
	function e -> fa (get_rec e);;


(*
 * Maps a symbol to its low-level symbol, and then returns a function to
 * extract the data from the field, whether it be in the equity record or
 * the associated industry, sector, or universal record
 *)

let do_indir get_field a =
	let sym = indir (mapname a) in
	match sym with
		IndVar(n) -> get_indir sym get_field passdata#industry
	|	SecVar(n) -> get_indir sym get_field passdata#sector
	|	UnivVar(n) ->
			begin
				let fa = get_field sym in
				function e -> fa passdata#universal
			end
	|	GlobVar(n) ->
			begin
				let fa = get_field sym in
				function e -> fa passdata#global
			end
	|	_ -> get_field sym;;


(* register a temp var, in both temp table and field index *)

let register_var varnm =
	add_temp varnm;
	ignore (ifield (TempVar varnm));;


(* raised when Sym-style operators are disallowed *)

let nosym = Syntax_error "non-aggregate within 'Set'";;


(* raised when aggregate-style (e.g., Median) operators are disallowed *)

let noagg = Syntax_error "aggregate not within 'Set'";;


(* map a value to itself *)

let identity v = v;;


(*
 * Determine the data type for a comparison function
 *)

type cmptype =
	Numcmp
|	Datecmp
|	Boolcmp
|	Strcmp
|	Anycmp
;;

let cmp_type x y =
	match (x,y) with
		(Any(a),Any(b)) -> Anycmp
	|	(Date(a),b)|(a,Date(b)) -> Datecmp
	|	(Num(a),b)|(a,Num(b)) -> Numcmp
	|	(Bool(a),b)|(a,Bool(b)) -> Boolcmp
	|	_ -> Strcmp
;;


(*
 *	t		type of value returned by these functions (e.g., float, string)
 *	fget	function to get a value of the appropriate type from a record
 *	bool2t	function to convert a bool into the appropriate type
 *	float2t	function to convert a float into the appropriate type
 *)
module type GetConv =
	sig
		type t

		val fget : symbol -> (Equity.equity -> t)
		val bool2t : bool -> t
		val float2t : float -> t
	end;;

(*
 * Generates a function that takes an equity and returns a value associated
 * with a Sym-style operator.  Rejects aggregate-style operators.
 *)
module EqSymGen = functor (GC : GetConv) ->
	struct
		let gen p =

			(* returns a function that performs a boolean check on an
				equity field and then converts the result to type "t" *)

			let boolit fcheck_field a =
				let fcheck = do_indir fcheck_field a in
				function e -> GC.bool2t (fcheck e)
			in

			match p with

				Sym(a) -> do_indir GC.fget a

			|	Null(a) -> boolit null_field a

			|	Exists(a) -> boolit exists_field a

			|	_ -> raise noagg
	end;;

(*
 * Generates a function that takes an equity list and returns a value
 * associated with a low-level operator.  Accepts aggregate-style operators,
 * but only accepts Sym-style operators for symbols that appear in the
 * global temp table.
 *)
module EqListSymGen = functor (GC : GetConv) ->
	struct
		let gen p =

			(* returns a function that extracts a field from the global temp
				record *)

			let glob_val a fop =
				let m = mapname a in
				match m with
					GlobVar(v) -> fop m
				|	_ -> raise nosym
			in

			(* returns a function that extracts a field from each record,
				discards those that are empty, and converts the remainder to
				numbers *)

			let to_num a =
				let getstr = do_indir get_string_field a in
				let addit lst e =
					try
						let s = getstr e in
						if s = "" then lst
						else
							let v = float_of_string s in
							v::lst
					with Failure("float_of_string") -> lst
				in
				function eqlst -> List.fold_left addit [] eqlst
			in

			(* returns a function that performs a boolean check on a
				global field and then converts the result to type "t" *)

			let boolit fcheck_field a =
				let fcheck = glob_val a fcheck_field in
				function eqlst -> GC.bool2t (fcheck passdata#global)
			in

			(* returns a function that takes a list of equities, converts
				it to a list of numbers, merges the list of numbers into
				a single number, and then converts the result to type "t" *)

			let floatit fcombine fnum =
				function eqlst -> GC.float2t (fcombine (fnum eqlst))
			in

			match p with

				Sym(a) ->
					let fcheck = glob_val a GC.fget in
					(function eqlst -> fcheck passdata#global)

			|	Null(a) -> boolit null_field a

			|	Exists(a) -> boolit exists_field a

			|	Average(a) -> floatit Util.average (to_num a)

			|	Median(a) -> floatit Util.median (to_num a)

			|	Sum(a) -> floatit (List.fold_left (+.) 0.0) (to_num a)

			|	Count(a) -> floatit float_of_int List.length

			|	_ -> raise nosym
	end;;

module FloatGetConv = 
	struct
		type t = float
		let fget = get_float_field
		let bool2t = cfloat_of_bool
		let float2t = identity
	end;;

module DateGetConv = 
	struct
		type t = float
		let fget = get_date_field
		let bool2t = cdate_of_bool
		let float2t = identity
	end;;

module BoolGetConv = 
	struct
		type t = bool
		let fget = get_bool_field
		let bool2t = identity
		let float2t = cbool_of_float
	end;;

module StrGetConv = 
	struct
		type t = string
		let fget = get_string_field
		let bool2t = cstring_of_bool
		let float2t = cstring_of_float
	end;;

module EqSymFloatGen = EqSymGen(FloatGetConv);;
module EqSymDateGen = EqSymGen(DateGetConv);;
module EqSymBoolGen = EqSymGen(BoolGetConv);;
module EqSymStrGen = EqSymGen(StrGetConv);;

module EqListSymFloatGen = EqListSymGen(FloatGetConv);;
module EqListSymDateGen = EqListSymGen(DateGetConv);;
module EqListSymBoolGen = EqListSymGen(BoolGetConv);;
module EqListSymStrGen = EqListSymGen(StrGetConv);;


(*
 * Low-level symbol generators for each data type.
 *)
module type SymGen =
	sig
		type t
		val gen_float : Op.op -> (t -> float)
		val gen_date : Op.op -> (t -> float)
		val gen_bool : Op.op -> (t -> bool)
		val gen_str : Op.op -> (t -> string)
	end;;

(*
 * Generators for high-level expressions of various data types
 *)

module ExprGen =
	functor (SG : SymGen) ->
		struct

(*
 * Returns a function that computes an expression of one type and then
 * converts it to another type
 *)
let conv_gen fgen fconv expr =
	let fexpr = fgen expr in
	begin
		function e -> fconv (fexpr e)
	end


(*
 * Returns a function that evaluates an expression and returns a particular
 * type.
 *	v		expression value/op
 *)
let rec num_gen v =
	let sign a = if a < 0.0 then (-1.0) else if a > 0.0 then 1.0 else 0.0 in
	let bin_op a b =
		let fa = num_gen a in
		let fb = num_gen b in
		(fa,fb)
	in
	match v with
		Add(a,b) ->
			let (fa,fb) = bin_op a b in
			(function e -> (fa e) +. (fb e))
	|	Sub(a,b) ->
			let (fa,fb) = bin_op a b in
			(function e -> (fa e) -. (fb e))
	|	Mult(a,b) ->
			let (fa,fb) = bin_op a b in
			(function e -> (fa e) *. (fb e))
	|	Div(a,b) ->
			let (fa,fb) = bin_op a b in
			(function e -> (fa e) /. (fb e))
	|	Pow(a,b) ->
			let (fa,fb) = bin_op a b in
			(function e -> (fa e) ** (fb e))
	|	Neg(a) ->
			let fa = num_gen a in
			(function e -> ~-. (fa e))
	|	Sym(a) -> SG.gen_float v
	|	NumCon(a) ->
			begin
				function e -> a
			end
	|	If(a,b,c) ->
			let ba = bool_gen a in
			let fb = num_gen b in
			let fc = num_gen c in
			begin
				function e ->
					if ba e then fb e else fc e
			end
	|	Average(a) -> SG.gen_float v
	|	Median(a) -> SG.gen_float v
	|	Sum(a) -> SG.gen_float v
	|	Count(a) -> SG.gen_float v
	|	Min(a,b) ->
			let (fa,fb) = bin_op a b in
			(function e -> min (fa e) (fb e))
	|	Max(a,b) ->
			let (fa,fb) = bin_op a b in
			(function e -> max (fa e) (fb e))
	|	Sign(a) ->
			let fa = num_gen a in
			(function e -> sign (fa e))
	|	Abs(a) ->
			let fa = num_gen a in
			(function e -> abs_float (fa e))
	|	Length(a) ->
			let fa = str_gen a in
			begin
				function e -> float_of_int (String.length (fa e))
			end
	|	Match(a,b,c) ->
			let fa = str_gen a in
			let re =
				if c then Str.regexp (Util.norm_re b)
				else Str.regexp_case_fold (Util.norm_re b)
			in
			begin
				function e ->
					try
						float_of_int ((Str.search_forward re (fa e) 0) + 1)
					with Not_found -> 0.0
			end

	|	Str(Sym(a)) -> num_gen (Sym(a))

	|	Num(a) -> num_gen a
	|	Date(a) -> date_gen a
	|	Bool(a) -> conv_gen bool_gen cfloat_of_bool a
	|	Str(a) -> conv_gen str_gen cfloat_of_string a
	|	Any(a) -> num_gen a
	|	_ -> raise (Internal_error "not a numeric expression")


and date_gen v =
	match v with
		Sym(a) -> SG.gen_date v
	|	CurTime ->
			begin
				function e -> passdata#curtime ()
			end
	|	If(a,b,c) ->
			let ba = bool_gen a in
			let fb = date_gen b in
			let fc = date_gen c in
			begin
				function e ->
					if ba e then fb e else fc e
			end

	|	Str(Sym(a)) -> date_gen (Sym(a))
			
	|	Num(a) -> num_gen a
	|	Date(a) -> date_gen a
	|	Bool(a) -> conv_gen bool_gen cdate_of_bool a
	|	Str(a) -> conv_gen str_gen cdate_of_string a
	|	Any(a) -> date_gen a

	|	_ -> num_gen v


and bool_gen v =
	match v with
		Eq(a,b) ->
			begin
				match cmp_type a b with
					Numcmp ->
						let fa = num_gen a in
						let fb = num_gen b in
						(function e -> (fa e) = (fb e))
				|	Datecmp ->
						let fa = date_gen a in
						let fb = date_gen b in
						(function e -> (fa e) = (fb e))
				|	Boolcmp ->
						let fa = bool_gen a in
						let fb = bool_gen b in
						(function e -> (fa e) = (fb e))
				|	Strcmp ->
						let fa = str_gen a in
						let fb = str_gen b in
						(function e -> (fa e) = (fb e))
				|	Anycmp ->
						let fa = str_gen a in
						let fb = str_gen b in
						(function e -> Util.compare_any (=) (=) (fa e) (fb e))
			end
	|	Neq(a,b) ->
			begin
				match cmp_type a b with
					Numcmp ->
						let fa = num_gen a in
						let fb = num_gen b in
						(function e -> (fa e) <> (fb e))
				|	Datecmp ->
						let fa = date_gen a in
						let fb = date_gen b in
						(function e -> (fa e) <> (fb e))
				|	Boolcmp ->
						let fa = bool_gen a in
						let fb = bool_gen b in
						(function e -> (fa e) <> (fb e))
				|	Strcmp ->
						let fa = str_gen a in
						let fb = str_gen b in
						(function e -> (fa e) <> (fb e))
				|	Anycmp ->
						let fa = str_gen a in
						let fb = str_gen b in
						(function e -> Util.compare_any (<>) (<>) (fa e) (fb e))
			end
	|	Lt(a,b) ->
			begin
				match cmp_type a b with
					Numcmp ->
						let fa = num_gen a in
						let fb = num_gen b in
						(function e -> (fa e) < (fb e))
				|	Datecmp ->
						let fa = date_gen a in
						let fb = date_gen b in
						(function e -> (fa e) < (fb e))
				|	Boolcmp ->
						let fa = bool_gen a in
						let fb = bool_gen b in
						(function e -> (fa e) < (fb e))
				|	Strcmp ->
						let fa = str_gen a in
						let fb = str_gen b in
						(function e -> (fa e) < (fb e))
				|	Anycmp ->
						let fa = str_gen a in
						let fb = str_gen b in
						(function e -> Util.compare_any (<) (<) (fa e) (fb e))
			end
	|	Gt(a,b) ->
			begin
				match cmp_type a b with
					Numcmp ->
						let fa = num_gen a in
						let fb = num_gen b in
						(function e -> (fa e) > (fb e))
				|	Datecmp ->
						let fa = date_gen a in
						let fb = date_gen b in
						(function e -> (fa e) > (fb e))
				|	Boolcmp ->
						let fa = bool_gen a in
						let fb = bool_gen b in
						(function e -> (fa e) > (fb e))
				|	Strcmp ->
						let fa = str_gen a in
						let fb = str_gen b in
						(function e -> (fa e) > (fb e))
				|	Anycmp ->
						let fa = str_gen a in
						let fb = str_gen b in
						(function e -> Util.compare_any (>) (>) (fa e) (fb e))
			end
	|	Le(a,b) ->
			begin
				match cmp_type a b with
					Numcmp ->
						let fa = num_gen a in
						let fb = num_gen b in
						(function e -> (fa e) <= (fb e))
				|	Datecmp ->
						let fa = date_gen a in
						let fb = date_gen b in
						(function e -> (fa e) <= (fb e))
				|	Boolcmp ->
						let fa = bool_gen a in
						let fb = bool_gen b in
						(function e -> (fa e) <= (fb e))
				|	Strcmp ->
						let fa = str_gen a in
						let fb = str_gen b in
						(function e -> (fa e) <= (fb e))
				|	Anycmp ->
						let fa = str_gen a in
						let fb = str_gen b in
						(function e -> Util.compare_any (<=) (<=) (fa e) (fb e))
			end
	|	Ge(a,b) ->
			begin
				match cmp_type a b with
					Numcmp ->
						let fa = num_gen a in
						let fb = num_gen b in
						(function e -> (fa e) >= (fb e))
				|	Datecmp ->
						let fa = date_gen a in
						let fb = date_gen b in
						(function e -> (fa e) >= (fb e))
				|	Boolcmp ->
						let fa = bool_gen a in
						let fb = bool_gen b in
						(function e -> (fa e) >= (fb e))
				|	Strcmp ->
						let fa = str_gen a in
						let fb = str_gen b in
						(function e -> (fa e) >= (fb e))
				|	Anycmp ->
						let fa = str_gen a in
						let fb = str_gen b in
						(function e -> Util.compare_any (>=) (>=) (fa e) (fb e))
			end
	|	Sym(a) -> SG.gen_bool v
	|	True ->
			begin
				function e -> true
			end
	|	False ->
			begin
				function e -> false
			end
	|	Not(a) ->
			let fa = bool_gen a in
			begin
				function e -> not (fa e)
			end
	|	Or(a,b) ->
			let fa = bool_gen a in
			let fb = bool_gen b in
			begin
				function e -> (fa e) || (fb e)
			end
	|	And(a,b) ->
			let fa = bool_gen a in
			let fb = bool_gen b in
			begin
				function e -> (fa e) && (fb e)
			end
	|	If(a,b,c) ->
			let fa = bool_gen a in
			let fb = bool_gen b in
			let fc = bool_gen c in
			begin
				function e ->
					if fa e then fb e else fc e
			end
	|	Null(a) -> SG.gen_bool v
	|	Exists(a) -> SG.gen_bool v

	|	Str(Sym(a)) -> bool_gen (Sym(a))

	|	Num(a) -> conv_gen num_gen cbool_of_float a
	|	Date(a) -> conv_gen date_gen cbool_of_date a
	|	Bool(a) -> bool_gen a
	|	Str(a) -> conv_gen str_gen cbool_of_string a
	|	Any(a) -> bool_gen a
	|	_ -> raise (Internal_error "not a boolean expression")


and str_gen v =
	match v with
		Sym(a) -> SG.gen_str v
	|	StrCon(a) ->
			begin
				function e -> a
			end
	|	If(a,b,c) ->
			let ba = bool_gen a in
			let fb = str_gen b in
			let fc = str_gen c in
			begin
				function e ->
					if ba e then fb e else fc e
			end

	|	Left(a,Num(NumCon(b))) ->
			let fa = str_gen a in
			let ib = int_of_float b in
			begin
				function e -> Util.string_left (fa e) ib
			end
	|	Left(a,b) ->
			let fa = str_gen a in
			let fb = num_gen b in
			begin
				function e -> Util.string_left (fa e) (int_of_float (fb e))
			end

	|	Right(a,Num(NumCon(b))) ->
			let fa = str_gen a in
			let ib = int_of_float b in
			begin
				function e -> Util.string_right (fa e) ib
			end
	|	Right(a,b) ->
			let fa = str_gen a in
			let fb = num_gen b in
			begin
				function e -> Util.string_right (fa e) (int_of_float (fb e))
			end

	|	Mid(a,Num(NumCon(b)),Num(NumCon(c))) ->
			let fa = str_gen a in
			let ib = int_of_float b in
			let ic = int_of_float c in
			begin
				function e -> Util.string_mid (fa e) ib ic
			end
	|	Mid(a,Num(NumCon(b)),c) ->
			let fa = str_gen a in
			let ib = int_of_float b in
			let fc = num_gen c in
			begin
				function e -> Util.string_mid (fa e) ib (int_of_float (fc e))
			end
	|	Mid(a,b,Num(NumCon(c))) ->
			let fa = str_gen a in
			let fb = num_gen b in
			let ic = int_of_float c in
			begin
				function e -> Util.string_mid (fa e) (int_of_float (fb e)) ic
			end
	|	Mid(a,b,c) ->
			let fa = str_gen a in
			let fb = num_gen b in
			let fc = num_gen c in
			begin
				function e ->
					Util.string_mid
						(fa e)
						(int_of_float (fb e))
						(int_of_float (fc e))
			end

	|	Concat(Str(StrCon(a)), b) ->
			let fb = str_gen b in
			begin
				function e -> a ^ (fb e)
			end
	|	Concat(a, Str(StrCon(b))) ->
			let fa = str_gen a in
			begin
				function e -> (fa e) ^ b
			end
	|	Concat(a,b) ->
			let fa = str_gen a in
			let fb = str_gen b in
			begin
				function e -> (fa e) ^ (fb e)
			end

	|	Num(Sym(a)) -> str_gen (Sym(a))
	|	Date(Sym(a)) -> str_gen (Sym(a))
	|	Bool(Sym(a)) -> str_gen (Sym(a))

	|	Num(a) -> conv_gen num_gen cstring_of_float a
	|	Date(a) -> conv_gen date_gen cstring_of_date a
	|	Bool(a) -> conv_gen bool_gen cstring_of_bool a
	|	Str(a) -> str_gen a
	|	Any(a) -> str_gen a
	|	_ -> raise (Internal_error "not a string expression")

end;;


module EqExprGen = ExprGen(
	struct
		type t = Equity.equity
		let gen_float = EqSymFloatGen.gen
		let gen_date = EqSymDateGen.gen
		let gen_bool = EqSymBoolGen.gen
		let gen_str = EqSymStrGen.gen
	end);;

module EqListExprGen = ExprGen(
	struct
		type t = Equity.equity list
		let gen_float = EqListSymFloatGen.gen
		let gen_date = EqListSymDateGen.gen
		let gen_bool = EqListSymBoolGen.gen
		let gen_str = EqListSymStrGen.gen
	end);;


(*
 * Compile a particular screen.  Returns a function to evaluate the screen
 *)

let rec compile screenm =
	let snm = String.lowercase screenm in
	if Hashtbl.mem screen2func snm then
		Hashtbl.find screen2func snm
	else if Hashtbl.mem screen2act snm then
		begin
			let tv = save_temp () in	(* save the temp table *)
			(* create temporary place-holder in case screen is called
				recursively *)
			Hashtbl.replace screen2func snm
				(function eqlst -> raise (Recursive_screen_error screenm));
			let app_parse = Hashtbl.find screen2act snm in
			let app_opt = Op.optimiz app_parse in
			let app_type = Op.typify app_opt in
			recreate_temp ();			(* discard changes from optimizer *)
			let f = gen app_type in
			let f2 =
				let rec first_is_sos p =
					match p with
						ScreenDef(a,b) -> first_is_sos b
					|	Actions(a,b) -> first_is_sos a
					|	AddScreen(a,b,c,d,e) -> true
					|	Sos(a) -> true
					|	Overlap -> true
					|	Pad(a,b) -> true
					|	_ -> false
				in
				if first_is_sos app_type then
					(* discard equity list if first statement is SOS-style *)
					begin
						function eqlst -> f []
					end
				else f
			in
			(* now stick the real function in *)
			Hashtbl.replace screen2func snm f2;
			restore_temp tv;			(* restore the temp table *)
			f2
		end
	else
		raise (Undefined_screen_error screenm)


(*
 * Generate a function to evaluate screen actions.  Each function takes an
 * equity list and returns a new equity list
 *)

and gen p =
	(* Sets a temp variable to a constant value *)

	let setcon a b fset =
		let fs = fset (TempVar a) in
		add_temp a;
		fs b
	in

	(* Sets a value for every equity in the list and returns the list *)

	let setall fset eqlst =
		List.iter fset eqlst;
		eqlst
	in

	(* Creates a value, dependent on each equity *)

	let do_create a b fset fgen =
		let fs = fset (TempVar a) in
		let fb = fgen b in
		add_temp a;
		let fsv = function e -> fs (fb e) e in
		function eqlst ->
			List.iter fsv eqlst;
			eqlst
	in

	(* Sets a value, independent of equity *)

	let do_set a b fset fgen =
		let fs = fset (GlobVar a) in
		let fb = fgen b in
		add_glob a;
		begin
			function eqlst ->
				fs (fb eqlst) passdata#global;
				eqlst
		end
	in

	(* Returns the equity list, unchanged *)

	let all_equities eqlst = eqlst in

	(* Returns an empty equity list *)

	let no_equities eqlst = [] in

	(* Performs a "Unique" operation, returning the equity list sans
		duplicates *)

	let uniquify fg eqlst =
		let t = Hashtbl.create (List.length eqlst) in
			(* tracks values that we've already seen *)
		let ffilt e =
			let v = fg e in		(* get the value from the equity *)
			if Hashtbl.mem t v then false
				(* already there - discard this record *)
			else
				begin
					(* new value - add it to the table and keep the record *)
					Hashtbl.add t v 0;
					true
				end
		in
		List.filter ffilt eqlst
	in

	(* compiles a screen with a limited number of outputs *)

	let compile_limit scrnm limit =
		let f = compile scrnm in
		let f2 = gen (Top(float_of_int limit,false,false)) in
		function eqlst -> f2 (f eqlst)
	in

	(* rerank equities *)

	let rerank eqlst =
		let set_int_rank = set_int_field (TempVar rank_var_nm) in
		let set_int_tied_rank = set_int_field (TempVar tied_rank_var_nm) in
		let rerank nrank r =
			set_int_rank nrank r;
			set_int_tied_rank nrank r;
			succ nrank
		in
		ignore (List.fold_left rerank 1 eqlst);
		eqlst
	in

	match p with
		Actions(a,b) ->
			let fa = gen a in
			let fb = gen b in
			begin
				function eqlst -> fb (fa eqlst)
			end

	|	Uses(a) -> all_equities		(* No-OP *)

	|	Deblank(a) ->
			let fexist v = Exists(v) in
			let exlst = List.map fexist a in
			let q = Op.Keep(Op.opify (fun a b -> Op.And(b,a)) exlst) in
			gen q

	|	Keep(Bool(True)) -> all_equities		(* everything passes *)

	|	Keep(Bool(False)) -> no_equities		(* nothing passes *)

	|	Keep(a) ->
			let fa = EqExprGen.bool_gen a in
			List.filter fa

	|	Unique(a,true) ->
			let fg = do_indir get_string_field a in
			begin
				function eqlst -> uniquify fg eqlst
			end

	|	Unique(a,b) ->
			let fg = do_indir get_string_field a in
			begin
				function eqlst ->
					List.rev (uniquify fg (List.rev eqlst))
			end

	|	Sort(a) ->
			let b = List.rev a in
			let h = List.hd b in
			let t = List.tl b in

			(* returns a sort function to compare a field in two equities *)
			let fsort s =
				match s with
					Ascending(c) ->
						let fget = do_indir get_string_field c in
						begin
							fun p q ->
								Util.compare_any compare compare
									(fget p) (fget q)
						end
				|	Descending(c) ->
						let fget = do_indir get_string_field c in
						begin
							fun p q ->
								Util.compare_any compare compare
									(fget q) (fget p)
						end
				|	_ -> raise (Internal_error "bad sort option")
			in

			(* combines a list of sort fields into a single function *)
			let adds f s =
				let fs = fsort s in
				fun p q ->
					let v = fs p q in
					if v <> 0 then v else f p q
			in
			let sorter = List.fold_left adds (fsort h) t in

			begin
				(* set the [Rank] and [Tied Rank] fields *)
				let fsr = set_int_field (TempVar rank_var_nm) in
				let fst = set_int_field (TempVar tied_rank_var_nm) in
				let setrank (r,t,p) e =
						(*
						 * r	rank of this equity
						 * t	tied rank of this equity if it matches the
						 *		previous equity
						 * p	previous equity
						 * e	current equity
						 *)
					fsr r e;			(* set the rank *)
					let t1 = if sorter p e = 0 then t else r in
					fst t1 e;			(* set the tied rank *)
					(succ r, t1, e)
				in
				function eqlst ->
					begin
						let sorted = List.stable_sort sorter eqlst in
						if sorted <> [] then
							begin
								(* first record has [Rank]=[Tied Rank]=1 *)
								let h = List.hd sorted in
								let t = List.tl sorted in
								fsr 1 h;
								fst 1 h;
								(* remaining records start from there *)
								ignore (List.fold_left setrank (2,1,h) t);
							end;
						sorted
					end
			end

	|	Top(a,b,false) ->	(* Top by Rank *)
			let rec picktop n src result =
				match src with
					[] -> List.rev result
				|	h::t ->
						if n <= 0 then List.rev result
						else picktop (n-1) t (h::result)
			in
			begin
				function eqlst ->
					(* determine how many to keep *)
					let cnt =
						if b then	(* as a percentage *)
							let n = List.length eqlst in
							(float_of_int n) *. a
						else a		(* as a plain number *)
					in
					picktop (int_of_float cnt) eqlst []
			end

	|	Top(a,b,c) ->	(* Top by Tied Rank *)
			let fg = get_int_field (TempVar tied_rank_var_nm) in
			(* include equities whose Tied Rank <= tr *)
			let rec pickties tr src result =
				match src with
					[] -> List.rev result
				|	h::t ->
						if fg h > tr then List.rev result
						else pickties tr t (h::result)
			in
			(* include the next "n" equities *)
			let rec picktop n src result =
				match src with
					[] -> List.rev result
				|	h::t ->
						if n <= 0 then
							match result with
								[] -> []
							|	h::t -> pickties (fg h) src result
						else picktop (n-1) t (h::result)
			in
			begin
				function eqlst ->
					(* determine how many to keep *)
					let cnt =
						if b then	(* as a percentage *)
							let n = List.length eqlst in
							(float_of_int n) *. a
						else a		(* as a plain number *)
					in
					picktop (int_of_float cnt) eqlst []
			end

	|	Create(a,Num(NumCon(b))) ->
			let f =
				if numok b then
					begin
						let v =
							if classify_float b = FP_normal then b else 0.0
						in
						let fs = set_float_field (TempVar a) v in
						add_temp a;
						setall fs
					end
				else
					no_equities		(* constant is invalid
										- discard all equities *)
			in
			add_temp a;
			f
				
	|	Create(a,Str(StrCon(b))) -> setall (setcon a b set_string_field)
	|	Create(a,Bool(True)) -> setall (setcon a true set_bool_field)
	|	Create(a,Bool(False)) -> setall (setcon a false set_bool_field)

	|	Create(a,Num(b)) ->
			let fb = EqExprGen.num_gen b in
			let fs = set_float_field (TempVar a) in
			add_temp a;
			let fcreate e =
				let v = fb e in
				if numok v then
					begin
						fs (if classify_float v = FP_normal then v else 0.0) e;
						true
					end
				else
					false
			in
			List.filter fcreate

	|	Create(a,Date(b)) ->
			do_create a b set_float_field EqExprGen.date_gen
	|	Create(a,Bool(b)) ->
			do_create a b set_bool_field EqExprGen.bool_gen
	|	Create(a,b) ->
			do_create a b set_string_field EqExprGen.str_gen

	|	Set(a,Num(b)) ->
			let fb = EqListExprGen.num_gen b in
			let fs = set_float_field (GlobVar a) in
			add_glob a;
			begin
				function eqlst ->
					let v = fb eqlst in
					if numok v then
						begin
							let v2 =
								if classify_float v = FP_normal then v
								else 0.0
							in
							fs v2 passdata#global;
							eqlst
						end
					else
						[]
			end

	|	Set(a,Date(b)) -> do_set a b set_float_field EqListExprGen.date_gen
	|	Set(a,Bool(b)) -> do_set a b set_bool_field EqListExprGen.bool_gen
	|	Set(a,b) -> do_set a b set_string_field EqListExprGen.str_gen

	|	AddScreen(scrnm,rbeg,rend,bscore,wantpad) ->
			register_var (var_name "Total Score");
			register_var (var_name "Screen Rank");
			register_var (var_name "Pad");
			register_var (var_name "Combo");
			register_var (var_name "Max");
			register_var rank_var_nm;
			register_var tied_rank_var_nm;
			let null_sctot = null_field (TempVar (var_name "Total Score")) in
			let set_sctot = set_string_field (TempVar (var_name "Total Score")) in
			let set_pad = set_string_field (TempVar (var_name "Pad")) in
			let get_int_sctot = get_int_field (TempVar (var_name "Total Score")) in
			let get_int_scrcnt = get_int_field (TempVar (var_name "Screen Rank")) in
			let get_int_combo = get_int_field (TempVar (var_name "Combo")) in
			let get_int_max = get_int_field (TempVar (var_name "Max")) in
			let get_int_pad = get_int_field (TempVar (var_name "Pad")) in
			let get_bool_pad = get_bool_field (TempVar (var_name "Pad")) in
			let set_int_sctot = set_int_field (TempVar (var_name "Total Score")) in
			let set_int_scrcnt = set_int_field (TempVar (var_name "Screen Rank")) in
			let set_int_combo = set_int_field (TempVar (var_name "Combo")) in
			let set_int_max = set_int_field (TempVar (var_name "Max")) in
			let set_int_pad = set_int_field (TempVar (var_name "Pad")) in
			let cusip = get_string_field cusip_var in
			let fscr = compile_limit scrnm rend in
			begin
				function eqlst ->
					let need_init =
						match eqlst with
							[] -> false
						|	h::t -> null_sctot h
					in
					let tc = (List.length eqlst) + 1 in
					(* record current list in a table *)
					let tbl = Hashtbl.create tc in
					if need_init then
						begin
							let rec addtbl r lst =
								match lst with
									[] -> ()
								|	h::t ->
										begin
											let ar = Array.make 5 0 in
											ar.(0) <- tc - r;
											ar.(1) <- 1;
											ar.(2) <- r;
											ar.(3) <- r;
											ar.(4) <- get_int_pad h;
											Hashtbl.add tbl (cusip h) (h,ar);
											addtbl (succ r) t
										end
							in
							addtbl 1 eqlst
						end
					else
						begin
							let rec addtbl r lst =
								match lst with
									[] -> ()
								|	h::t ->
										begin
											let ar = Array.make 5 0 in
											ar.(0) <- get_int_sctot h;
											ar.(1) <- get_int_scrcnt h;
											ar.(2) <- get_int_combo h;
											ar.(3) <- get_int_max h;
											ar.(4) <- get_int_pad h;
											Hashtbl.add tbl (cusip h) (h,ar);
											addtbl (succ r) t
										end
							in
							addtbl 1 eqlst
						end;
					(* clear some fields *)
					let clear_fields r =
						set_sctot "" r;
						set_pad "" r
					in
					List.iter clear_fields passdata#equities;
					(* apply the new screen *)
					let newlst = fscr passdata#equities in
					(* restore values *)
					let restore t (r,ar) =
						set_int_sctot ar.(0) r;
						set_int_scrcnt ar.(1) r;
						set_int_combo ar.(2) r;
						set_int_max ar.(3) r;
						set_int_pad ar.(4) r
					in
					Hashtbl.iter restore tbl;
					(* add new screen items *)
					let zar = Array.make 0 0 in
					let addnew (adds,nsc,nrank) nr =
						let t = cusip nr in
						if nrank < rbeg then (adds, nsc, succ nrank)
						else if Hashtbl.mem tbl t then
							begin
								let m = get_int_max nr in
								if nrank > m then set_int_max nrank nr;
								set_int_sctot ((get_int_sctot nr) + nsc) nr;
								set_int_scrcnt (succ (get_int_scrcnt nr)) nr;
								set_int_combo ((get_int_combo nr) + nrank) nr;
								(adds, pred nsc, succ nrank)
							end
						else if wantpad || (not (get_bool_pad nr)) then
							begin
								set_int_max nrank nr;
								set_int_sctot nsc nr;
								set_int_scrcnt 1 nr;
								set_int_combo nrank nr;
								Hashtbl.add tbl t (nr,zar);
								(nr::adds, pred nsc, succ nrank)
							end
						else (adds, pred nsc, succ nrank)
					in
					let (adds,_,_) = List.fold_left addnew ([],bscore,1) newlst in
					rerank (List.append eqlst (List.rev adds))
				end

	|	Sos(a) ->
			register_var (var_name "Screen Rank");
			register_var (var_name "Total Score");
			register_var rank_var_nm;
			let get_int_scrcnt = get_int_field (TempVar (var_name "Screen Rank")) in
			let min_scrcnt e = get_int_scrcnt e >= a in
			let fsort = gen
				(Sort [
					Descending (var_name "Total Score");
					Ascending rank_var_nm]
					) in
			begin
				function eqlst -> fsort (List.filter min_scrcnt eqlst)
			end

	|	Overlap ->
			register_var (var_name "Screen Rank");
			register_var (var_name "Max");
			register_var (var_name "Combo");
			register_var rank_var_nm;
			let get_int_scrcnt = get_int_field (TempVar (var_name "Screen Rank")) in
			let min_scrcnt e = get_int_scrcnt e >= 2 in
			let fsort = gen
				(Sort [
					Ascending (var_name "Max");
					Ascending (var_name "Combo");
					Ascending rank_var_nm]
					) in
			begin
				function eqlst -> fsort (List.filter min_scrcnt eqlst)
			end

	|	Pad(ndesired, scrcnts) ->
			register_var (var_name "Pad");
			register_var rank_var_nm;
			register_var tied_rank_var_nm;
			let fscreens =
				let makef (scrnm,cnt) = compile_limit scrnm cnt in
				List.map makef scrcnts
			in
			let apply_each f = f passdata#equities in
			let not_empty result = result <> [] in
			let cusip = get_string_field cusip_var in
			let set_bool_pad = set_bool_field (TempVar (var_name "Pad")) in
			begin
				function eqlst ->
					let ncurrent = List.length eqlst in
					(* build table of current tickers *)
					let tbl = Hashtbl.create (max ncurrent ndesired) in
					let addcusip e = Hashtbl.add tbl (cusip e) 0 in
					List.iter addcusip eqlst;
					(* add equities from results *)
					let rec add_results adds curtot unused results =
						if curtot >= ndesired
									|| (unused = [] && results = []) then
							rerank (List.append eqlst (List.rev adds))
						else if results = [] then
							add_results adds curtot [] (List.rev unused)
						else
							let h = List.hd results in
							let r2 = List.tl results in
							let e = List.hd h in
							let u2 =
								let t = List.tl h in
								if t = [] then unused
								else t::unused
							in
							begin
								let t = cusip e in
								if Hashtbl.mem tbl t then
									add_results adds curtot u2 r2
								else
									begin
										Hashtbl.add tbl t 0;
										set_bool_pad true e;
										let c2 = succ curtot in
										add_results (e::adds) c2 u2 r2
									end
							end
					in
					let results =
						List.filter not_empty (List.map apply_each fscreens)
					in
					add_results [] ncurrent [] results
			end

	|	_ ->
			let f = unit_gen p in
			setall f


(*
 * Generate a function that returns a unit
 *)

and unit_gen p =
	match p with

		Replace(a,Str(StrCon(vb)),Str(StrCon(vc))) ->
			begin
				let fa = do_indir get_string_field a in
				let fs = set_string_field (TempVar a) in
				add_temp a;
				begin
					function e ->
						let va = fa e in
						let vn = if va = vb then vc else va in
						fs vn e
				end
			end

	|	Replace(a,Str(StrCon(vb)),c) ->
			begin
				let fa = do_indir get_string_field a in
				let fc = EqExprGen.str_gen c in
				let fs = set_string_field (TempVar a) in
				add_temp a;
				begin
					function e ->
						let va = fa e in
						let vn = if va = vb then fc e else va in
						fs vn e
				end
			end

	|	Replace(a,b,Str(StrCon(vc))) ->
			begin
				let fa = do_indir get_string_field a in
				let fb = EqExprGen.str_gen b in
				let fs = set_string_field (TempVar a) in
				add_temp a;
				begin
					function e ->
						let va = fa e in
						let vn = if va = fb e then vc else va in
						fs vn e
				end
			end

	|	Replace(a,b,c) ->
			begin
				let fa = do_indir get_string_field a in
				let fb = EqExprGen.str_gen b in
				let fc = EqExprGen.str_gen c in
				let fs = set_string_field (TempVar a) in
				add_temp a;
				begin
					function e ->
						let va = fa e in
						let vn = if va = fb e then fc e else va in
						fs vn e
				end
			end

	|	Print(a) ->
			let b = List.rev a in
			let h = List.hd b in
			let t = List.tl b in
			(* generate a function to print a single value *)
			let fprint s =
				match s with
					Num(a) ->
						let fa = EqExprGen.num_gen a in
						begin
							function e -> print_float (fa e)
						end
				|	Date(a) ->
						let fa = EqExprGen.date_gen a in
						begin
							function e -> print_string (cstring_of_date (fa e))
						end
				|	Bool(a) ->
						let fa = EqExprGen.bool_gen a in
						begin
							function e -> print_string (cstring_of_bool (fa e))
						end
				|	_ ->
						let fa = EqExprGen.str_gen s in
						begin
							function e -> print_string (fa e)
						end
			in
			(* generate a function to print all values *)
			let addp f s =
				let fp = fprint s in
				function e ->
					begin
						fp e;
						f e
					end
			in
			let printer = List.fold_left addp (fprint h) t in
			(* append a newline *)
			begin
				function e ->
					begin
						printer e;
						print_newline ()
					end
			end

	|	_ -> raise (Internal_error "not an action")
;;


(*
 * Returns a list of screen names
 *)

let screen_names () =
	let adds s f lst =
		if s = "basic" then lst
		else s::lst
	in
	Hashtbl.fold adds screen2act [];;


(*
 * Parse a file containing screen definitions
 *)

let parse filenm =
	let fnm =
		let f2 = filenm ^ ".txt" in
		if Sys.file_exists f2 then f2
		else filenm
	in
	let inp = open_in fnm in
	let lexbuf = Lexing.from_function (Lexer.def_read inp) in
	lexbuf.Lexing.lex_curr_p <-
		{Lexing.pos_fname=fnm;
			Lexing.pos_lnum=1;
			Lexing.pos_bol=0;
			Lexing.pos_cnum=0};
	let app_parse =
		try
			Parser.main Lexer.token lexbuf
		with Parsing.Parse_error ->
			raise (Syntax_error (Lexer.pos_msg lexbuf))
	in
	close_in inp;
	(* add all screens to the screen2act table and return the name of the
		first screen definition found in the file *)
	match app_parse with
		Op.Screens(a) ->
			begin
				let addit s d =
					match d with
						Op.ScreenDef(a,b) ->
							let a2 = String.lowercase a in
							Hashtbl.replace screen2act a2 b;
							if s <> "" then s
							else if a2 <> "basic" then a2
							else ""
					|	_ -> raise (Internal_error "expecting screen def")
				in
				List.fold_left addit "" a
			end
	|	_ -> raise (Internal_error "expecting screens");;


(*
 * Parses all of the files.  Returns the first screen
 *)

let parse_all screen files =
	(* parse a single file, return the name of the first screen, if no
		screen has been identified yet *)
	let parse1 s f =
		let snm = parse f in
		if s = "" then snm else s
	in
	List.fold_left parse1 screen files
;;


(*
 * Compiles the basic screen, if any, and assigns it as a pre-filter
 *)

let set_prefilter () =
	if Hashtbl.mem screen2act "basic" then
		passdata#set_prefilter (compile "basic")
	else
		prerr_endline "** warning: no basic filter **";
;;


(*
 * Parses all of the files and compiles the desired screen and any screens
 * that it uses.  Automatically compiles the "basic" screen, if it can, and
 * arranges for it to be invoked as a pre-filter before the desired screen
 *)

let compile_all screen files =
	let snm = parse_all screen files in
	set_prefilter ();
	(* compile the desired screen *)
	compile snm
;;
