
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

open Siarchive;;

type symbol =
		Var			of string	(* plain variable *)
	|	TempVar		of string	(* temp variable *)
	|	GlobVar		of string	(* global temp variable *)
	|	IndVar		of string	(* industry variable *)
	|	SecVar		of string	(* sector variable *)
	|	UnivVar		of string	(* universal variable *)
;;

exception Internal_error of string;;
exception Field_not_found of string;;


(*
 * Normalize plain, industry, and sector variables
 *)

let var_name n =
	if n = "" then "" else "[" ^ (Util.normalize_name n) ^ "]";;

let ind_var_name n =
	if n = "" then "" else "[" ^ (Util.norm_ind_name n) ^ "]";;

let sec_var_name n =
	if n = "" then "" else "[" ^ (Util.norm_sec_name n) ^ "]";;

	
let tkr_var_nm = var_name "SI Ticker";;
let tkr_var = Var tkr_var_nm;;

let cusip_var_nm = var_name "SI Company Identifier";;
let cusip_var = Var cusip_var_nm;;

let rank_var_nm = var_name "Rank";;
let tied_rank_var_nm = var_name "Tied Rank";;


(*
 * Extracts the name from a symbol
 *)

let sym_name sym =
	match sym with
		Var(n) -> n
	|	IndVar(n) -> n
	|	SecVar(n) -> n
	|	UnivVar(n) -> n
	|	TempVar(n) -> n
	|	GlobVar(n) -> n;;


let symboltbl =

	(*
	 * Tracks the highest allocated index for a given record type:
	 *
	 *	0	plain & temp variables in the equity's record
	 *	1	global temp variables
	 *	2	industry variables in the industry-specific records
	 *	3	sector variables in the sector-specific records
	 *	4	universal variables in the universal record
	 *)

	let idxset = Array.make 5 0 in


	(*
	 * Maps a symbol to the array index in which the associated data can be
	 * found within the record.  If a symbol appears here, then it should
	 * also appear within sym2sym.  Only symbols that are actually referenced
	 * by the screen will be added to sym2idx
	 *)

	let sym2idx = Hashtbl.create 111 in


	(*
	 * Maps a symbol to a pair:
	 *
	 *	dbi		array index in which the associated data can be found with
	 *			the DB record
	 *	ftype	field type
	 *
	 * This is not populated until after build_sym has been called
	 *)

	let sym2db = Hashtbl.create (2500 * 4) in


	(*
	 * Maps a symbol to a sub-symbol.  This is used to deal with industry,
	 * sector, and universal (e.g., "Median") variables.  For instance:
	 *
	 *		Var("[SI IND PRICE]")
	 *
	 * maps to:
	 *
	 *		IndVar("[SI IND PRICE]")
	 *
	 * All symbols seen while scanning the data dictionaries will be loaded
	 * into this table.  However, only those that are actually referenced by
	 * the screen will be added to sym2idx
	 *)

	let sym2sym = Hashtbl.create (2500 * 4) in


	object(self)


	(*
	 * Whether or not the table is accepting new symbols
	 *)

	val mutable accept = true


	(*
	 * True if symbol table has been loaded from DB
	 *)

	val mutable loaded = false


	(*
	 * Maps a symbol to its record type
	 *)

	method sym2set sym =
			match sym with
				Var(_) -> 0
			|	TempVar(_) -> 0
			|	GlobVar(_) -> 1
			|	IndVar(_) -> 2
			|	SecVar(_) -> 3
			|	UnivVar(_) -> 4


	(*
	 * Gets the maximum index allocated to record types containing the
	 * given symbol
	 *)

	method max_index sym = Array.get idxset (self#sym2set sym)


	(*
	 * Gets the symbol-to-memory-index mapping table
	 *)

	method get_mem_map = sym2idx


	(*
	 * Populates sym2sym from the respective DBs.  Also populates sym2db
	 *)

	method build_sym (dbg:db_group) =
		let tcur = Hashtbl.copy sym2sym in

		let addsyms dbf makesym =
			let af = Array.of_list dbf#fields in
			let nf = Array.length af in
			for i=0 to nf-1 do
				let f = af.(i) in
				let fn = var_name f#name in
				let v = Var(fn) in
				let s = makesym fn in
				Hashtbl.replace sym2sym v s;
				Hashtbl.replace sym2sym s s;
				Hashtbl.remove tcur v;
				Hashtbl.remove tcur s;
				Hashtbl.replace sym2db s (i,f#ftype)
			done
		in
		addsyms dbg#ind_db (function n -> IndVar(n));
		addsyms dbg#sec_db (function n -> SecVar(n));
		addsyms dbg#univ_db (function n -> UnivVar(n));

		(* do this last so it overrides others, if necessary *)
		addsyms dbg#si_db (function n -> Var(n));

		let missing a b =
			match a with
				TempVar(x)|GlobVar(x) -> ()
			|	_ -> raise (Field_not_found (sym_name a));
		in
		Hashtbl.iter missing tcur;

		loaded <- true


	(*
	 * Prevents additional symbols from being added to the symbol table.
	 * This should be called after all code has been compiled, but before
	 * execution begins
	 *)

	method stop_add = accept <- false


	(*
	 * Add an indirect symbol to the symbol table.  Returns the symbol to
	 * which it maps (which may be itself).  Raises an exception if the
	 * symbol cannot be found in any data dictionary
	 *)

	method add_indir sym =
		if Hashtbl.mem sym2idx sym then Hashtbl.find sym2sym sym
		else if accept then
			begin
				begin
					match sym with
						TempVar(_)|GlobVar(_) ->
							Hashtbl.replace sym2sym sym sym
					|	_ -> ()
				end;
				let sym2 =
					try
						Hashtbl.find sym2sym sym
					with Not_found ->
						if loaded then
							raise (Field_not_found (sym_name sym))
						else
							begin
								let sn = sym_name sym in
								let sym2 =
									if Util.is_ind_var sn then
										IndVar(sn)
									else if Util.is_sec_var sn then
										SecVar(sn)
									else if Util.is_univ_var sn then
										UnivVar(sn)
									else
										Var(sn)
								in
								Hashtbl.replace sym2sym sym sym2;
								sym2
							end
				in
				let idx =
					try
						Hashtbl.find sym2idx sym2
					with Not_found ->
						begin
							let ss = self#sym2set sym2 in
							let idx = idxset.(ss) in
							idxset.(ss) <- succ idx;
							Hashtbl.replace sym2idx sym2 idx;
							idx
						end
				in
				Hashtbl.replace sym2idx sym idx;
				sym2
			end
		else
			raise (Internal_error ("added symbol " ^ (sym_name sym)))


	(*
	 * Adds a symbol to the symbol table.  Returns the index to which it
	 * maps.  It is assumed that the caller knows which type of record
	 * contains the given field
	 *)

	method add sym =
		let sym2 = self#add_indir sym in
		Hashtbl.find sym2idx sym2


	(*
	 * Gets the DB-index for a symbol
	 *)

	method get_dbi sym =
		let sym2 = self#add_indir sym in
		fst (Hashtbl.find sym2db sym2)


	(*
	 * Gets the DB field-type for a symbol
	 *)

	method get_db_type sym =
		let sym2 = self#add_indir sym in
		snd (Hashtbl.find sym2db sym2)

	end;;


(*
 * Get the field index of the given symbol
 *)

let ifield sym = symboltbl#add sym;;


(*
 * Get the symbol to which a symbol maps (used to determine if it maps
 * to an industry or sector variable)
 *)

let indir sym = symboltbl#add_indir sym;;


(*
 * Conversion routines that always succeed, without raising exceptions
 *)

let cfloat_of_string a =
	if a = "" then 0.0
	else
		try
			float_of_string a
		with Failure("float_of_string") ->
			0.0;;

let cfloat_of_bool a =
	if a then 1.0 else 0.0;;

let cbool_of_string a =
	a <> "" && a <> "0" && a <> "F" && a <> "FALSE" && a <> "false";;

let cbool_of_float a =
	a <> 0.0;;

let cbool_of_date = cbool_of_float;;

let cstring_of_float a =
	string_of_float a;;

let cstring_of_bool a =
	if a then "TRUE" else "FALSE";;

let cstring_of_date a =
	let t = Unix.localtime (a *. (24.0 *. 60.0 *. 60.0)) in
	let twodig a =
		if a < 10 then "0" ^ (string_of_int a)
		else string_of_int a
	in
	(string_of_int (t.Unix.tm_year + 1900))
		^ (twodig (t.Unix.tm_mon + 1))
		^ (twodig t.Unix.tm_mday);;

let cdate_of_bool = cfloat_of_bool;;

let cdate_of_string a = Util.date2time a;;

let cint_of_string a =
	if a = "" then 0
	else
		try
			int_of_string a
		with Failure("int_of_string") ->
			0;;


(*
 * Returns functions that test or extract data from a record
 *)

let null_field sym =
	let idx = ifield sym in
	function p -> p.(idx) = "";;

let exists_field sym =
	let idx = ifield sym in
	function p -> p.(idx) <> "";;


let get_string_field sym =
	let idx = ifield sym in
	function (p:string array) -> p.(idx);;

let get_bool_field sym =
	let idx = ifield sym in
	function p -> cbool_of_string p.(idx);;

let get_float_field sym =
	let idx = ifield sym in
	function p -> cfloat_of_string p.(idx);;

let get_int_field sym =
	let idx = ifield sym in
	function p -> cint_of_string p.(idx);;

let get_date_field sym =
	let idx = ifield sym in
	function p -> Util.date2time p.(idx);;


(*
 * Returns functions that set data within a record
 *)

let set_float_field sym =
	let idx = ifield sym in
	fun v p ->
		p.(idx) <- cstring_of_float v;;

let set_int_field sym =
	let idx = ifield sym in
	fun v p ->
		p.(idx) <- string_of_int v;;

let set_string_field sym =
	let idx = ifield sym in
	fun v p ->
		p.(idx) <- v;;

let set_bool_field sym =
	let idx = ifield sym in
	fun v p ->
		p.(idx) <- cstring_of_bool v;;

let set_date_field sym =
	let idx = ifield sym in
	fun v p ->
		p.(idx) <- cstring_of_date v;;
