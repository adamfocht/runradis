
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
open Fieldindex;;

let cusip = ifield cusip_var;;
let rank = ifield (TempVar rank_var_nm);;

(* module StringMap = Map.Make (String);; *)


let null_rec = Array.make 0 "";;	(* record with no fields *)


(*
 * Load records
 *
 *	srcdir			source directory (i.e., period)
 *	srcdb			source DB
 *	imax			max index needed within a memory record
 *	include_sym		function that returns true if a given symbol
 *					should be included
 *
 * Returns:
 *	records			array of records
 *	id2rec			map from id to record
 *)

let emptyrec = Array.make 0 "";;
let emptyrecs = Array.make 0 emptyrec;;
let emptytbl = Hashtbl.create 0;;


(*
 * Build mapping from source DB-index to destination memory-index
 *
 *	include_sym		returns true if the given symbol should be included
 *					in the mapping
 *)

let build_db2mem srcdb include_sym =
	let get_dbi = symboltbl#get_dbi in
	(*
	 * Add a symbol to the list.
	 *
	 *	sym		candidate symbol
	 *	memi	index of the field within the memory record
	 *	s2d		list of (DB-field-index, memory-field-index)
	 *			for all fields that should be included
	 *)
	let addf sym memi s2d =
		if include_sym sym then	(* we want this field *)
			let dbi = get_dbi sym in
			(dbi,memi)::s2d
		else s2d
	in
	let s2d = Hashtbl.fold addf symboltbl#get_mem_map [] in
	let cmp a b = compare (fst a) (fst b) in
	List.sort cmp s2d;;		(* sort by ascending DB index *)


let eq_db2mem = ref [];;
let ind_db2mem = ref [];;
let sec_db2mem = ref [];;
let univ_db2mem = ref [];;


(*
 * Load records from a DB
 *
 *	srcdir		source period name
 *	srcdb		DB from which data should be read
 *	db2mem		mapping from DB index to memory index
 *	idv			name of the variable containing the unique record id
 *	imax		max no. of fields needed in each record
 *
 * Returns:
 *	records		array of records, in order in which they were loaded from
 *				the DB
 *	(tkr2rec,id2rec)
 *		tkr2rec	map from tkr to record
 *		id2rec	map from record id to record
 *)

let load_records srcdir srcdb db2mem idv imax =
	if imax < 0 then (emptyrecs, emptytbl)
		(* don't need this table - just return empty data *)
	else
		begin
			(* load the records *)
			let records = srcdb#make_records srcdir imax db2mem in

			(* build a map from id to record *)
			let nr = Array.length records in
			let id2rec = Hashtbl.create nr in
			let icode = ifield idv in
			for i=0 to nr-1 do
				let r = records.(i) in
				Hashtbl.replace id2rec r.(icode) r
			done;

			(records, id2rec)
		end;;


(*
 * Loads the equity records from a particular directory
 *
 * Returns:
 *	equities		array of equity record
 *	(tkr2eq,cusip2eq)
 *		tkr2eq		map of ticker to equity record
 *		cusip2eq	map of ticker to equity record
 *)

let load_equities dbg srcdir =
	let db = si_db dbg in
	if !eq_db2mem = [] then
		begin
			let include_sym sym =
				match indir sym with
					Var(_) -> true
				|	_ -> false
			in
			eq_db2mem := build_db2mem db include_sym
		end;
	let imax = symboltbl#max_index (Var "") in
	let (records, cusip2rec) =
		load_records srcdir db !eq_db2mem cusip_var imax
	in
	(* build a map from tkr to record *)
	let nr = Array.length records in
	let tkr2rec = Hashtbl.create nr in
	let tcode = ifield tkr_var in
	for i=0 to nr-1 do
		let r = records.(i) in
		Hashtbl.replace tkr2rec r.(tcode) r
	done;
	(records, (tkr2rec,cusip2rec));;


(*
 * Loads industry data from a particular directory
 *
 * Returns a map of industry code to industry record
 *)

let load_ind dbg srcdir =
	let db = ind_db dbg in
	if !ind_db2mem = [] then
		begin
			let include_sym sym =
				match indir sym with
					IndVar(_) -> true
				|	_ -> false
			in
			ind_db2mem := build_db2mem db include_sym
		end;
	let imax = symboltbl#max_index (IndVar "") in
	snd (load_records srcdir db !ind_db2mem
			(IndVar (ind_var_name "Industry code"))
			imax);;


(*
 * Loads sector data from a particular directory
 *
 * Returns a map of sector code to sector record
 *)

let load_sec dbg srcdir =
	let db = sec_db dbg in
	if !sec_db2mem = [] then
		begin
			let include_sym sym =
				match indir sym with
					SecVar(_) -> true
				|	_ -> false
			in
			sec_db2mem := build_db2mem db include_sym
		end;
	let imax = symboltbl#max_index (SecVar "") in
	snd (load_records srcdir db !sec_db2mem
			(SecVar (sec_var_name "Industry code"))
			imax);;


(*
 * Loads universal data from a particular directory
 *
 * Returns the universal record
 *)

let load_univ dbg srcdir =
	let db = univ_db dbg in
	if !univ_db2mem = [] then
		begin
			let include_sym sym =
				match indir sym with
					UnivVar(_) -> true
				|	_ -> false
			in
			univ_db2mem := build_db2mem db include_sym
		end;
	let imax = symboltbl#max_index (UnivVar "") in
	let recs = db#make_records srcdir imax !univ_db2mem in
	recs.(0);;
