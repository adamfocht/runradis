
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
open Eqloader;;

let tkr = get_string_field tkr_var;;

(* date when AAII changed cusip mappings *)
let dt_cusip_cng = Util.date2time "20110729";;


(*
 * "Global" data used while processing a single period
 *)

let passdata =

	object

	val mutable tcur = 0.0	(* unix time, in days, of the current period *)


	val mutable prefilter = function eqlst -> eqlst
		(* pre-filters equities before applying other screens *)

	val mutable apply_corrections = true
		(* true if price corrections should be applied from the flat file,
			false otherwise *)

	val mutable globrec = null_rec
		(* global temp record *)

	val mutable getind = function e -> ""
		(* maps an equity to its industry *)

	val mutable ind2rec = Hashtbl.create 0
		(* maps an industry to its data record *)

	val mutable indempty = null_rec
		(* empty industry record *)


	val mutable getsec = function e -> ""
		(* maps an equity to its sector *)

	val mutable sec2rec = Hashtbl.create 0
		(* maps a sector to its data record *)

	val mutable secempty = null_rec
		(* empty sector record *)


	val mutable univrec = null_rec
		(* universal record *)


	val mutable eqlst = []
		(* period's initial equity list *)


	(*
	 * Returns the initial equity list for the current period
	 *)

	method equities = eqlst


	(*
	 * Returns the time associated with the current period
	 *)

	method curtime () = tcur


	(*
	 * Returns the global temp record
	 *)

	method global = globrec


	(*
	 * Returns the industry record associated with an equity, or an empty
	 * record if the desired record cannot be found
	 *)

	method industry e =
			let code = getind e in
			try
				Hashtbl.find ind2rec code
			with Not_found -> indempty


	(*
	 * Returns the sector record associated with an equity, or an empty
	 * record if the desired record cannot be found
	 *)

	method sector e =
			let code = getsec e in
			try
				Hashtbl.find sec2rec code
			with Not_found -> secempty


	(*
	 * Returns the universal record
	 *)

	method universal = univrec


	(*
	 * Sets the equity pre-filter
	 *)

	method set_prefilter pf = prefilter <- pf


	(*
	 * Initializes the symbol tables.  This should be called once, after
	 * compiling the code, but before calling load_dir
	 *)

	method init ac =
		begin
			apply_corrections <- ac;

			(* create empty records for industry and sector *)
			let indmax = symboltbl#max_index (IndVar "") in
			let secmax = symboltbl#max_index (SecVar "") in
			let globmax = symboltbl#max_index (GlobVar "") in

			indempty <- Array.make indmax "";
			secempty <- Array.make secmax "";
			globrec <- Array.make globmax "";

			if indmax > 0 then
				begin
					(* allocate industry fields from fieldindex *)
					ignore (ifield (Var (var_name "SI Industry")));
					ignore (ifield (IndVar
								(ind_var_name "Industry code")))
				end;

			if secmax > 0 then
				begin
					(* allocate sector fields from fieldindex *)
					ignore (ifield (Var (var_name "SI Sector")));
					ignore (ifield (SecVar
								(sec_var_name "Industry code")))
				end;

			symboltbl#stop_add
				(* prevent additional symbols from being added to
					symbol table *)
		end


	(*
	 * Loads data from the given SI PRO source directory, associating the
	 * given time with it
	 *)

	method load_dir (dbg:Siarchive.db_group) srcdir curtime =
		begin
			let tprev = tcur in

			tcur <- curtime;

			let globmax = symboltbl#max_index (GlobVar "") in
			globrec <- Array.make globmax "";

			if symboltbl#max_index (IndVar "") > 0 then
				begin
					(* the industry data is needed *)
					getind <- get_string_field (Var (var_name "SI Industry"));
					ind2rec <- load_ind dbg srcdir
				end;

			if symboltbl#max_index (SecVar "") > 0 then
				begin
					(* the sector data is needed *)
					getsec <- get_string_field (Var (var_name "SI Sector"));
					sec2rec <- load_sec dbg srcdir
				end;

			if symboltbl#max_index (UnivVar "") > 0 then
				begin
					(* the universal data is needed *)
					univrec <- load_univ dbg srcdir
				end;

			(* load the equities *)
			let (arecords, (tkr2equities,cusip2equities)) =
				load_equities dbg srcdir
			in

			if apply_corrections then
				begin
					Correct.correct_prices srcdir tkr2equities;
				end;

			eqlst <- prefilter (Array.to_list arecords);

			if tprev >= dt_cusip_cng || tcur < dt_cusip_cng then
				Hashtbl.clear tkr2equities;

			(tkr2equities,cusip2equities)
		end

	end;;
