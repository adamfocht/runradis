
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
open Util;;
open Args;;

(* analyze5.ml *)

(*
 * Segregates equities into separate bins and then computes the cumulative
 * return for each bin using the median stock within the bin.
 *)

exception Unknown_option of string;;


let nbins = 5;;


let main () =
	Arg.parse
		[months_spec; weeks_spec; emax_spec; screen_spec]
		add_file
		"arg(s): [-screen <name>] [-months <nn>] [-weeks <nn>] [-max <nn>] file1 file2 ...";
	if !files = [] then raise (Invalid_argument "missing file name");
	let dbg = Siarchive.db_group false in
	symboltbl#build_sym dbg;
	if !screens = [] then screens := [""];
	let action =
		let act = Compiler.compile_all (List.hd !screens) !files in
		if !emax <= 0 then act
		else
			(* wrap it with a function to select the top stocks *)
			let ftop = Compiler.gen (Op.Top(float_of_int !emax, false, false)) in
			function eqlst -> ftop (act eqlst)
	in
	passdata#init true;
	let (dirs0,relevant,eoy) = list_dirs_in_range dbg in
	let tradeable d =
		Hashtbl.mem relevant d
	in
	let dirs = List.filter tradeable dirs0 in
	let ftrade = trade action relevant in
	let totals = Array.make nbins 1.0 in
	let proc_per equities dir =	(* process a single period *)
		let tcur = date2time dir in
		let keys2data = passdata#load_dir dbg dir tcur in
		let limits = Array.make nbins 0 in
		(* determine limits (i.e., # of equities) for each bin *)
		let len = List.length equities in
		limits.(0) <- (len + nbins - 1) / nbins;
		limits.(nbins - 1) <- len;
		for i=1 to nbins-2 do
			let navail = len - limits.(i - 1) in
			let bavail = nbins - i in
			limits.(i) <- limits.(i - 1) + (navail + bavail - 1) / bavail
		done;
		(* add the next equity to the appropriate bin *)
		let rec add1 i n returns equities =
			if i >= nbins then
				()
			else if n > limits.(i) then
				(* reached the limit for this bin - compute its return *)
				begin
					let ni =
						if i > 0 then
							limits.(i) - limits.(i - 1)
						else
							limits.(i)
					in
					if ni > 0 then
						begin
							let r = Util.median returns in
							totals.(i) <- totals.(i) *. r;
							print_string "\t";
							print_ret totals.(i)
						end
					else	(* empty bin - no change *)
						begin
							print_string "\t";
							print_ret totals.(i)
						end;
					add1 (i + 1) n [] equities	(* process next bin *)
				end
			else
				begin
					let r = comp_ret keys2data (List.hd equities) in
					add1 i (n + 1) (r::returns) (List.tl equities)
				end
		in
		print_string dir;
		add1 0 1 [] equities;
		print_string "\t";
		print_int len;
		print_newline ();
		ftrade keys2data dir equities passdata#equities
			(* trade equities for next period *)
	in
	ignore (List.fold_left proc_per [] dirs);
	exit 0;;

main ();;
