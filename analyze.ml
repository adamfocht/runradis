
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

(* analyze.ml *)

(*
 * Lists the period returns for each of the 10 stocks passing a screen.
 *)

exception Unknown_option of string;;


let main () =
	Arg.parse
		[months_spec; weeks_spec; screen_spec]
		add_file
		"arg(s): [-screen <name>] [-months <nn>] [-weeks <nn>] file1 file2 ...";
	if !files = [] then raise (Invalid_argument "missing file name");
	let dbg = Siarchive.db_group false in
	symboltbl#build_sym dbg;
	if !screens = [] then screens := [""];
	let action = Compiler.compile_all (List.hd !screens) !files in
		(* function implementing screen *)
	passdata#init true;
	let (dirs0,relevant,eoy) = list_dirs_in_range dbg in
	let tradeable d =
		Hashtbl.mem relevant d
	in
	let dirs = List.filter tradeable dirs0 in
	let ftrade = trade action relevant in
	let proc_per equities dir =	(* process a single period *)
		let tcur = date2time dir in
		let keys2data = passdata#load_dir dbg dir tcur in
		(* show the returns for this period *)
		let rec show_ret n eqlst =
			if n > 0 && eqlst <> [] then
				let e = List.hd eqlst in
				let t = List.tl eqlst in
				let r = comp_ret keys2data e in
				print_string " ";
				print_ret r;
				show_ret (n-1) t
		in
		print_string dir;
		show_ret 10 equities;
		print_newline ();
		ftrade keys2data dir equities passdata#equities
	in
	ignore (List.fold_left proc_per [] dirs);
	exit 0;;

main ();;
