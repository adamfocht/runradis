
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
open Args;;

(* pick.ml *)

(*
 * Identify the stocks, in sorted order, that currently pass the
 * specified screens.
 *)

exception Unknown_option of string;;

let tkr = get_string_field tkr_var;;


let main () =
	Arg.parse
		[beg_spec; emax_spec; ties_spec; screen_spec]
		add_file (
		"arg(s): [-begin YYYYMMDD] [-max count] [-ties]"
			^ "\n\t[-screen <name1> -screen <name2> ...]"
			^ "\n\tfile1 file2 ...");
	if !files = [] then raise (Invalid_argument "missing file name");
	let dbg = Siarchive.db_group false in
	symboltbl#build_sym dbg;
	(* parse everything *)
	ignore (Compiler.parse_all "" !files);
	(* compile everything *)
	let screen2func = Hashtbl.create 100 in
	let compile screen =
		let f = Compiler.compile screen in
		Hashtbl.replace screen2func screen f
	in
	if !screens = [] then screens := [Compiler.parse_all "" !files];
	List.iter compile !screens;
	(* get function to select top stocks *)
	let ftop =
		if !emax > 0 then
			Compiler.gen (Op.Top(float_of_int !emax, false, !ties))
		else
			function eqlst -> eqlst
	in
	Compiler.set_prefilter ();
	passdata#init true;
	let dir =
		if !dbegin <> "00000000" then !dbegin
		else
			(* load the latest period *)
			let dirs = List.sort compare (Siarchive.periods dbg) in
			List.hd (List.rev dirs)
	in
	ignore (passdata#load_dir dbg dir (Util.date2time dir));
	(* apply each screen *)
	let apply snm =
		let prteq e =
			print_string " ";
			print_string (tkr e)
		in
		print_string snm;
		let fact = Hashtbl.find screen2func snm in
		List.iter prteq (ftop (fact passdata#equities));
		print_newline ()
	in
	List.iter apply !screens;
	print_newline ();
	print_endline dir;
	exit 0;;

main ();;
