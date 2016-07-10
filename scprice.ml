
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

(* scprice.ml *)

(*
 * Compares prices in a flat file to those in the DB.  The flat file
 * should have lines of the following form, sorted by date:
 *
 *	YYYYMMDD,TKR,CLOSE
 *)

exception Unknown_option of string;;

let price = get_float_field (Var (var_name "SI Price"));;
let exchange = get_string_field (Var (var_name "SI Exchange"));;


let main () =
	let otc = ref false in
	let otc_spec =
		("-otc", Arg.Set otc, "  include OTC stocks")
	in
	let tol = ref 0.05 in
	let tol_spec =
		("-tolerance", Arg.Set_float tol, "  tolerance, DEFAULT=0.05 => +/-5%")
	in
	let penny = ref 0.15 in
	let penny_spec =
		("-penny", Arg.Set_float penny, "  penny-stock tolerance, DEFAULT=0.15 => +/-15%")
	in
	Arg.parse
		[beg_spec; end_spec; tol_spec; penny_spec; otc_spec]
		add_file
		"arg(s): [-begin YYYYMMDD] [-end YYYYMMDD] [-tolerance n.n] [-penny n.n] [-otc] price-file";
	if !files = [] || (List.tl !files) <> [] then
		raise (Invalid_argument "missing file name");
	let fin = open_in (List.hd !files) in
	let dbg = Siarchive.db_group false in
	symboltbl#build_sym dbg;
	passdata#init false;
	let dirs =
		let dfilt d =
			d >= !dbegin && d <= !dend
		in
		List.sort compare (List.filter dfilt (Siarchive.periods dbg))
	in
	let recomma = Str.regexp "," in
	let out_of_range actual expected =
		let t =
			if actual <= 1.0 then 1.0 +. !penny
			else 1.0 +. !tol
		in
		actual < expected /. t || actual > expected *. t
	in
	let proc_per fdata0 dir =
			(* process a single period *)
		print_endline dir;
		let tcur = date2time dir in
		let (tkr2data,cusip2data) = passdata#load_dir dbg dir tcur in
		let rec proc_lines fdata0 =
			let rec next_line () =
				try
					let ln = chomp (input_line fin) in
					match (Str.split recomma ln) with
						[dt;tkr;cls] ->
							(dt, tkr, float_of_string cls)
					|	_ -> next_line ()
				with Failure "float_of_string" -> next_line ()
					| End_of_file -> ("99999999","",0.0)
			in
			let fdata =
				let (fdt,ftkr,fcls) = fdata0 in
				if fdt >= dir then fdata0
				else next_line ()
			in
			let (fdt, ftkr, fcls) = fdata in
			if fdt > dir then fdata
			else
				begin
					if Hashtbl.mem tkr2data ftkr then
						begin
							let r = Hashtbl.find tkr2data ftkr in
							let actual = price r in
							if actual > 0.0
									&& (!otc || (exchange r) <> "O") then
								begin
									if out_of_range actual fcls then
										begin
											print_string ftkr;
											print_string " ";
											print_float actual;
											print_string " ";
											print_float fcls;
											print_newline ()
										end
								end
						end;
					proc_lines (next_line ())
				end
		in
		proc_lines fdata0
	in
	ignore (List.fold_left proc_per ("","",0.0) dirs);
	print_newline ();
	exit 0;;

main ();;
