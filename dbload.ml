
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
open Dbf;;
open Dbfreader;;

(* dbload.ml *)

(*
 * Loads a DB with SI Pro data
 *)


let main () =
	Gc.set { (Gc.get()) with Gc.space_overhead = 20 };

	let dbg = db_group true in

	(* get list of directories to load *)
	let dirs =
		if Array.length Sys.argv <= 1 then [Util.si_pro ^ "/Professional"]
							(* default *)
		else
			let no_dots d =
				String.length d < 1 || Str.last_chars d 1 <> "."
			in
			let addd lst d = d::lst in
			let lst = List.fold_left addd []
				(List.filter no_dots
					(List.tl
						(Array.to_list Sys.argv)))
			in
			List.rev lst
	in

	(* extract period name from the DBF files in the given directory *)
	let pernm dir =
		let dbf = open_dbf (dir ^ "/Setup.dbf") in
		if not (next_record dbf) then raise End_of_file;
		let ipn =
			try
				get_field_index dbf "MONTHDATE"
			with Not_found ->
				begin
					try
						get_field_index dbf "MONTH_DATE"
					with Not_found ->
						invalid_arg "no month date in Setup.dbf"
				end
		in
		let pn = get_field dbf ipn in
		close_dbf dbf;
		pn
	in

	(* table of existing periods *)
	let tper = Hashtbl.create 500 in
	List.iter (function p -> Hashtbl.replace tper p 0) (periods dbg);

	(* clean-up: remove partially-loaded periods *)
	let remove_partial pn =
		if not (Hashtbl.mem tper pn) then
			begin
				print_string "remove partial ";
				print_endline pn;
				begin
					try
						(univ_db dbg)#remove pn
					with Not_found -> ()
				end;
				begin
					try
						(sec_db dbg)#remove pn
					with Not_found -> ()
				end;
				begin
					try
						(ind_db dbg)#remove pn
					with Not_found -> ()
				end;
			end
	in
	List.iter remove_partial (ind_db dbg)#periods;

	(* load the directories *)
	let load_dir a =
		let pn = pernm a in
		if Hashtbl.mem tper pn then
			begin
				print_string "already loaded ";
				print_endline pn;
			end
		else
			begin
				print_string "loading ";
				print_endline pn;
				dbf_reader ind_filenm (ind_db dbg) a pn;
				dbf_reader sec_filenm (sec_db dbg) a pn;
				dbf_univ_reader univ_filenm (univ_db dbg) a pn;
				dbf_reader "" (si_db dbg) a pn;
				Hashtbl.replace tper pn 0
			end
	in
	List.iter load_dir dirs;

	close_db dbg;

	exit 0;;


main ();;
