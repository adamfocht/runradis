
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

(* dbdel.ml *)

(*
 * Deletes the specified periods from a DB
 *)

let main () =
	let tdel = Hashtbl.create 20 in
	Array.iter (function p -> Hashtbl.replace tdel p 0) Sys.argv;
	let ndb = new db_file si_prefix true in
	let dblst =
		[
			ndb;
			new db_file ind_prefix true;
			new db_file sec_prefix true;
			new db_file univ_prefix true;
		]
	in
	let tper = Hashtbl.create 200 in
	let addp p = Hashtbl.replace tper p 0 in
	List.iter (function d -> List.iter addp d#periods) dblst;
	let fdel p =
		if Hashtbl.mem tdel p then
			begin
				print_endline p;
				let fdel2 d =
					try
						d#remove p
					with Not_found -> ()
				in
				List.iter fdel2 dblst
			end
	in
	Hashtbl.iter (fun a b -> fdel a) tper;
	exit 0;;


main ();;
