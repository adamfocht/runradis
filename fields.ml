
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

(* fields.ml *)

(*
 * Lists the fields in the DB.  If any argument is specified, then it
 * lists the industry, sector, and universal fields, too.
 *)

let main () =
	let dbg = Siarchive.db_group false in
	let showfield def = print_endline def#name in
	List.iter showfield (si_db dbg)#fields;
	if Array.length Sys.argv > 1 then
		begin
			List.iter showfield (ind_db dbg)#fields;
			List.iter showfield (sec_db dbg)#fields;
			List.iter showfield (univ_db dbg)#fields
		end;
	exit 0;;

main ();;
