
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

open Util;;
open Siarchive;;

(* dbinit.ml *)

(*
 * Creates and initializes a DB for historical AAII data
 *)

let main () =
	if Sys.file_exists (si_dbpath^"/"^si_prefix^"."^db_ext)
		&& Sys.file_exists (si_dbpath^"/"^ind_prefix^"."^db_ext)
		&& Sys.file_exists (si_dbpath^"/"^sec_prefix^"."^db_ext) 
		&& Sys.file_exists (si_dbpath^"/"^univ_prefix^"."^db_ext) then
		begin
			prerr_endline
				(si_dbpath^"/"^si_prefix ^ "." ^ db_ext ^ " already exists");
			exit 1
		end;
	db_init si_prefix;
	db_init ind_prefix;
	db_init sec_prefix;
	db_init univ_prefix;
	exit 0;;

main ();;
