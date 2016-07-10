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
open Util;;


(*
 * Applies corrections to the equity data based on the contents in other
 * flat files
 *)
 
let set_price = set_string_field (Var (var_name "SI Price"));;


(*
 * Reads lines from a flat file that are sorted by date.  Fields within
 * a line must be comma-separated and the first field must be of the
 * form YYYYMMDD
 *)

class date_reader filenm =
	let rdline =
		try
			let fin = open_in (si_dbpath ^ "/" ^ filenm) in
			function () -> chomp (input_line fin)

		with Sys_error(_) ->
			begin
				function () -> raise End_of_file
			end
	in

	let recomma = Str.regexp "," in

	let end_data = ["99999999";"";"";"";"";""] in

	object (self)

		(* fields in last line that was read *)
		val mutable fdata = ["";"";"";"";"";""]

		(* get data from current line *)
		method cur_line = fdata

		(* get data from next line *)
		method next_line =
			try
				let ln = rdline () in
				fdata <- Str.split recomma ln

			with End_of_file ->
				fdata <- end_data

		(* skip to the first line containing data for the given date *)
		method skip_to date =
			while (List.hd fdata) < date do
				self#next_line
			done
	end;;


let pr_rdr = new date_reader "si_prices.txt";;

(*
 * Corrects prices in the ticker data based on what is in the price file
 *)

let correct_prices date tkr2data =
	pr_rdr#skip_to date;
	while (List.hd pr_rdr#cur_line) = date do
		begin
			match pr_rdr#cur_line with
				[dt;tkr;pr] ->
					begin
						try
							let r = Hashtbl.find tkr2data tkr in
							(*
							print_string tkr;
							print_string " ";
							print_endline pr;
							*)
							set_price pr r

						with Not_found -> ()
					end
			|	_ -> ()
		end;

		pr_rdr#next_line
	done;;
