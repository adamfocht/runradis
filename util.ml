

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

exception MissingEnvVariables;;


(*
 * path to SI Pro install directory and RunRadis DB - use env variables,
 * if available
 *)

let si_pro =
	try
		Sys.getenv "SI_PRO"
	with Not_found ->
		begin
			let d = "C:/Program Files/Stock Investor" in
			let d2 = "C:/Program Files (x86)/Stock Investor" in
			try
				if Sys.is_directory d2 then d2
				else d
			with Sys_error(_) -> d
		end;;

let si_dbpath =
	try
		Sys.getenv "RUNRADIS"
	with Not_found ->
		begin
			try
				begin
					let dpfx = Sys.getenv "APPDATA" in
					let d = dpfx ^ "/RunRadis" in
					begin
						try
							if not (Sys.is_directory d) then
								Unix.mkdir d 0o750
						with Sys_error(_) ->
								Unix.mkdir d 0o750
					end;
					d
				end
			with Not_found ->
				raise MissingEnvVariables
		end;;


(*
 * Strip CR from the end of the line, if there is one
 *)
let chomp ln =
	let ilast = pred (String.length ln) in
	if ilast >= 0 then
		if ln.[ilast] = '\r' then Str.string_before ln ilast
		else ln
	else ln;;


(*
 * Normalize variable names so they're more likely to match.  Map them
 * to lowercase and remove dots and extra blanks.
 *)

let dot_re = Str.regexp "[.]+";;
let multi_blank_re = Str.regexp "[ ][ ]+";;

let normalize_name varnm =
	String.lowercase
		(Str.global_replace dot_re ""
			(Str.global_replace multi_blank_re " " varnm));;

let ind_name_prefix = normalize_name "SI IND ";;
let sec_name_prefix = normalize_name "SI SEC ";;

let norm_ind_name varnm = normalize_name (ind_name_prefix ^ varnm);;
let norm_sec_name varnm = normalize_name (sec_name_prefix ^ varnm);;
let norm_univ_name ctx varnm = normalize_name ("SI " ^ ctx ^ " " ^ varnm);;

let ind_filenm = "si_mgav2";;	(* name of industry file *)
let sec_filenm = "si_mgavg";;	(* name of sector file *)
let univ_filenm = "si_avg";;	(* name of universal file *)


(*
 * Returns true if a variable is an industry/sector variable name
 *)

let is_ind_var n =
	let len = 1 + (String.length ind_name_prefix) in
	if String.length n < len then false
	else normalize_name (Str.first_chars n len) = ("[" ^ ind_name_prefix);;

let is_sec_var n =
	let len = 1 + (String.length sec_name_prefix) in
	if String.length n < len then false
	else normalize_name (Str.first_chars n len) = ("[" ^ sec_name_prefix);;

let is_univ_var n =
	let is_var ctx =
		let ctx_pfx = normalize_name ("SI " ^ ctx ^ " ") in
		let len = 1 + (String.length ctx_pfx) in
		if String.length n < len then false
		else normalize_name (Str.first_chars n len) = ("[" ^ ctx_pfx)
	in
	is_var "median" || is_var "average" || is_var "high" || is_var "low"
		|| is_var "std dev" || is_var "sample";;


(*
 * Normalize a regular expression using "normal" syntax (i.e., no backslash
 * before ), (, or |
 *)
let specre = Str.regexp "\\([)(|]\\)";;

let norm_re re = Str.global_replace specre "\\\\\\1" re;;


(*
 * Convert a date string of the form YYYYMMDD to a unix time, in days
 *)

let datere = Str.regexp (norm_re "^([0-9]+)/([0-9]+)/([0-9]+)$");;

let date2time dt =
	let len = String.length dt in
	if len = 0 then 0.0
	else
		try
			let (yy,mm,dd) =
				if len = 8 && dt.[1] <> '/' && dt.[2] <> '/' then
					begin
						(* YYYYMMDD *)
						let yy = int_of_string (String.sub dt 0 4) in
						let mm = int_of_string (String.sub dt 4 2) in
						let dd = int_of_string (String.sub dt 6 2) in
						(yy,mm,dd)
					end
				else if Str.string_match datere dt 0 then
					begin
						(* Mm/Dd/YYyy *)
						let yy =
							let y = int_of_string (Str.matched_group 3 dt) in
							if y < 30 then y + 2000
							else if y < 1000 then y + 1900
							else y
						in
						let mm = int_of_string (Str.matched_group 1 dt) in
						let dd = int_of_string (Str.matched_group 2 dt) in
						(yy,mm,dd)
					end
				else
					raise (Failure "int_of_string")
			in
			if yy < 1950
					|| mm < 1 || mm > 12
					|| dd < 1 || dd > 31 then 0.0
			else
				let (f, t) =
					Unix.mktime {
						Unix.tm_sec = 0;
						Unix.tm_min = 0;
						Unix.tm_hour = 10;
						Unix.tm_mday = dd;
						Unix.tm_mon = mm - 1;
						Unix.tm_year = yy - 1900;
						Unix.tm_wday = 0;
						Unix.tm_yday = 0;
						Unix.tm_isdst = false
						} in
				f /. (24.0 *. 60.0 *. 60.0)
		with	Unix.Unix_error(_, "mktime", _) -> 0.0
			|	Failure("int_of_string") -> 0.0;;


(*
 * Compute the average of a list of numbers.
 *)

let average lst =
	if lst = [] then 0.0
	else
		let v = List.fold_left (+.) 0.0 lst in
		v /. (float_of_int (List.length lst));;

(*
 * Compute the median of a list of numbers.
 *)

let median lst =
	if lst = [] then 0.0
	else
		let len = List.length lst in
		let ordlst = List.sort compare lst in
		if len mod 2 = 1 then
			List.nth ordlst (len / 2)
		else
			let x = List.nth ordlst (len / 2) in
			let y = List.nth ordlst (len / 2 - 1) in
			(x +. y) /. 2.0;;

(*
 * String extraction
 *)

let string_left text numchars =
	if numchars < 0 then "#VALUE!"
	else
		let len = String.length text in
		if len <= numchars then text
		else Str.first_chars text numchars;;

let string_right text numchars =
	if numchars < 0 then "#VALUE!"
	else
		let len = String.length text in
		if len <= numchars then text
		else Str.last_chars text numchars;;

let string_mid text start numchars =
	if numchars < 0 then "#VALUE!"
	else
		let len = String.length text in
		if start < 1 then "#VALUE!"
		else if start >= len then ""
		else
			let nremain = len - start in
			if nremain <= numchars then
				Str.last_chars text nremain
			else
				String.sub text (pred start) numchars;;


(*
 * Compare two values.  Try to compare them as numbers and, if that fails,
 * then compare them as strings
 *)
let compare_any numcmp strcmp a b =
	if a = "" || b = "" then strcmp a b
	else
		try
			numcmp (float_of_string a) (float_of_string b)
		with Failure("float_of_string") -> strcmp a b;;


(*
 * Print a return
 *)

let print_ret r = Printf.printf "%6.1f" (r *. 100.0 -. 100.0);;


(*
 * Annualize a return accumulated across the given number of days
 *)

let annualize retn ndays =
	if ndays <= 0.0 then
		1.0
	else
		retn ** (365.25 /. ndays);;
