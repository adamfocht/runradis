
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

open Utilio;;


let null_chr = Char.chr 0;;


(*
 * Truncate a string at the first null character
 *)

let truncz s =
	try
		let i = String.index s null_chr in
		Str.string_before s i
	with Not_found -> s;; (* if no null char then return whole string *)


(*
 * Strip trailing spaces from a string
 *)

let strip_trail_sp s =
	(* find last non-space char or -1 if none *)
	let rec last_nonsp n =
		if n < 0 then
			(-1)
		else
			let c = String.get s n in
			if c = ' ' || c = '\r' || c = '\n' || c = '\t' then
				last_nonsp (n-1)
			else
				n
	in
	let n = last_nonsp ((String.length s) - 1) in
	Str.string_before s (n+1);;


(*
 * info about a single DBF field
 *
 *	fieldnum	field number
 *	name		field name
 *	ftype		field type
 *	len			length of the field
 *	disp		displacement to the start of the field within the record
 *	ldec		??
 *)

class field_info (fieldnum:int) (name:string) (ftype:char) (len:int)
					(disp:int) (ldec:int) =

	object

	method name = name
	method index = fieldnum
	method ftype = ftype
	method len = len
	method disp = disp
	method ldec = ldec

	(*
	 * extract the field from the record, stripping trailing spaces
	 *)

	method extract record = strip_trail_sp (String.sub record disp len)


	(*
	 * extract the field from the record, w/o stripping spaces
	 *)

	method raw_extract record = String.sub record disp len

	end;;


(*
 * Return this when info is requested for a null field name
 *)

let dummy_field = new field_info (-1) "" 'C' 0 0 0;;


(*
 * Converts a list of field info into a Hashtbl mapping the field name
 * to the field info
 *)

let build_name2field fields =
	let n2f = Hashtbl.create (Array.length fields) in
	let addf f = Hashtbl.replace n2f f#name f in
	Array.iter addf fields;
	n2f;;


(*
 * A DBF file
 *
 *	fin			channel for reading the file contents
 *	name		name of the file
 *	dbtype		??
 *	nrecords	number of records in the file
 *	recdisp		displacement to the first record
 *	reclen		length of each record
 *	field_list	list of field info
 *)

class dbf_file (fin:fd_type) (name:string) (dbtype:int) (nrecords:int)
				(recdisp:int) (reclen:int) (field_list:field_info list) =

	let fields = Array.of_list field_list in
	let n2f = build_name2field fields in

	object

	val mutable recbuf = ""

	method name = name
	method dbtype = dbtype
	method nrecords = nrecords
	method recdisp = recdisp
	method reclen = reclen
	method fields = fields
	method name2field = n2f


	(* Close the input channel *)

	method close = close_fd fin


	(*
	 * Read the next record into the internal buffer, returning true
	 * if successful, false if EOF
	 *)

	method next_record =
		try
			begin
				recbuf <- read_string fin reclen;
				true
			end
		with End_of_file -> false


	(*
	 * Get the contents of the field of the given index from the buffer
	 * that was previously loaded
	 *)

	method get_field idx =
		if idx < Array.length fields then
			let f = Array.get fields idx in
			f#extract recbuf
		else
			raise (Invalid_argument "field index")

	method get_raw_field idx =
		if idx < Array.length fields then
			let f = Array.get fields idx in
			f#raw_extract recbuf
		else
			raise (Invalid_argument "field index")


	(*
	 * Get the contents of a field, by name, from the internal buffer
	 *)

	method get_field_by_name name =
		let f =
			if name = "" then dummy_field
			else Hashtbl.find n2f name
		in
		f#extract recbuf


	(*
	 * Get the index of the field of the given name.  Returns a dummy
	 * field if the name is empty
	 *)

	method get_field_index name =
		let f =
			if name = "" then dummy_field
			else Hashtbl.find n2f name
		in
		f#index

	end;;


(*
 * Open a DBF file.  Returns a dbf_file object
 *)

let open_dbf filenm =
	let fin = fd_type filenm Read in
	(* read header info from DBF file *)
	let dbtype = read_byte fin in
	ignore (read_byte fin);
	ignore (read_byte fin);
	ignore (read_byte fin);
	let nrecords = read_int4 fin in
	let recdisp = read_int2 fin in
	let reclen = read_int2 fin in
	let fmax = (recdisp - 33) / 32 in

	(* reads and returns a list of the field info *)
	let rec read_fields lst n disp =
		if n <= fmax then
			begin
				setpos fin (n * 32);
				let nm = strip_trail_sp (truncz (read_string fin 10)) in
				if (String.length nm) == 0 then
					List.rev lst
				else
					begin
						ignore (read_char fin);
						let ftype = read_char fin in
						ignore (read_byte fin);
						ignore (read_byte fin);
						ignore (read_byte fin);
						ignore (read_byte fin);
						let len = read_byte fin in
						let ldec = read_byte fin in
						let f = new field_info n nm ftype len disp ldec in
						read_fields (f::lst) (n+1) (disp + len)
					end
			end
		else
			List.rev lst
	in
	(*
	print_string " dbtype=";
	print_int dbtype;
	print_string " nrecords=";
	print_int nrecords;
	print_string " disp=";
	print_int recdisp;
	print_string " len=";
	print_int reclen;
	print_newline ();
	*)

	let delf = new field_info 0 "_DELETED" 'C' 0 1 0 in
		(* each record has this field, which indicates whether or not the
			record has been deleted *)
	let fields = read_fields [delf] 1 1 in
	(* move to the start of the first record *)
	setpos fin recdisp;
	new dbf_file fin filenm dbtype nrecords recdisp reclen fields;;


(*
 * Open a DBF file, possibly changing the case of the first character
 *)
let open_dbf_anycase dbfname =
	if Sys.file_exists dbfname then open_dbf dbfname
	else
		begin
			let ic = succ (String.rindex dbfname '/') in
			let n = String.copy dbfname in
			let c = String.uppercase (String.sub n ic 1) in
			String.set n ic (String.get c 0);
			open_dbf n
		end;;


(*
 * Close the DBF file
 *)

let close_dbf (dbf:dbf_file) = dbf#close;;


(*
 * Load the next record from a DBF file into the internal buffer
 *)

let next_record (dbf:dbf_file) = dbf#next_record;;


(*
 * Extract a field content, by name, from the internal buffer
 *)

let get_field_by_name (dbf:dbf_file) fieldnm = dbf#get_field_by_name fieldnm;;


(*
 * Extract a field content, by index, from the internal buffer
 *)

let get_field (dbf:dbf_file) fieldidx = dbf#get_field fieldidx;;

let get_raw_field (dbf:dbf_file) fieldidx = dbf#get_raw_field fieldidx;;


(*
 * Get the index of a given named field
 *)

let get_field_index (dbf:dbf_file) fieldnm = dbf#get_field_index fieldnm;;
