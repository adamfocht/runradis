
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
open Util;;

exception Corrupt_db_file;;

(*
 * DB file name prefixes and extensions
 *)
let si_prefix = "si_archive";;
let ind_prefix = "si_industry";;
let sec_prefix = "si_sector";;
let univ_prefix = "si_universal";;

let db_ext = "dat";;
let dd_ext = "dd";;
let idx_ext = "idx";;


(*
 * Start/end markers
 *)
let sov = 'V';;
let som = 'M';;
let sor = 'R';;
let eom = 'm';;

let version = 1;


(*
 * BNF of various DB files:
 
 	dd_file:		SOV version SOM field_def*

	period_file:	SOM period*

	data_file:		SOM data
		Next available block begins after the last period

	field_def:	SOR field_name field_type EOM

	field_name:	length1 text

	field_type:
				'N'		numeric
			|	'D'		date
			|	'B'		boolean
			|	'S'		string
			|	'M'		mixed
		This is used by the compiler; all data is still stored in the DB
		file as text strings

	period:		SOR period_name end_offset record_count field_count field_offset EOM
		All of the fields for a period are stored contiguously within a block ENDING
		at end_offset

	period_name:	YYYYMMDD	text
		If the first character is ' ', then the entry has been deleted

	end_offset:	length8
		Offset of field data within data_file

	version:		length4

	record_count:	length4

	field_count:	length4

	field_offset:	length8
		Offset of field headers within data_file

	field_hdr:		SOR data_type field_data_offset compress_hdr EOM
		One field_hdr for each field (field_count headers)

	data_type:
				'C'		single character field values
			|	'M'		multi-character field values

	field_data_offset:	offset8
		Offset of field data within data_file

	field_data:	field_data_item*
		One item for each record (record_count items)

	field_data_item:
				' '					if data_type='C', empty string
			|	single-character	if data_type='C'
			|	length1 text		if data_type='M'


	compress_hdr:	uncompressed_length compressed_length
		compressed_length = uncompressed_length indicates that the data is
			not compressed

	uncompressed_length:	length4
		0 indicates no data

	compressed_length:	length4


	length1 = 1-byte length
	length4 = 4-byte length, big-endian
	length8 = 8-byte length, big-endian
	offset8 = 8-byte offset within the file, big-endian

 *)


type field_type =
		Num
	|	Date
	|	Bool
	|	String
	|	Mixed;;


(*
 * Verify that the next input char is as expected, raise an exception
 * of the given type otherwise
 *)
let expect_or_corrupt fd v =
	if read_char fd <> v then raise Corrupt_db_file;;

let expect_or_eof fd v =
	if read_char fd <> v then raise End_of_file;;


(*
 * Reads a list of items.  Invokes "fnew" to create the item that is used
 * to read the data.  Returns a list of the items, in the order in which
 * they were created/read
 *)

let read_item_list fd fnew =
	let rec read_rec lst =
		let f = fnew () in
		let p = getpos fd in
		let r =
			try
				f#read fd;
				true
			with End_of_file ->
				begin
					setpos fd p;
					false
				end
		in
		if r then read_rec (f::lst)
		else List.rev lst
	in
	read_rec [];;


(*
 * Populate a field in all of the records.  The field is encoded as a single
 * character for each record.  If the character is a space, then the field is
 * an empty string, otherwise it is a string of length one, containing the
 * character
 *)

let pop_single_char fd records destidx =
	let nr = Array.length records in
	try
		for i=0 to nr-1 do
			let r = records.(i) in
			r.(destidx) <- read_char_str fd
		done
	with Invalid_argument(_)  -> raise Corrupt_db_file;;

(*
 * Populate a field in all of the records.  The field for each record is
 * encoded as a single character, which is the field length, followed by the
 * text of the field
 *)

let pop_multi_char fd records destidx =
	let nr = Array.length records in
	try
		for i=0 to nr-1 do
			let r = records.(i) in
			r.(destidx) <- read_text_len1 fd
		done
	with Invalid_argument(_) -> raise Corrupt_db_file;;


(*
 * Manages a compressed-data field.  The length of the compressed and
 * uncompressed data is encoded together in the IDX file, while the data
 * itself is stored elsewhere (i.e., in the DB file)
 *)
let b2 = ref "";;

class compressed_data =
	object
		val mutable cmprlen = 0
		val mutable unclen = 0
		val mutable data = ""

		method len_compressed = cmprlen
		method len_uncompressed = unclen

		(*
		 * Read compressed data header
		 *)
		method read (fd:fd_type) =
			cmprlen <- read_int4 fd;
			unclen <- read_int4 fd

		(*
		 * Write compressed data header
		 *)
		method write (fd:fd_type) =
			write_int4 fd cmprlen;
			write_int4 fd unclen

		(*
		 * Read the compressed data, itself
		 *)
		method read_data (dbfd:fd_type) =
			data <-
				if cmprlen > 0 then read_string dbfd cmprlen
				else ""

		(*
		 * Write the compressed data, itself
		 *)
		method write_data (dbfd:fd_type) =
			if String.length data <> cmprlen then
				invalid_arg "wrong compressed data"
			else if cmprlen > 0 then
				write_string dbfd data

		(*
		 * Get the data, uncompressing it if necessary
		 *)
		method get_data =
			if String.length data <> cmprlen then
					invalid_arg "wrong compressed data"
			else if unclen = cmprlen then data
			else
				begin
					if unclen > String.length !b2 then
						b2 := String.create (unclen+1000);
					let len = Camlbz2.bz2_uncompress !b2 data in
					if len = unclen then !b2
					else raise Corrupt_db_file
				end

		(*
		 * Set the data, compressing it if possible
		 *)
		method set_data d =
			unclen <- String.length d;
			data <-
				if d = "" then ""
				else
					begin
						try
							let cdata = String.create (unclen + 1000) in
							let clen = Camlbz2.bz2_compress cdata d in
							if clen > int_of_float
									(0.85 *. (float_of_int unclen)) then d
										(* didn't compress enough *)
							else
								(* verify uncompress *)
								let s = String.create unclen in
								let slen = Camlbz2.bz2_uncompress s cdata in
								if slen = unclen && s = d then
									begin
										(*
										print_string "compress ";
										print_int unclen;
										print_string " to ";
										print_int clen;
										print_newline ();
										*)
										Str.first_chars cdata clen
									end
								else
									begin
										(*
										print_endline "compress diff";
										*)
										d
									end
						with Failure(s) -> d
							(* compress/uncompress failed -> leave the
								data uncompressed *)
					end;

			cmprlen <- String.length data
	end;;


class field_def =
	object
		val mutable nm = ""
		val mutable ft = Mixed
		val mutable toffset = 0

		method name = nm
		method ftype = ft

		(*
		 * read the field definition
		 *)
		method read (fd:fd_type) =
			expect_or_eof fd sor;
			nm <- read_text_len1 fd;
			toffset <- getpos fd;
			ft <-
				begin
					match read_char fd with
						'N' -> Num
					|	'D' -> Date
					|	'B' -> Bool
					|	'S' -> String
					|	'M' -> Mixed
					|	_ -> raise Corrupt_db_file
				end;
			expect_or_eof fd eom

		(*
		 * write the field definition
		 *)
		method write (fd:fd_type) n t =
			nm <- n;
			ft <- t;
			write_char fd sor;
			toffset <- getpos fd;
			write_text_len1 fd nm;
			let ct =
				match ft with
					Num -> 'N'
				|	Date -> 'D'
				|	Bool -> 'B'
				|	String -> 'S'
				|	Mixed -> 'M'
			in
			write_char fd ct;
			write_char fd eom

		(*
		 * change the field's type to Mixed
		 *)
		method set_mixed (fd:fd_type) =
			if ft <> Mixed then
				begin
					let p = getpos fd in
					setpos fd toffset;
					write_char fd 'M';
					ft <- Mixed;
					setpos fd p
				end
	end;;


class dd_file prefix writeable =
	let fd = fd_type (prefix^"."^dd_ext) writeable in

	object
		val mutable defs = []
		val mutable nm2def = Hashtbl.create 2500

		method fields = defs

		method close = close_fd fd

		(*
		 * read the field definitions from the DD file
		 *)
		method read =
			expect_or_corrupt fd sov;
			let v = read_int4 fd in
			if v <> version then raise Corrupt_db_file;
			expect_or_corrupt fd som;
			defs <- read_item_list fd (function () -> new field_def);
			let addtbl d =
				Hashtbl.replace nm2def d#name d
			in
			List.iter addtbl defs

		(*
		 * add a field to the definitions, and write it to the DD file
		 *)
		method add_field nm ft =
			try
				let f = Hashtbl.find nm2def nm in
				if f#ftype <> ft then f#set_mixed fd

			with Not_found ->
				begin
					let f = new field_def in
					f#write fd nm ft;
					defs <- defs @ [f];
					Hashtbl.replace nm2def nm f
				end
	end;;


class field_hdr =
	object
		val mutable ft = 'M'
		val mutable offset = Int64.zero
		val datahdr = new compressed_data

		(*
		 * read a field data header
		 *)
		method read fd =
			expect_or_corrupt fd sor;
			ft <- read_char fd;
			offset <- read_int64 fd;
			datahdr#read fd;
			expect_or_corrupt fd eom

		(*
		 * write a field data header
		 *)
		method write fd =
			write_char fd sor;
			write_char fd ft;
			write_int64 fd offset;
			datahdr#write fd;
			write_char fd eom

		(*
		 * read the field from the DB file and populate the records with
		 * its contents.  Assumes that all records are pre-populated with an
		 * empty string for the field
		 *)
		method pop_records dbfd records destidx =
			if datahdr#len_uncompressed <> 0 && (Array.length records) > 0 then
				begin
					setpos64 dbfd offset;
					datahdr#read_data dbfd;
					let rdr = string_in_buffer datahdr#get_data in
					match ft with
						'C' -> pop_single_char rdr records destidx
					|	'M' -> pop_multi_char rdr records destidx
					|	_ -> raise Corrupt_db_file
				end

		(*
		 * Extract the field from the records and write it to the DB file
		 *)
		method write_field dbfd records srcidx =
			offset <- getpos64 dbfd;
			let nr = Array.length records in
			let data =
				(* Array version of List.find *)
				let rec find fmatch i =
					if i >= nr then false
					else if fmatch records.(i) then true
					else find fmatch (succ i)
				in
				let multi r = String.length r.(srcidx) > 1 in
				let single r = String.length r.(srcidx) = 1 in
				if find multi 0 then	(* encode as multi-char strings *)
					begin
						ft <- 'M';

						let b = string_out_buffer (nr * 2) in
						for i=0 to nr-1 do
							let r = records.(i) in
							write_text_len1 b r.(srcidx)
						done;
						b#contents
					end

				else if find single 0 then	(* encode as single-char strings *)
					begin
						ft <- 'C';

						let b = string_out_buffer nr in
						for i=0 to nr-1 do
							let r = records.(i) in
							write_char_str b r.(srcidx)
						done;
						b#contents
					end

				else
					begin		(* all empty - type is irrelevant *)
						(*
						print_int srcidx;
						print_endline " empty";
						*)
						ft <- 'C';
						""
					end
			in
			datahdr#set_data data;
			datahdr#write_data dbfd
	end;;


class period =
	object (self)
		val mutable nm = ""
		val mutable nmoff = 0
		val mutable endoff = Int64.zero
		val mutable nrecords = 0
		val mutable nfields = 0
		val mutable fldoff = Int64.zero

		method name = nm
		method end_offset = endoff

		(*
		 * NOTE:	Does *NOT* leave dbfd at the next free block
		 *)
		method fields (dbfd:fd_type) =
			setpos64 dbfd fldoff;
			let nf = nfields in
			let fields = Array.make nf (new field_hdr) in
			for i=0 to nf-1 do
				let f = new field_hdr in
				f#read dbfd;
				fields.(i) <- f
			done;
			fields

		(*
		 * read the period header from the IDX file
		 *)
		method read (fd:fd_type) =
			expect_or_eof fd sor;
			nmoff <- getpos fd;
			nm <- read_string fd 8;
			endoff <- read_int64 fd;
			nrecords <- read_int4 fd;
			nfields <- read_int4 fd;
			fldoff <- read_int64 fd;
			expect_or_eof fd eom

		(*
		 * write the period header to the IDX file
		 *)
		method write (fd:fd_type) =
			write_char fd sor;
			nmoff <- getpos fd;
			write_string fd nm;
			write_int64 fd endoff;
			write_int4 fd nrecords;
			write_int4 fd nfields;
			write_int64 fd fldoff;
			write_char fd eom

		(*
		 * Update the IDX file to indicate that the period has been removed
		 *)
		method remove (fd:fd_type) =
			let p = getpos fd in
			setpos fd nmoff;
			write_char fd ' ';
			setpos fd p

		(*
		 * Creates an array of records, loading all relevant fields from
		 * the DB file
		 *
		 *	maxfields	number of fields with which each record should be
		 *				created.  Those not populated from the DB are
		 *				populated with empty strings.  This *MUST* be
		 *				at least as large as the largest destination
		 *				index in src2dest
		 *
		 *	src2dest	list mapping source field index in the DB record
		 *				to destination field index in the memory record.
		 *				This should be sorted by src index to maximize
		 *				loading speed
		 *
		 * NOTE:	Does *NOT* leave dbfd at the next free block
		 *)
		method make_records (dbfd:fd_type) maxfields src2dest =
			let nr = nrecords in
			let records = Array.make nr (Array.make 0 "") in
			for i=0 to nr-1 do
				records.(i) <- Array.make maxfields ""
			done;
			let flds = self#fields dbfd in
			let nf = Array.length flds in
			let pf (si,di) =
				if si < nf then
					begin
						let f = flds.(si) in
						f#pop_records dbfd records di
					end
			in
			List.iter pf src2dest;
			records

		(*
		 * Initializes a new period, writing the field data to the DB file.
		 * Does *NOT* write the period header data to the IDX file; that
		 * must be done by the caller
		 *
		 * ASSUMES: Each record has fields in 1-1 correspondence with field_def
		 *
		 *			records must not be empty
		 *)
		method init n (dbfd:fd_type) records =
			let nmpat = Str.regexp "^20[0-9][0-9][0-9][0-9][0-9][0-9]$" in
			if not (Str.string_match nmpat n 0) then invalid_arg "period name";

			nm <- n;
			nrecords <- Array.length records;
			let nf = Array.length records.(0) in
			let hdrs = Array.make nf (new field_hdr) in

			(* write data for each field *)
			for i=0 to nf-1 do
				let h = new field_hdr in
				hdrs.(i) <- h;
				print_int i;
				print_string "\r";
				flush stdout;
				h#write_field dbfd records i
			done;
			print_newline ();

			(* write hdr for each field *)
			nfields <- nf;
			fldoff <- getpos64 dbfd;
			Array.iter (function h -> h#write dbfd) hdrs;

			endoff <- getpos64 dbfd
	end;;


class period_file prefix writeable =
	let fd = fd_type (prefix^"."^idx_ext) writeable in

	object
		val mutable pers = []
		val mutable nm2per = Hashtbl.create 500

		method fd = fd
		method periods = pers

		method close = close_fd fd

		method read =
			expect_or_corrupt fd som;
			let lst = read_item_list fd (function () -> new period) in
			pers <- List.filter (function p -> String.get p#name 0 <> ' ') lst;
			List.iter (function p -> Hashtbl.replace nm2per p#name p) pers

		(*
		 * NOTE: This does *NOT* leave dbfd pointing at the next available
		 *			block
		 *)
		method make_records nm dbfd maxfields src2dest =
			let p = Hashtbl.find nm2per nm in
			p#make_records dbfd maxfields src2dest

		method remove nm =
			let p = Hashtbl.find nm2per nm in
			Hashtbl.remove nm2per nm;
			pers <- List.filter (function p -> p#name <> nm) pers;
			p#remove fd

		method add_period nm records dbfd =
			if Hashtbl.mem nm2per nm then invalid_arg "duplicate period";
			let p = new period in
			p#init nm dbfd records;
			Hashtbl.replace nm2per nm p;
			pers <- pers @ [p];
			p
	end;;


class data_file prefix writeable =
	let fd = fd_type (prefix^"."^db_ext) writeable in

	object (self)

		method fd = fd

		method close = close_fd fd

		method read =
			expect_or_corrupt fd som
	end;;


class db_file prefix writeable =
	let mode = if writeable then Write else Read in
	let pfx = si_dbpath^"/"^prefix in
	let ddf = new dd_file pfx mode in
	let idxf = new period_file pfx mode in
	let dbf = new data_file pfx mode in
	let _ =
		ddf#read;
		idxf#read;
		dbf#read;

		if not writeable then
			begin
				ddf#close;
				idxf#close
			end
	in

	object
		method close =
			if writeable then
				begin
					ddf#close;
					idxf#close;
				end;

			dbf#close

		method fields = ddf#fields
		method periods = List.map (function p -> p#name) idxf#periods

		method make_records nm maxfields src2dest =
			idxf#make_records nm dbf#fd maxfields src2dest

		method remove nm = idxf#remove nm

		method add_period nm records =
			(* position DB file at next free spot *)
			let free =
				let omax m p = max m p#end_offset in
				List.fold_left omax Int64.one idxf#periods
			in
			setpos64 dbf#fd free;
			(* low the records *)
			if Array.length records = 0 then invalid_arg "no records";
			let r = records.(0) in
			let nf = Array.length r in
			if nf <> List.length ddf#fields then
					invalid_arg "field count mismatch";
			let p = idxf#add_period nm records dbf#fd in
			p#write idxf#fd
		
		method add_field nm ft = ddf#add_field nm ft
	end;;


class db_group writeable =
	let sidb = new db_file si_prefix writeable in
	let inddb = new db_file ind_prefix writeable in
	let secdb = new db_file sec_prefix writeable in
	let univdb = new db_file univ_prefix writeable in

	object
		method si_db = sidb
		method ind_db = inddb
		method sec_db = secdb
		method univ_db = univdb

		method close =
			sidb#close;
			inddb#close;
			secdb#close;
			univdb#close
	end;;


let db_group writeable = new db_group writeable;;
let si_db dbg = dbg#si_db;;
let ind_db dbg = dbg#ind_db;;
let sec_db dbg = dbg#sec_db;;
let univ_db dbg = dbg#univ_db;;
let periods dbg = dbg#si_db#periods;;
let close_db dbg = dbg#close;;


(*
 * Create and initialize a DB file
 *)
let db_init prefix =
	let pfx = si_dbpath^"/"^prefix in

	let dd = fd_type (pfx^"."^dd_ext) Create in
	write_char dd sov;
	write_int4 dd version;
	write_char dd som;
	dd#close;

	let idx = fd_type (pfx^"."^idx_ext) Create in
	write_char idx som;
	idx#close;

	let db = fd_type (pfx^"."^db_ext) Create in
	write_char db som;
	db#close;;
