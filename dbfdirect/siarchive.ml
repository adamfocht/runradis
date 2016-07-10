
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

(*
 * This version of siarchive loads SI Pro data directly from the DBF files
 *)

open Utilio;;
open Util;;
open Dbf;;

(*
 * Loads SI Pro data
 *
 *	C - character (numeric or text)
 *	D - date
 *	L - logical (T/F)
 *	M - memo (text)
 *)


exception Corrupt_db_file;;


(*
 * Field cache file name
 *)
let field_cache_nm = si_dbpath ^ "/si_def.dat";;


(*
 * Start/end markers
 *)
let som = 'M';;
let sof = 'F';;
let sop = 'P';;
let eom = 'm';;

let version = 1;;


(*
 * Normalize DBF data, discarding various characters
 *)

let normalize_data orig =
	if orig = "" then orig
	else
		begin
			let len = String.length orig in
			let target = String.create len in
			let rec copyc si di =
				if si >= len then di
				else
					let c = String.get orig si in
					if c = ' ' || c = '@' then copyc (succ si) di
					else
						begin
							String.set target di c;
							copyc (succ si) (succ di)
						end
			in
			let tlen = copyc 0 0 in
			String.sub target 0 tlen
		end;;


let indre = Str.regexp_case_fold "^ind[.] ";;
let secre = Str.regexp_case_fold "^sec[.] ";;

let strip_ind fn = Str.global_replace indre "" fn;;
let strip_sec fn = Str.global_replace secre "" fn;;


let plain_base filenm =
	let lfn = String.lowercase filenm in
	let bn = Filename.basename lfn in
	Filename.chop_extension bn;;


(*
 * Convert a description to a long variable name
 *)
let desc2long myfn d =
	if myfn = ind_filenm then
		norm_ind_name (strip_ind d)
	else if myfn = sec_filenm then
		norm_sec_name (strip_sec d)
	else normalize_name ("SI " ^ d);;


(*
 * Strips spaces from a string
 *)
let space_re = Str.regexp " ";;

let strip_sp v = Str.global_replace space_re "" v;;


(*
 * Maps a universal file context to a record index
 *)
let ctx2idx dbf ctx =
	let c = strip_sp ctx in
	get_field_index dbf c;;


(*
 * DBF files to skip
 *)

let skip_files = Hashtbl.create 10
  let _ =
    List.iter
		(function f ->
			Hashtbl.add skip_files (String.lowercase f) 0)
      [ ind_filenm;
		sec_filenm;
		univ_filenm;
		"setup";
		"si_udf";
		"si_utyp" ];;


let mapfs filenm shortnm = filenm ^ " " ^ shortnm;;


(*
 * BNF of field cache:

	cache_file:		version plain_def industry_def sector_def universal_def

	version:		int2

	plain_def:		data_def

	industry_def:	data_def

	sector_def:		data_def

	universal_def:	data_def

	data_def:		SOM field_def_count field_def*
						period_count period* EOM

	field_def_count:	int2
		Number of field_defs

	field_def:	SOF field_name field_type

	field_name:	length1 text

	field_type:
				'N'		numeric
			|	'D'		date
			|	'B'		boolean
			|	'S'		string
			|	'M'		mixed

	period_count:
		Number of periods

	period:		SOP period_name period_dir

	period_name:	YYYYMMDD	text
		If the first character is ' ', then the entry has been deleted

	period_dir:		length1 text
		Name of directory containing the period


	length1 = 1-byte length
	int2 = 2-byte number, big-endian

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


(*
 * Reads an array of items.  Invokes "fnew" to create the item that is used
 * to read the data.  Returns an array of the items, in the order in which
 * they were created/read
 *)

let read_item_array fd nitems fnew =
	let ar = Array.make nitems (fnew ()) in
	for i=0 to nitems-1 do
		let f = fnew () in
		f#read fd;
		ar.(i) <- f
	done;
	ar;;

(*
 * Writes an array of items
 *)
let write_item_array fd nitems ar =
	for i=0 to nitems-1 do
		let f = ar.(i) in
		f#write fd
	done;;


(*
 * Grow an array by a given amount
 *)
let grow arr nadd vinit = Array.append arr (Array.make nadd vinit);;


class field_def =
	object
		val mutable nm = ""
		val mutable ft = Mixed

		method name = nm
		method ftype = ft

		(*
		 * Initialize the field definition
		 *)
		method init n t =
			nm <- n;
			ft <- t

		(*
		 * read the field definition
		 *)
		method read (fd:fd_type) =
			expect_or_corrupt fd sof;
			nm <- read_text_len1 fd;
			ft <-
				begin
					match read_char fd with
						'N' -> Num
					|	'D' -> Date
					|	'B' -> Bool
					|	'S' -> String
					|	'M' -> Mixed
					|	_ -> raise Corrupt_db_file
				end

		(*
		 * write the field definition
		 *)
		method write (fd:fd_type) =
			write_char fd sof;
			write_text_len1 fd nm;
			let ct =
				match ft with
					Num -> 'N'
				|	Date -> 'D'
				|	Bool -> 'B'
				|	String -> 'S'
				|	Mixed -> 'M'
			in
			write_char fd ct

		(*
		 * change the field's type to Mixed
		 *)
		method set_mixed =
			ft <- Mixed
	end;;


class field_defs =
	object
		val mutable ndefs = 0	(* number of defs *)

		val nm2fi = Hashtbl.create 2500
					(* maps field-name to (field,index) *)

		val mutable adefs = Array.make 0 (new field_def)

		method nfields = ndefs
		method afields = adefs

		method fields =
			let lst = ref [] in
			let n = ndefs in
			for i=n-1 downto 0 do
				let f = adefs.(i) in
				lst := f::!lst
			done;
			!lst

		(*
		 * read the field definitions
		 *)
		method read (fd:fd_type) =
			let n = read_int2 fd in
			ndefs <- n;
			adefs <- read_item_array fd n (function () -> new field_def);
			for i=0 to n-1 do
				let f = adefs.(i) in
				Hashtbl.replace nm2fi f#name (f,i)
			done

		(*
		 * write the field definitions
		 *)
		method write (fd:fd_type) =
			write_int2 fd ndefs;
			write_item_array fd ndefs adefs

		(*
		 * add a field to the definitions
		 *)
		method add_field nm ft =
			try
				let (f,i) = Hashtbl.find nm2fi nm in
				if f#ftype <> ft then f#set_mixed

			with Not_found ->
				begin
					print_endline ("add field " ^ nm);
					let f = new field_def in
					f#init nm ft;
					let i = ndefs in
					if i >= Array.length adefs then
						begin
							adefs <- grow adefs 200 (new field_def)
						end;
					adefs.(i) <- f;
					ndefs <- succ i;
					Hashtbl.replace nm2fi nm (f,i)
				end
	end;;


class period =
	object
		val mutable nm = ""
		val mutable dir = ""

		method name = nm
		method directory = dir

		method init n d =
			if String.length d > 255 then
				invalid_arg "length of directory name";
			nm <- n;
			dir <- d

		(* Read period info *)
		method read (fd:fd_type) =
			expect_or_corrupt fd sop;
			nm <- read_text_len1 fd;
			dir <- read_text_len1 fd

		(* Write period info *)
		method write (fd:fd_type) =
			write_char fd sop;
			write_text_len1 fd nm;
			write_text_len1 fd dir
	end;;


class period_defs =
	object
		val tperiods = Hashtbl.create 500

		method contains nm = Hashtbl.mem tperiods nm

		method periods =
			let addit nm p lst = nm::lst in
			Hashtbl.fold addit tperiods []

		method period nm = Hashtbl.find tperiods nm

		(*
		 * read the periods
		 *)
		method read (fd:fd_type) =
			let n = read_int2 fd in
			let ar = read_item_array fd n (function () -> new period) in
			for i=0 to n-1 do
				let p = ar.(i) in
				if Sys.is_directory p#directory then
					Hashtbl.replace tperiods p#name p
				else
					raise Corrupt_db_file
			done

		(*
		 * write the periods
		 *)
		method write (fd:fd_type) =
			let n = Hashtbl.length tperiods in
			write_int2 fd n;
			let wrt nm p = p#write fd in
			Hashtbl.iter wrt tperiods

		(*
		 * add a period
		 *)
		method add_period nm dir =
			print_endline ("add period " ^ nm);
			let p = new period in
			p#init nm dir;
			Hashtbl.replace tperiods nm p
	end;;


(*
 * Build data dictionary by reading all field info from a given SI PRO
 * directory.
 *)

let build_dd fdefs fnm dir =
	let myfn = String.lowercase fnm in

	let dbf = open_dbf (dir ^ "/Datadict/datadict.dbf") in
	let ifile = get_field_index dbf "FILE" in
	let ifield = get_field_index dbf "FIELD_NAME" in
	let idesc = get_field_index dbf "FIELD_DESC" in
	let ftype = get_field_index dbf "FIELD_TYPE" in
	let sitype = get_field_index dbf "SI_TYPE" in

	while next_record dbf do
		let filenm = String.lowercase (get_field dbf ifile) in
		if filenm = myfn || not (Hashtbl.mem skip_files filenm) then
			begin
				let sn = get_field dbf ifield in
				if sn.[0] <> '_' then
					begin
						let nln =
							let d = get_field dbf idesc in
							desc2long myfn d
						in
						let t =
							let t1 = get_field dbf ftype in
							let t2 = get_field dbf sitype in
							match (t1,t2) with
								("D",_)|(_,"D") -> Date
							|	("N",_)|(_,"N") -> Num
							|	("L",_)|(_,"L") -> Bool
							|	("M",_)|(_,"M") -> String
							|	_ -> Mixed
						in
						fdefs#add_field nln t
					end
			end
	done;

	close_dbf dbf;;


(*
 * Construct records from DBF files within a particular directory
 *)

let dbf_reader fnm fdefs src2dest maxfields dir pernm =
	let dbfields = fdefs#afields in
	let nfields = fdefs#nfields in

	let myfn = String.lowercase fnm in

	let tnames = Hashtbl.create 20 in
			(* hash of relevant table names *)

	let fs2long = Hashtbl.create nfields in
			(* "file-name short-field-name" => long-name *)

	let long2dest = Hashtbl.create nfields in
			(* long-field-name => dest-index *)

	(* build map from long field name to record *)
	let addsrc (si,di) =
		let f = dbfields.(si) in
		Hashtbl.replace long2dest f#name di
	in
	List.iter addsrc src2dest;


	(*
	 * Read the data dictionary to build fs2long & tnames
	 *)

	let () =
		let dbf = open_dbf (dir ^ "/Datadict/datadict.dbf") in
		let ifile = get_field_index dbf "FILE" in
		let ifield = get_field_index dbf "FIELD_NAME" in
		let idesc = get_field_index dbf "FIELD_DESC" in

		while next_record dbf do
			let filenm = String.lowercase (get_field dbf ifile) in
			if filenm = myfn || not (Hashtbl.mem skip_files filenm) then
				begin
					let sn = get_field dbf ifield in
					if sn.[0] <> '_' then
						begin
							let nsn = normalize_name sn in
							let fs = mapfs filenm nsn in
							let nln =
								let d = get_field dbf idesc in
								desc2long myfn d
							in
							if Hashtbl.mem long2dest nln then
								begin
									Hashtbl.replace fs2long fs nln;
									Hashtbl.replace tnames filenm 0
								end
						end
				end
		done;

		close_dbf dbf
	in

	let idnm =
		if myfn = ind_filenm then "INDCODE"
		else if myfn = sec_filenm then "INDCODE"
		else "COMPANY_ID"
	in

	let itkr =
		if myfn = ind_filenm || myfn = sec_filenm then (-1)
		else Hashtbl.find long2dest (normalize_name "SI Ticker")
	in

	(*
	 * Get list of relevant dbf files in the given directory
	 *)
	let list_dir dirnm lst0 =
		let add_file lst filenm =
			let lfn = String.lowercase filenm in
			let b = plain_base filenm in

			if b <> myfn && Hashtbl.mem skip_files b then
				lst

			else if b <> myfn && myfn <> "" then
				lst

			else if String.length lfn <= 4 then
				lst

			else if Str.last_chars lfn 4 = ".dbf" then
				let b = plain_base filenm in
				if Hashtbl.mem tnames b then
					let fullnm = dirnm ^ "/" ^ filenm in
					fullnm::lst
				else
					lst

			else
				lst
		in
		let names = Sys.readdir dirnm in
		Array.fold_left add_file lst0 names
	in

	let dirs =
		let d1 = list_dir (dir ^ "/Static") [] in
		let d2 = list_dir (dir ^ "/Dbfs") d1 in
		d2
	in


	(*
	 * Read all records from a DBF file.  Adds records to id2rec.
	 * Returns a list of records that weren't yet in the id2rec, in
	 * reverse order from which they were read
	 *)

	let read_recs id2rec dbf ididx filenm =

		(* map DBF fields to appropriate record index *)
		let fields = dbf#fields in
		let tf = Array.length fields in
		let idx = Array.make tf (-1) in
		let found_one = ref false in
		for i=0 to tf-1 do
			try
				let f = fields.(i) in
				let sn = normalize_name f#name in
				let fs = mapfs filenm sn in
				let ln = Hashtbl.find fs2long fs in
				idx.(i) <- Hashtbl.find long2dest ln;
				Hashtbl.remove long2dest ln;
				found_one := true
			with Not_found -> ()
		done;

		(* read the records *)
		let rec read_rec lst =
			if next_record dbf then
				(* check "DEL" field to see if record has been deleted *)
				let delf = get_field dbf 0 in
				if delf = "" then	(* record is still active *)
					let id = get_field dbf ididx in
					if id <> "" then
						begin
							let (lst2,r) =
								try
									(lst, Hashtbl.find id2rec id)
								with Not_found ->
									begin
										let r = Array.make maxfields "" in
										Hashtbl.replace id2rec id r;
										(r::lst, r)
									end
							in
							(* copy the fields *)
							for i=0 to tf-1 do
								let j = idx.(i) in
								if j >= 0 then
									r.(j) <-
										normalize_data (get_raw_field dbf i)
							done;
							read_rec lst2
						end
					else
						read_rec lst
				else
					read_rec lst
			else lst
		in

		if !found_one then read_rec []
		else []
	in

	(* read records from each DBF file *)
	let id2rec = Hashtbl.create 10000 in
	let rdf lst dbfname =
		let dbf = open_dbf_anycase dbfname in
		let lst2 =
			try
				let ididx = get_field_index dbf idnm in
				let newrecs =
					(* print_endline ("reading " ^ dbfname); *)
					let b = plain_base dbfname in
					read_recs id2rec dbf ididx b
				in
				List.rev_append newrecs lst
			with Not_found -> lst
		in
		dbf#close;
		lst2
	in
	let records = List.fold_left rdf [] dirs in

	(* remove records with no ticker *)
	let rec2 =
		if itkr < 0 then records
		else List.filter (function r -> r.(itkr) <> "") records
	in

	Array.of_list rec2;;


(*
 * Create a map from short field name to long by reading the data dictionary
 *)

let build_short2desc dir =

	let dbf = open_dbf (dir ^ "/Datadict/datadict.dbf") in
	let ifile = get_field_index dbf "FILE" in
	let ifield = get_field_index dbf "FIELD_NAME" in
	let idesc = get_field_index dbf "FIELD_DESC" in

	let t = Hashtbl.create 2500 in

	while next_record dbf do
		let filenm = String.lowercase (get_field dbf ifile) in
		if not (Hashtbl.mem skip_files filenm) then
			begin
				let sn = get_field dbf ifield in
				if sn.[0] <> '_' then
					begin
						let d = get_field dbf idesc in
						Hashtbl.replace t sn d
					end
			end
	done;

	close_dbf dbf;
	t;;


(*
 * Build data dictionary for universal/average table, by reading all field
 * info from a given SI PRO directory
 *)

let build_univ_dd fdefs fnm dir =

	let short2desc = build_short2desc dir in

	let dbf = open_dbf_anycase (dir ^ "/Dbfs/" ^ fnm ^ ".dbf") in
	let ifield = get_field_index dbf "FIELD_NAME" in

	(*
	 * Returns a function that takes a short field name and adds its
	 * field info for a particular context
	 *)
	let savefld ctx =
		try
			ignore (ctx2idx dbf ctx);
			begin
				function sn ->
					try
						let d = Hashtbl.find short2desc sn in
						fdefs#add_field (norm_univ_name ctx d) Num
					with Not_found -> ()
			end
		with Not_found ->
			begin
				function sn -> ()
			end
	in

	let smed = savefld "MEDIAN" in
	let savg = savefld "AVERAGE" in
	let sdev = savefld "STD DEV" in
	let shigh = savefld "HIGH" in
	let slow = savefld "LOW" in
	let ssample = savefld "SAMPLE" in


	while next_record dbf do
		let delf = get_field dbf 0 in
		if delf = "" then	(* record is still active *)
			let sn = get_field dbf ifield in
			if sn.[0] <> '_' then
				begin
					smed sn;
					savg sn;
					sdev sn;
					shigh sn;
					slow sn;
					ssample sn;
				end
	done;

	close_dbf dbf;;


(*
 * Construct a record from the universal file within a particular directory
 *)

let dbf_univ_reader fdefs src2dest maxfields dir pernm =
	let dbfields = fdefs#afields in
	let nfields = fdefs#nfields in

	let long2dest = Hashtbl.create nfields in
			(* long-field-name => dest-index *)

	(* build map from long field name to record *)
	let addsrc (si,di) =
		let f = dbfields.(si) in
		Hashtbl.replace long2dest f#name di
	in
	List.iter addsrc src2dest;

	let short2desc = build_short2desc dir in


	(*
	 * Read all records from the DBF file, populating the universal record
	 *)

	let recdata = Array.make maxfields "" in

	let dbf = open_dbf_anycase (dir ^ "/Dbfs/" ^ univ_filenm ^ ".dbf") in
	let ifield = get_field_index dbf "FIELD_NAME" in

	let copyrec ctx =
		try
			let ictx = ctx2idx dbf ctx in
			begin
				function sn ->
					try
						let desc = Hashtbl.find short2desc sn in
						let nln = norm_univ_name ctx desc in
						let di = Hashtbl.find long2dest nln in
						let v = normalize_data (get_raw_field dbf ictx) in
						recdata.(di) <- v
					with Not_found -> ()
			end
		with Not_found ->
			begin
				function sn -> ()
			end
	in
	let cmed = copyrec "MEDIAN" in
	let cavg = copyrec "AVERAGE" in
	let cdev = copyrec "STD DEV" in
	let chigh = copyrec "HIGH" in
	let clow = copyrec "LOW" in
	let csample = copyrec "SAMPLE" in

	(* read the records *)
	while next_record dbf do
		(* check "DEL" field to see if record has been deleted *)
		let delf = get_field dbf 0 in
		if delf = "" then	(* record is still active *)
			begin
				let fn = get_field dbf ifield in
				cmed fn;
				cavg fn;
				cdev fn;
				chigh fn;
				clow fn;
				csample fn;
			end
	done;
	dbf#close;
	recdata;;


class data_def_base =
	object
		val fdefs = new field_defs
		val pdefs = new period_defs

		method fields = fdefs#fields
		method periods = pdefs#periods

		(*
		 * Reads the data definition
		 *)
		method read (fd:fd_type) =
			expect_or_corrupt fd som;
			fdefs#read fd;
			pdefs#read fd;
			expect_or_corrupt fd eom

		(*
		 * Writes the data definition
		 *)
		method write (fd:fd_type) =
			write_char fd som;
			fdefs#write fd;
			pdefs#write fd;
			write_char fd eom
	end;;


class data_def fnm =
	object
		inherit data_def_base

		(*
		 * Adds a new period to the dictionary
		 *)
		method add_period fnm dir pernm =
			if not (pdefs#contains pernm) then
				begin
					build_dd fdefs fnm dir;
					pdefs#add_period pernm dir
				end

		(*
		 * Creates an array of records, loading all relevant fields from
		 * the DBF files
		 *
		 *	maxfields	number of fields with which each record should be
		 *				created.  Those not populated from the DB are
		 *				populated with empty strings.  This *MUST* be
		 *				at least as large as the largest destination
		 *				index in src2dest
		 *
		 *	src2dest	list mapping source field index in the field
		 *				definitions to destination field index in the
		 *				memory record.  This should be sorted by src
		 *				index to maximize loading speed
		 *)
		method make_records pernm maxfields src2dest =
			let p = pdefs#period pernm in
			dbf_reader fnm fdefs src2dest maxfields p#directory pernm
	end;;


class data_def_univ fnm =
	object
		inherit data_def_base

		(*
		 * Adds a new period to the dictionary
		 *)
		method add_period fnm dir pernm =
			if not (pdefs#contains pernm) then
				begin
					build_univ_dd fdefs fnm dir;
					pdefs#add_period pernm dir
				end

		(*
		 * Creates a record, loading all relevant fields from the DBF
		 * universal file
		 *
		 *	maxfields	number of fields with which each record should be
		 *				created.  Those not populated from the DB are
		 *				populated with empty strings.  This *MUST* be
		 *				at least as large as the largest destination
		 *				index in src2dest
		 *
		 *	src2dest	list mapping source field index in the field
		 *				definitions to destination field index in the
		 *				memory record.  This should be sorted by src
		 *				index to maximize loading speed
		 *)
		method make_records pernm maxfields src2dest =
			let p = pdefs#period pernm in
			let r =
				dbf_univ_reader fdefs src2dest maxfields p#directory pernm
			in
			Array.make 1 r
	end;;


class db_group (writeable:bool) =
	let (sidb,inddb,secdb,univdb) =
		try
			let fd = fd_type field_cache_nm Read in
			let v = read_int2 fd in
			if v <> version then raise Corrupt_db_file;
			let sidb = new data_def "" in
			let inddb = new data_def ind_filenm in
			let secdb = new data_def sec_filenm in
			let univdb = new data_def_univ univ_filenm in
			sidb#read fd;
			inddb#read fd;
			secdb#read fd;
			univdb#read fd;
			close_fd fd;
			(sidb,inddb,secdb,univdb)
		with Unix.Unix_error(_,_,_) | Corrupt_db_file ->
			begin
				print_endline "building cache of field names";
				(new data_def "",
					new data_def ind_filenm,
					new data_def sec_filenm,
					new data_def_univ univ_filenm)
			end
	in

	(* add all directories to the field cache *)
	let () =
		let mainre = Str.regexp "^[0-9][0-9][0-9][0-9]$" in
		let subre = Str.regexp ".*[0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9]$" in
		let thave = Hashtbl.create 500 in
		List.iter (function p -> Hashtbl.replace thave p 0) sidb#periods;

		(* add a period from a sub-directory *)
		let addsub pfx added ds =
			if Str.string_match subre ds 0 then
				let pn = Str.last_chars ds 8 in
				if Hashtbl.mem thave pn then added
				else
					begin
						let dn = pfx ^ "/" ^ ds in
						sidb#add_period "" dn pn;
						inddb#add_period ind_filenm dn pn;
						secdb#add_period sec_filenm dn pn;
						univdb#add_period univ_filenm dn pn;
						Hashtbl.replace thave pn 0;
						true
					end
			else
				added
		in

		(* add periods under a year's directory *)
		let addyear added dm =
			let dm2 = si_pro ^ "/" ^ dm in
			if Str.string_match mainre dm 0
					&& Sys.is_directory dm2 then
				begin
					Array.fold_left (addsub dm2) added (Sys.readdir dm2)
				end
			else
				added
		in

		(* add periods under the top-level directory *)
		let added = Array.fold_left addyear false (Sys.readdir si_pro) in

		(* create the cache file if any periods have been added *)
		if added then
			begin
				let fd = fd_type field_cache_nm Create in
				write_int2 fd version;
				sidb#write fd;
				inddb#write fd;
				secdb#write fd;
				univdb#write fd;
				close_fd fd
			end
	in

	object
		method si_db = sidb
		method ind_db = inddb
		method sec_db = secdb
		method univ_db = univdb

		method close = ()
	end;;


let db_group writeable = new db_group writeable;;
let si_db dbg = dbg#si_db;;
let ind_db dbg = dbg#ind_db;;
let sec_db dbg = dbg#sec_db;;
let univ_db dbg = dbg#univ_db;;
let periods dbg = dbg#si_db#periods;;
let close_db dbg = dbg#close;;
