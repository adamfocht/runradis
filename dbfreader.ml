
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


let ind_filenm = "si_mgav2";;	(* name of industry file *)
let sec_filenm = "si_mgavg";;	(* name of sector file *)
let univ_filenm = "si_avg";;	(* name of universal file *)


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


(*
 * Normalize data, discarding various characters
 *)
let badchr = Str.regexp "[ @]+";;

let normalize_data d = Str.global_replace badchr "" d;;


let indre = Str.regexp_case_fold "^ind[.] ";;
let secre = Str.regexp_case_fold "^sec[.] ";;

let strip_ind fn = Str.global_replace indre "" fn;;
let strip_sec fn = Str.global_replace secre "" fn;;


let plain_base filenm =
	let lfn = String.lowercase filenm in
	let bn = Filename.basename lfn in
	Filename.chop_extension bn;;

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
 * Loads a DB with SI Pro data
 *
 *	C - character (numeric or text)
 *	D - date
 *	L - logical (T/F)
 *	M - memo (text)
 *)


let mapfs filenm shortnm = filenm ^ " " ^ shortnm;;


(*
 * Copy all data from a given SI PRO directory to the new DB
 *)

let dbf_reader fnm ndb dir pernm =
	let myfn = String.lowercase fnm in

	let fs2long = Hashtbl.create 2500 in
			(* "file-name short-field-name" => long-name *)

	let long2recidx = Hashtbl.create 2500 in
			(* long-field-name => dest-index *)


	(*
	 * Read the data dictionary
	 *)

	let read_dd dir =
		Hashtbl.clear fs2long;
		let dbf = open_dbf (dir ^ "/Datadict/datadict.dbf") in
		let ifile = get_field_index dbf "FILE" in
		let ifield = get_field_index dbf "FIELD_NAME" in
		let idesc = get_field_index dbf "FIELD_DESC" in
		let ftype = get_field_index dbf "FIELD_TYPE" in
		let sitype = get_field_index dbf "SI_TYPE" in
		let curt = Hashtbl.create 2500 in
		List.iter (function f -> Hashtbl.replace curt f#name 0) ndb#fields;

		while next_record dbf do
			let filenm = String.lowercase (get_field dbf ifile) in
			if filenm = myfn || not (Hashtbl.mem skip_files filenm) then
				begin
					let sn = get_field dbf ifield in
					if sn.[0] <> '_' then
						begin
							let nsn = Util.normalize_name sn in
							let fs = mapfs filenm nsn in
							let nln =
								let d = get_field dbf idesc in
								if myfn = ind_filenm then
									Util.norm_ind_name (strip_ind d)
								else if myfn = sec_filenm then
									Util.norm_sec_name (strip_sec d)
								else Util.normalize_name ("SI " ^ d)
							in
							Hashtbl.replace fs2long fs nln;
							if not (Hashtbl.mem curt nln) then
									print_endline ("add field " ^ nln);
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
							Hashtbl.replace curt nln 0;
							ndb#add_field nln t
						end
				end
		done;

		close_dbf dbf
	in

	(* read data dictionary *)
	read_dd dir;

	let idnm =
		if myfn = ind_filenm then "INDCODE"
		else if myfn = sec_filenm then "INDCODE"
		else "COMPANY_ID"
	in

	(* build map from long field name to record *)
	let af = Array.of_list ndb#fields in
	let naf = Array.length af in
	for i=0 to naf-1 do
		let f = af.(i) in
		Hashtbl.replace long2recidx f#name i
	done;

	let itkr =
		if myfn = ind_filenm || myfn = sec_filenm then (-1)
		else Hashtbl.find long2recidx (Util.normalize_name "SI Ticker")
	in

	let nfields = Hashtbl.length long2recidx in

	(*
	 * Get list of dbf files in the given directory
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
				let fullnm = dirnm ^ "/" ^ filenm in
				fullnm::lst

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
				let sn = Util.normalize_name f#name in
				let fs = mapfs filenm sn in
				let ln = Hashtbl.find fs2long fs in
				idx.(i) <- Hashtbl.find long2recidx ln;
				Hashtbl.remove long2recidx ln;
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
										let r = Array.make nfields "" in
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
				print_string "loading ";
				print_endline dbfname;
				let newrecs =
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

	(* add records to the DB *)
	ndb#add_period pernm (Array.of_list rec2);;


(*
 * Copy all universal data from a given SI PRO directory to the new DB
 *)

let dbf_univ_reader fnm ndb dir pernm =

	let long2recidx = Hashtbl.create 2500 in
			(* long-field-name => dest-index *)

	(*
	 * Create a map from short field name to long by reading the data dictionary
	 *)

	let short2desc =

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
		t
	in


	(*
	 * Read the data dictionary
	 *)

	let () =
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
							ndb#add_field (Util.norm_univ_name ctx d) Num
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

		close_dbf dbf
	in

	(* build map from long field name to record *)
	let af = Array.of_list ndb#fields in
	let naf = Array.length af in
	for i=0 to naf-1 do
		let f = af.(i) in
		Hashtbl.replace long2recidx f#name i
	done;


	(*
	 * Read all records from the DBF file, populating the universal record
	 *)
	let dbfname = dir ^ "/Dbfs/" ^ fnm ^ ".dbf" in
	print_string "loading ";
	print_endline dbfname;

	let recdata = Array.make naf "" in

	let dbf = open_dbf_anycase dbfname in
	let ifield = get_field_index dbf "FIELD_NAME" in

	let copyrec ctx =
		try
			let ictx = ctx2idx dbf ctx in
			begin
				function sn ->
					try
						let desc = Hashtbl.find short2desc sn in
						let nln = Util.norm_univ_name ctx desc in
						let di = Hashtbl.find long2recidx nln in
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

	(* add records to the DB *)
	ndb#add_period pernm (Array.make 1 recdata);;
