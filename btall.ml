
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
open Passdata;;
open Util;;
open Args;;
open Stats;;

(* btall.ml *)

(*
 * Applies all screens, showing the the YTD returns and other statistics
 * for each screen
 *)

exception Unknown_option of string;;


let main () =
	Arg.parse
		[beg_spec; end_spec; months_spec; weeks_spec; bias_spec;
					emin_spec; emax_spec;
					ties_spec; wt_spec]
		add_file (
			"arg(s): [-min count] [-max count] [-ties] [-weight first]"
				^ "\n\t[-begin YYYYMMDD] [-end YYYYMMDD]"
				^ "\n\t[-months <nn>] [-weeks <nn>] [-bias <n.n>]"
				^ "\n\tsource-file1 source-file2 ...");
	if !files = [] then raise (Invalid_argument "missing file name");
	let dbg = Siarchive.db_group false in
	symboltbl#build_sym dbg;
	(* parse everything *)
	ignore (Compiler.parse_all "" !files);
	(* compile everything *)
	let screen2func = Hashtbl.create 100 in
	let compile screen =
		let f = Compiler.compile screen in
		Hashtbl.replace screen2func screen f
	in
	let screens = List.sort compare (Compiler.screen_names ()) in
	List.iter compile screens;
	if !emax > 0 then
		begin
			(* wrap actions with a function to select the top stocks *)
			let ftop = Compiler.gen (Op.Top(float_of_int !emax, false, false)) in
			let wrap s =
				let act = Hashtbl.find screen2func s in
				let f = function eqlst -> ftop (act eqlst) in
				Hashtbl.replace screen2func s f
			in
			List.iter wrap screens
		end;
	Compiler.set_prefilter ();
	passdata#init true;
	let (dirs0,relevant,eoy) = list_dirs_in_range dbg in
	let trade_or_eoy d =
		Hashtbl.mem relevant d || Hashtbl.mem eoy d
	in
	let dirs = List.filter trade_or_eoy dirs0 in
	let tbeg = date2time (List.hd dirs) in
	let addcum lst s =
		let cum = new cum_per s tbeg
			[new draw_down; new geo_std_dev]
		in
		let act = Hashtbl.find screen2func s in
		let ftrade = trade act relevant in
		(cum,ftrade,[])::lst
	in
	let cum = List.rev (List.fold_left addcum [] screens) in
	let proc_trade_per accumulators dir =
			(* process a single period *)
		let tcur = date2time dir in
		let keys2data = passdata#load_dir dbg dir tcur in
		let proc_screen (cum, ftrade, equities) =
			if dir <> List.hd dirs then
					cum#add_per dir keys2data equities;
			print_string dir;
			print_string "\t";
			cum#print;
			print_string "\t";
			print_int (List.length equities);
			print_string "\t";
			print_string cum#name;
			print_newline ();
			let eqlst2 = ftrade keys2data dir equities passdata#equities in
			(cum, ftrade, eqlst2)
		in
		List.map proc_screen accumulators
	in
	let proc_per accumulators dir =
        let result =
            if Hashtbl.mem relevant dir then proc_trade_per accumulators dir
            else accumulators
        in
        if Hashtbl.mem eoy dir then
					List.iter (function (cum,_,_) -> cum#finish_year) accumulators;
        result
  in
	print_endline "PERIOD\t\t   CUR\t   YTD\t  CAGR\tPASSING\tSCREEN";
	ignore (List.fold_left proc_per cum dirs);
	print_newline ();
	let prcum (cum,_,_) = cum#print_acc in
	List.iter prcum cum;
	exit 0;;

main ();;
