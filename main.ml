
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

(* bt.ml *)

(*
 * Applies a screen, showing the return for the period, the YTD return,
 * the CAGR, and the number of equities passing the screen.
 *)

exception Unknown_option of string;;


let main () =
	Arg.parse
		[beg_spec; end_spec; months_spec; weeks_spec; bias_spec;
					emin_spec; emax_spec;
					ties_spec; wt_spec; screen_spec; blend_spec]
		add_file (
			"arg(s): [-min count] [-max count] [-ties] [-weight first]"
				^ "\n\t[-begin YYYYMMDD] [-end YYYYMMDD]"
				^ "\n\t[-months <nn>] [-weeks <nn>] [-bias <n.n>]"
				^ "\n\t[-screen <name1> -screen <name2> ...]"
				^ "\n\t[-blend <weight1> -blend <weight2> ...]"
				^ "\n\tsource-file1 source-file2 ...");
	if !files = [] then raise (Invalid_argument "missing file name");
	let dbg = Siarchive.db_group false in
	symboltbl#build_sym dbg;
	(* functions to implement the screens *)
	if !screens = [] then screens := [Compiler.parse_all "" !files];
	let add_act lst screenm =
		let action =
			let act = Compiler.compile_all screenm !files in
			if !emax <= 0 then act
			else
				(* wrap it with a function to select the top stocks *)
				let ftop = Compiler.gen (Op.Top(float_of_int !emax, false, false)) in
				function eqlst -> ftop (act eqlst)
		in
		(action,[])::lst
	in
	let act_eq0 = List.rev (List.fold_left add_act [] !screens) in
	passdata#init true;
	let (dirs0,relevant,eoy) = list_dirs_in_range dbg in
	let trade_or_eoy d =
		Hashtbl.mem relevant d || Hashtbl.mem eoy d
	in
	let dirs = List.filter trade_or_eoy dirs0 in
	let tbeg = date2time (List.hd dirs) in
	let cum = new multi_cum_per tbeg
			[new draw_down; new geo_std_dev]
	in
	(* process a trading period *)
	let proc_trade_per act_eq dir =
			(* process a single period *)
		let (_, multi_equities) = List.split act_eq in
		let tcur = date2time dir in
		let keys2data = passdata#load_dir dbg dir tcur in
		if dir <> List.hd dirs then
				cum#add_per dir keys2data multi_equities;
		print_string dir;
		print_string "\t";
		cum#print;
		print_string "\t";
		let count_len acc equities = acc + (List.length equities) in
		print_int (List.fold_left count_len 0 multi_equities);
		print_newline ();
		let ftrade (action,equities) =
			let new_eq =
				trade action relevant keys2data dir equities passdata#equities
			in
			(action,new_eq)
		in
		List.map ftrade act_eq
	in
	let proc_per act_eq dir =
		let result =
			if Hashtbl.mem relevant dir then proc_trade_per act_eq dir
			else act_eq
		in
		if Hashtbl.mem eoy dir then cum#finish_year;
		result
	in
	print_endline "PERIOD\t\t   CUR\t   YTD\t  CAGR\tPASSING";
	ignore (List.fold_left proc_per act_eq0 dirs);
	print_newline ();
	cum#print_acc;
	exit 0;;

main ();;
