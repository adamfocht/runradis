
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
open Args;;


(*
 * These compute and print various statistics
 *)

class virtual statistic =
	object
		method virtual add_per: string -> float -> float -> unit
			(* add_per directory current-time period-return -> unit *)

		method virtual finish_year: unit

		method virtual print: float -> unit
			(* print begin-time *)
	end;;


(*
 * Computes the maximum draw down
 *)

class draw_down =
	object
		inherit statistic

		val mutable maxret = 1.0
		val mutable cumret = 1.0
		val mutable dd = 1.0

		method add_per dir tcur pr =
			cumret <- cumret *. pr;
			if cumret > maxret then maxret <- cumret;
			dd <- min dd (cumret /. maxret)

		method finish_year = ()

		method print tbeg =
			print_string "MAX DD=";
			print_ret dd;
			print_newline ()
	end;;


(*
 * Computes the GSD
 *)

class geo_std_dev =
	object(self)
		inherit statistic

		val mutable ytd = 1.0				(* YTD return *)
		val mutable processed = true
				(* whether or not ytd is included in sums *)
		val mutable sum_ln_ret = 0.0		(* sum of ln(year's return) *)
		val mutable sum_ln_ret_sq = 0.0
				(* sum of square of ln(year's return) *)
		val mutable count = 0				(* number of items in the sum *)

		method add_per dir tcur pr =
			ytd <- ytd *. pr;
			processed <- false

		method finish_year =
			if not processed then
				begin
					let lnr = log ytd in
					sum_ln_ret <- sum_ln_ret +. lnr;
					sum_ln_ret_sq <- sum_ln_ret_sq +. lnr *. lnr;
					count <- count + 1;
					ytd <- 1.0;
					processed <- true
				end

		method print tbeg =
			self#finish_year;
			if count > 1 then
				begin
					let n = float_of_int count in
					let alnr = sum_ln_ret /. n in
					let alnrsq = sum_ln_ret_sq /. n in
					let r = n /.  (n -. 1.0) in
					let sigma = sqrt ((alnrsq -. alnr *. alnr) *. r) in
					let gsd = exp sigma in
					print_string "GSD=";
					print_ret gsd;
					print_newline ()
				end
	end;;


(*
 * Computes statistics for a single period
 *)

class cum_per name tbeg acc =
	let fper_ret = per_ret () in
	let fcash_ret = cash_ret () in

	object

		val mutable tcur = 0.0				(* time of latest period *)
		val mutable current = 1.0			(* current period return *)
		val mutable ytd = 1.0				(* YTD return *)
		val mutable prevytd = 1.0			(* previous YTD return *)
		val mutable total = 1.0				(* cumulative return *)

		method name = name

		method add_per dir keys2data equities =
			tcur <- date2time dir;
			current <-
				begin
					let rlst = List.map (comp_ret keys2data) equities in
					let r =
						if equities = [] then 1.0
						else fper_ret rlst
					in
					fcash_ret r equities
						(* include cash positions *)
				end;
			let addit a = a#add_per dir tcur current in
			List.iter addit acc;
			ytd <- ytd *. current;
			prevytd <- ytd;
			total <- total *. current

		method finish_year =
			List.iter (function a -> a#finish_year) acc;
			ytd <- 1.0

		method print =
			print_ret current;
			print_string "\t";
			print_ret prevytd;
			print_string "\t";
			print_ret (annualize total (tcur -. tbeg))

		method print_acc =
			let pracc a =
				if name <> "" then
					begin
						print_string name;
						print_string " "
					end;
				a#print tbeg
			in
			List.iter pracc acc

	end;;


(*
 * Computes statistics for a single period, for multiple screens
 *)

class multi_cum_per tbeg acc =
	let fper_ret = per_ret () in
	let fcash_ret = cash_ret () in

	object

		val mutable tcur = 0.0				(* time of latest period *)
		val mutable current = 1.0			(* current period return *)
		val mutable ytd = 1.0				(* YTD return *)
		val mutable prevytd = 1.0			(* previous YTD return *)
		val mutable total = 1.0				(* cumulative return *)

		method add_per dir keys2data multi_equities =
			tcur <- date2time dir;
			current <-
				begin
					let one_screen (tcur,twt,weights) equities =
						let rlst = List.map (comp_ret keys2data) equities in
						let r =
							if equities = [] then 1.0
							else fper_ret rlst
						in
						let rc = fcash_ret r equities in
							(* include cash positions *)
						match weights with
						[] -> (tcur +. rc, twt +. 1.0, [])
						| h::t -> (tcur +. rc *. h, twt +. h, t)
					in
					let (tcur,twt,_) =
						List.fold_left
							one_screen (0.0,0.0,!blends) multi_equities
					in
					tcur /. twt
				end;
			let addit a = a#add_per dir tcur current in
			List.iter addit acc;
			ytd <- ytd *. current;
			prevytd <- ytd;
			total <- total *. current

		method finish_year =
			List.iter (function a -> a#finish_year) acc;
			ytd <- 1.0

		method print =
			print_ret current;
			print_string "\t";
			print_ret prevytd;
			print_string "\t";
			print_ret (annualize total (tcur -. tbeg))

		method print_acc =
			let pracc a = a#print tbeg in
			List.iter pracc acc

	end;;
