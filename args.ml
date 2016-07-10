
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

(* functions to extract relevant data from an equity *)
let tkr = get_string_field tkr_var;;
let cusip = get_string_field cusip_var;;
let price = get_float_field (Var (var_name "SI Price"));;
let null_price = null_field (Var (var_name "SI Price"));;
let prdate = get_date_field (Var (var_name "SI Price Date"));;
let fsplit = get_float_field (Var (var_name "SI Split Factor"));;
let null_fsplit = null_field (Var (var_name "SI Split Factor"));;
let dsplit = get_date_field (Var (var_name "SI Split Date"));;
let divq1 = get_float_field (Var (var_name "SI Dividend Q1"));;
let dateq1 = get_date_field (Var (var_name "SI Ending date Q1"));;


(*
 * Command-line argument processing
 *)


(*
 * Name of the screens to apply; defaults to the first screen parsed
 *)
let screens = ref [];;

(*
 * Blend weights, in 1-1 correspondence with screens.  There may be
 * fewer weights than screens, in which case, then blend weight defaults
 * to 1
 *)
let blends = ref [];;

(*
 * Files to be parsed
 *)
let files = ref [];;

(*
 * Starting date, in YYYYMMDD format, inclusive
 *)
let dbegin = ref "00000000";;

(*
 * Ending date, in YYYYMMDD format
 *)
let dend = ref "99999999";;

(*
 * Number of months per holding period
 *)
let months = ref 1;;


(*
 * Number of weeks per holding period, or zero if monthly.
 * This interacts with the months parameter as follows:
 *
 *	-months <m>
 *		Simulates trading stocks at the end of every <m> months
 *
 *	-weeks <w>
 *		Simulates trading stocks at the end of the <w>-th week of each
 *		month
 *
 *	-months <m> -weeks 5
 *		Simulates trading stocks at the end of the last week of every
 *		<m> months
 *
 *	-months <m> -weeks <w>
 *		Simulates trading stocks at the end of the <w>-th week of every
 *		<m> months
 *
 *	-months 0 -weeks <w>
 *		Simulates trading stocks every <w> weeks; thus "-months 0 -weeks 4"
 *		results in 13 trades per year
 *)
let weeks = ref 0;;

(*
 * Minimum number of equities to be held.  If too few equities pass the
 * screen, then the missing positions are held in cash.  For instance,
 * if "-min 10" is specified and only 7 stocks pass the screen, then 3
 * positions will be held in cash.  Cash positions have a 0% return
 *)
let emin = ref 1;;

(*
 * Maximum number of equities to be held.  If too many equities pass the
 * screen, the extras are discarded.  Equities are picked in order, based
 * on how they're sorted by the screen
 *)
let emax = ref 0;;

(*
 * If true, then stocks with a tied rank are included when "max" is specified
 *)
let ties = ref false;;

(*
 * Non-survivor bias.  This specifies the return to use if a new price
 * cannot be found for a stock.
 *
 * Note: This is a 1-based return, thus:
 *	1.10 = 10% gain
 *	0.70 = 30% loss
 *)
let bias = ref 1.0;;

(*
 * Weight of the first stock relative to the last.  Thus "-weight 4" causes
 * the first stock to be weight 4 times as much as the last when computing
 * the return for each period.
 *
 * A weight of 0 causes it to use the median return.
 *
 * If both a max and weight are specified, then the weight of the first
 * stock is <weight> and the weight of the <max> stock is 1.  Thus if there
 * are fewer than <max> stocks, then the last stock will have a weight
 * greater than 1
 *)
let firstwt = ref 1.0;;


(*
 * Specs for parsing command-line options
 *)

let beg_spec =
	("-begin", Arg.Set_string dbegin, "  starting date, YYYYMMDD");;
let end_spec =
	("-end", Arg.Set_string dend, "  ending date, YYYYMMDD");;
let months_spec =
	("-months", Arg.Set_int months, "  months per period (default=1)");;
let weeks_spec =
	("-weeks", Arg.Set_int weeks,
	"  weeks per period (default=0 -> end of month)");;
let emin_spec =
	("-min", Arg.Set_int emin, "  min stocks to hold, rest in cash");;
let emax_spec =
	("-max", Arg.Set_int emax, "  max stocks to hold");;
let ties_spec =
	("-ties", Arg.Set ties, "  include ties");;
let bias_spec =
	("-bias", Arg.Set_float bias,
	"  1-based return for non-surviving equities default=1.0 -> 0% gain");;
let wt_spec =
	("-weight", Arg.Set_float firstwt,
		"weight of first relative to last, default=1->equal weight");;
let add_screen nm =
	screens := !screens @ [nm];;
let screen_spec =
	("-screen", Arg.String add_screen,
		"  screen-name");;
let add_blend bw =
	blends := !blends @ [bw];;
let blend_spec =
	("-blend", Arg.Float add_blend,
		"  blend-weight, default=1");;
let add_file (nm:string) =
	files := !files @ [nm];;


(*
 * Compute the return for an equity.  If the equity is not found in the
 * new data, then the return is assumed to be 0% for the period
 *
 *	pold	old data for the equity to be processed
 *	t2d		Hashtbl mapping tkr to new equity data
 *	c2d		Hashtbl mapping cusip to new equity data
 *)

let comp_ret (t2d,c2d) pold =
	try
		let dtkr = tkr pold in
		let dcusip = cusip pold in
		let pr = price pold in
		if pr <= 0.0 then !bias
		else
		let pnew =								(* new data *)
			try Hashtbl.find c2d dcusip
			with Not_found -> Hashtbl.find t2d dtkr
		in
		let told = prdate pold in				(* price date of old data *)
		if null_price pold || told = 0.0 then !bias
					(* old price is empty or out of date *)
		else
		let tnew = prdate pnew in				(* price date of new data *)
		if null_price pnew || told >= tnew then !bias
					(* new price is empty or older than previous *)
		else
		let tsplit = dsplit pnew in				(* split date of new data *)
		let buypr =						(* split-adjusted purchase price *)
			if tsplit <= told || null_fsplit pnew then pr
				(* split occurred before old price *)
			else
				let f = fsplit pnew in
				if f = 0.0 then pr				(* no split factor *)
				else pr /. f
		in
		let tdiv = dateq1 pnew in
		let div =								(* dividend this period *)
			if told < tdiv && tdiv < tnew then divq1 pnew
					(* dividend was paid in previous period *)
			else 0.0
		in
		(*
		print_string dtkr;
		print_string " cusip=";
		print_string dcusip;
		print_string " told=";
		print_float told;
		print_string " tsplit=";
		print_float tsplit;
		print_string " tdiv=";
		print_float tdiv;
		print_string " tnew=";
		print_float tnew;
		print_string " pr=";
		print_float pr;
		print_string " b=";
		print_float buypr;
		print_string " s=";
		print_float (price pnew);
		print_string " r=";
		print_float ((div +. (price pnew)) /. buypr);
		print_newline ();
		*)
		(div +. (price pnew)) /. buypr
	with Not_found ->
		!bias;;


(*
 * Returns a function to compute weighted return for a period
 *)

let per_ret () =
	if !firstwt = 0.0 then median
	else if !firstwt = 1.0 then average
	else if !emax > 0 && not !ties then		(* use a fixed delta *)
		let delta = (1.0 -. !firstwt) /. ((float_of_int !emax) -. 1.0) in
		let rec accum w tw r lst =
			match lst with
				[] -> if r = 0.0 then 0.0 else r /. tw
			|	h::t -> accum (w +. delta) (w +. tw) (r +. w *. h) t
		in
		accum !firstwt 0.0 0.0
	else					(* delta depends on number of returns *)
		function returns ->
			begin
				if returns = [] then 0.0
				else
					let lenf = float_of_int (List.length returns) in
					let delta = (1.0 -. !firstwt) /. (lenf -. 1.0) in
					let rec accum w tw r lst =
						match lst with
							[] -> r /. tw
						|	h::t -> accum (w +. delta) (w +. tw)
											(r +. w *. h) t
					in
					accum !firstwt 0.0 0.0 returns
			end;;


(*
 * Returns a function to compute returns, including cash positions
 *)

let cash_ret () =
	if !emin <= 1 then
		fun r eqlst -> r
	else
		let em = !emin in
		fun r eqlst ->
			let len = List.length eqlst in
			if len < em then
				(* not enough stocks - include some cash positions *)
				(r *. (float_of_int len) +. (float_of_int (!emin - len)))
						/. (float_of_int !emin)
			else
				r;;


(*
 * Trade equities if the directory is relevant, otherwise carry the
 * existing equities forward with the new data
 *)

let trade action relevant (tkr2data,cusip2data) dir oldeq neweq =
	if Hashtbl.mem relevant dir then
		(* pick new equities *)
		action neweq
	else
		(* carry same equities forward *)
		let adde lst e =
			try
				let d =
					try Hashtbl.find cusip2data (cusip e)
					with Not_found -> Hashtbl.find tkr2data (tkr e)
				in
				d::lst
			with Not_found -> lst
		in
		List.fold_left adde [] oldeq;;


(*
 * Lists the directories that are within the begin & end range.
 * This is a mess, but it's difficult to automatically figure everything
 * out based on the directory/period names.  It's also difficult to
 * determine which ones should be traded, particularly when some trading
 * periods may be missing, in which case, the next one must be picked,
 * whether or not it's a typical trading period
 *
 * Returns:
 *	dirs	directories for which returns should be reported
 *	trade	hash of directories for which trades should be made
 *	eoy		hash of directories that are at the end of the year
 *)
 
let list_dirs_in_range dbg =
	let dirs1 = List.sort compare (Siarchive.periods dbg)
	(*	this loads a list of directories from a test file just so
			we can see if this function works correctly
		let fin = open_in "dates.txt" in
		let rec read_ln lst =
			let (rd,ln) =
				try
					let ln = chomp (input_line fin) in
					(true,ln)
				with End_of_file -> (false,"")
			in
			if rd then read_ln (ln::lst)
			else
				begin
					close_in fin;
					List.sort compare lst
				end
		in
		read_ln []
	*)
	in

	(* only keep those that are in range *)
	let in_range d =
		d >= !dbegin && d <= !dend
	in
	let dirs2 = List.filter in_range dirs1 in
	let len2 = List.length dirs2 in

	let month_day d = int_of_string (String.sub d 6 2) in
	let month d = int_of_string (String.sub d 4 2) in
	let year d = int_of_string (String.sub d 0 4) in
	let week_day tdays =
		let t = Unix.localtime (tdays *. 24.0 *. 60.0 *. 60.0) in
		t.Unix.tm_wday
	in
	let date3time yr mon mday =
		let digit_of_int v ndig =
			let zi = int_of_char '0' in
			let s = String.create ndig in
			let vr = ref v in
			for i=ndig-1 downto 0 do
				s.[i] <- char_of_int ((!vr mod 10) + zi);
				vr := !vr / 10
			done;
			s
		in
		date2time (
			(digit_of_int yr 4)
				^ (digit_of_int mon 2)
				^ (digit_of_int mday 2))
	in

	(* categorize directories by week *)
	let week = Hashtbl.create len2 in		(* week number within month *)
	let cat_week dlast dcur =
		let tlast = date2time dlast in
		let tcur = date2time dcur in
		let mlast = month dlast in
		let mcur = month dcur in
		let wday = week_day tcur in
		let mday = month_day dcur in
		let wk =
			if wday = 5 then (mday - 1) / 7 + 1
				(* friday 1st-7th counts as week 1 *)

			else if wday = 4 then (mday / 7) + 1
				(* thursday 1st-6th counts as week 1,
					7th as week 2 *)

			else
				(* any other day is rounded down, but if it still says
					week 4, after rounding, then put it into week 5 *)
				let w = mday / 7 in
				if w >= 4 then 5
				else w
		in
		if wday >= 4 && tcur -. tlast < 6.0 then
				(* previous period is within 6 days of this period *)
			if mlast = mcur then
				begin
					(* same month - previous period should go in the
						previous week *)
					let lwk = wk - 1 in
					Hashtbl.replace week dlast lwk
				end;

		Hashtbl.replace week dcur wk;
		dcur
	in
	ignore (List.fold_left cat_week "19900101" dirs2);
	Hashtbl.remove week "19900101";

	(* categorize directories as eom *)
	let eom = Hashtbl.create len2 in		(* end of month *)
	let cat_eom dlast dcur =
		let tlast = date2time dlast in
		let tcur = date2time dcur in
		let mlast = month dlast in
		let mcur = month dcur in
		if tcur -. tlast > 40.0 then
			(* too much time elapsed - treat previous as eom *)
			Hashtbl.replace eom dlast 0
		else if (mlast mod 12) + 1 = mcur then
			(* adjacent months - closest one is eom *)
			let t =
				let t0 =	(* 1st of current month *)
					date3time
						(int_of_string (String.sub dcur 0 4))
						(int_of_string (String.sub dcur 4 2))
						1
				in
				t0 -. 1.0	(* last day of previous month *)
			in
			if t -. tlast < tcur -. t then
				Hashtbl.replace eom dlast 0
			else
				Hashtbl.replace eom dcur 0
		else if mlast <> mcur then
			begin
				(* months are not adjacent - both are eom *)
				Hashtbl.replace eom dlast 0;
				Hashtbl.replace eom dcur 0
			end;
		(* populate eom based on weekday *)
		let wday = week_day tcur in
		let wk = Hashtbl.find week dcur in
		if wday >= 4 && tcur -. tlast < 6.0 then
				(* previous period is within 6 days of this period *)
			if mlast = mcur then
				begin
					(* same month - see if previous period is eom *)
					if Hashtbl.find week dlast < 1 then
						Hashtbl.replace eom dlast 0
				end
			else Hashtbl.replace eom dlast 0;
				(* previous period is close to this, but in previous
					month, thus it must be eom *)

		if wk > 4 || wk < 1 then Hashtbl.replace eom dcur 0;
		dcur
	in
	ignore (List.fold_left cat_eom "19900101" dirs2);
	Hashtbl.remove eom "19900101";

	(* categorize eolw directories *)
	let eolw = Hashtbl.create len2 in		(* end of last week of month *)
	let cat_eolw dlast dcur =
		let tcur = date2time dcur in
		if week_day tcur >= 5 then		(* end of a week *)
			begin
				if String.sub dlast 0 6 = String.sub dcur 0 6 then
					(* dlast is in the same month - it can't be eolw *)
					Hashtbl.remove eolw dlast;

				Hashtbl.replace eolw dcur 0
			end;

		dcur
	in
	ignore (List.fold_left cat_eolw "19900101" dirs2);
	Hashtbl.remove eolw "19900101";

	(* categorize directories as eoy *)
	let eoy = Hashtbl.create (len2 / 12 + 1) in		(* end of year *)
	let cat_eoy dlast dcur =
		if year dlast <> year dcur then
			begin
				if Hashtbl.mem eom dcur then
					begin
						let mcur = month dcur in
						let daycur = month_day dcur in
						if mcur = 1 && daycur < 15 then
							(* eom before 1/15 - must be end of
									previous year *)
							Hashtbl.replace eoy dcur 0
						else if mcur = 12 && daycur > 15 then
							(* eom after 12/15 - must be end of
									current year *)
							Hashtbl.replace eoy dcur 0
					end
				else
					begin
						(* different years and current is not eom,
								thus previous must be end of its year *)
						Hashtbl.replace eoy dlast 0
					end
			end
		else if Hashtbl.mem eom dcur
					&& month dcur = 12 && month_day dcur > 15 then
			begin
				(* eom after 12/15 - must be end of current year *)
				Hashtbl.replace eoy dcur 0
			end;
		dcur
	in
	ignore (List.fold_left cat_eoy "19900101" dirs2);
	Hashtbl.remove eoy "19900101";

	(* identify directories which are traded *)
	let dh = List.hd dirs2 in
	let month_basis d curoff =
		let m = (year d) * 12 + (month d) - 1 in
		if month_day d < curoff then m - 1
		else m
	in
	let trade = Hashtbl.create len2 in		(* trade this period *)
	let cat_trade fnext d =
		if fnext d then
			begin
				Hashtbl.replace trade d 0;

				(* generate a function to recognize the first directory
						in the next period *)

				if !weeks = 0 then
					(* monthly trading *)

					(* determine number of months since beginning *)
					let mbeg = month_basis dh 15 in
					if !months <= 1 then		(* trade every eom *)
						begin
							function d ->
								Hashtbl.mem eom d
						end
					else	(* trade eom every so many months *)
						let md = month_basis d 0 in
						let n = (md - mbeg) / !months in
						let m = mbeg + (n + 1) * !months in	(* add a period *)
						let mon = (m mod 12) + 1 in
						let yr = m / 12 in
						let tn = date3time yr mon 21 in
						begin
							function d ->
								Hashtbl.mem eom d && date2time d >= tn
						end

				else if !weeks = 5 && !months >= 1 then
					(* monthly trading at end of last week *)

					(* determine number of months since beginning *)
					if !months <= 1 then		(* trade every eolw *)
						let tn = (date2time d) +. 21.0 in
						begin
							function d ->
								(Hashtbl.mem eolw d || Hashtbl.mem eom d)
									&& date2time d >= tn
						end
					else	(* trade eolw every so many months *)
						let mbeg = month_basis dh 15 in
						let md = month_basis d 0 in
						let n = (md - mbeg) / !months in
						let m = mbeg + (n + 1) * !months in	(* add a period *)
						let mon = (m mod 12) + 1 in
						let yr = m / 12 in
						let tn = date3time yr mon 21 in
						begin
							function d ->
								(Hashtbl.mem eolw d || Hashtbl.mem eom d)
									&& date2time d >= tn
						end

				else if !weeks >= 1 && !weeks <= 4 && !months >= 1 then
					(* trade the same week every so many months *)

					(* determine number of months since beginning *)
					let mbeg = month_basis dh 0 in
					let md = month_basis d 0 in
					let n = (md - mbeg) / !months in
					let m = mbeg + (n + 1) * !months in	(* add a period *)
					let mon = (m mod 12) + 1 in
					let yr = m / 12 in
					let tn = date3time yr mon 1 in
					let tn2 =
						let m2 = (mon mod 12) + 1 in
						let y2 = (yr + mon / 12) in
						date3time y2 m2 1
					in
					begin
						function d ->
							let td = date2time d in
							td >= tn2 ||
								(td >= tn && Hashtbl.find week d >= !weeks)
					end

				else if !months >= 1 then
					invalid_arg "weeks are outside of a month"

				else
					(* trade every so many weeks *)

					(* determine number of weeks (in days) since beginning *)
					let dir2days d =
						let t = date2time d in
						let wday = week_day t in
						(int_of_float t) + 5 - wday		(* round up to friday *)
					in
					let dbeg = dir2days dh in
					let dd = dir2days d in
					let n = (dd - dbeg) / 7 / !weeks in
					let nadd = (n + 1) * 7 * !weeks in
					let tn = float_of_int (dbeg + nadd) in
					begin
						function d ->
							let td = date2time d in
							td >= tn
					end
			end
		else
			fnext
	in
	let f = List.fold_left cat_trade (function p -> true) dirs2 in
	ignore(f "19890101");
	Hashtbl.remove trade "19890101";

	(*
	let show_it d =
		print_string d;
		let w = Hashtbl.find week d in
		if w >= 1 then
			begin
				let s = String.make w '*' in
				print_string " ";
				print_string s
			end;
		if Hashtbl.mem eom d then
			print_string " eom";
		if Hashtbl.mem eolw d then
			print_string " eolw";
		if Hashtbl.mem eoy d then
			print_string " eoy";
		if Hashtbl.mem trade d then
			print_string " trade";
		print_newline ();
	in
	List.iter show_it dirs2;
	if true then exit 0;
	*)

	(dirs2, trade, eoy);;
