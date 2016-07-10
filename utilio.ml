
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


let i64_256 = Int64.of_int 256;;


(*
 * Force int to char via modulo
 *)
let int2char v = char_of_int (v land 0xff);;


(*
 * Convert numbers to little-endian text
 *)
let text_of_int2 v =
	let b = String.create 2 in
	String.set b 0 (int2char v);
	String.set b 1 (int2char (v / 256));
	b;;

let text_of_int4 v =
	let b = String.create 4 in
	String.set b 0 (int2char v);
	String.set b 1 (int2char (v / 256));
	String.set b 2 (int2char (v / (256 * 256)));
	String.set b 3 (int2char (v / (256 * 256 * 256)));
	b;;

let text_of_int64 v =
	let b = String.create 8 in
	let v2 = ref v in
	for i=0 to 7 do
		String.set b i (int2char (Int64.to_int !v2));
		v2 := Int64.div !v2 i64_256
	done;
	b;;


(*
 * Convert little-endian text to numbers
 *)
let int_of_text s =
	let v = ref 0 in
	let n = String.length s in
	for i=n-1 downto 0 do
		v := !v * 256 + (int_of_char s.[i])
	done;
	!v;;

let int64_of_text s =
	let v = ref Int64.zero in
	let n = String.length s in
	for i=n-1 downto 0 do
		let b = Int64.of_int (int_of_char s.[i]) in
		v := Int64.add (Int64.mul !v i64_256) b
	done;
	!v;;


(*
 * Reader base
 *)
class virtual reader =
	object
		method virtual getpos: int
		method virtual setpos: int -> unit

		method virtual read_char: unit -> char
		method virtual read_string: int -> string
	end;;


let getpos r = r#getpos;;
let setpos r = r#setpos;;

let read_char r = r#read_char ();;
let read_string r len = r#read_string len;;		(* used a lot so don't curry *)

let read_byte r = int_of_char (read_char r);;
let read_int2 r = int_of_text (read_string r 2);;
let read_int4 r = int_of_text (read_string r 4);;
let read_int64 r = int64_of_text (read_string r 8);;

let read_text_len1 r = read_string r (read_byte r);;

let read_char_str r =
	let c = read_char r in
	if c = ' ' then ""
	else String.make 1 c;;


(*
 * Writer base
 *)
class virtual writer =
	object (self)
		method virtual write_char: char -> unit
		method virtual write_string: string -> unit
	end;;


let write_char w = w#write_char;;
let write_string w s = w#write_string s;;		(* used a lot so don't curry *)

let write_byte w b = write_char w (int2char b);;
let write_int2 w i = write_string w (text_of_int2 i);;
let write_int4 w i = write_string w (text_of_int4 i);;
let write_int64 w i = write_string w (text_of_int64 i);;

let write_text_len1 w s =
	let len = String.length s in
	if len >= 256 then invalid_arg "length exceeds 255";
	write_char w (int2char len);
	if len <> 0 then write_string w s;;

let write_char_str w s =
	if s = "" then write_char w ' '
	else if String.length s > 1 then invalid_arg "length exceeds 1"
	else write_char w s.[0];;


(*
 * String from which data may be read
 *)
class string_in_buffer src =
	object (self)
		inherit reader

		val mutable i = 0

		method getpos = i
		method setpos j = i <- j

		method read_char () =
			let c = src.[i] in
			i <- i + 1;
			c

		method read_string len =
			let s = String.sub src i len in
			i <- i + len;
			s
	end;;

let string_in_buffer src = new string_in_buffer src;;


(*
 * String into which data may be written
 *)
class string_out_buffer beglen =
	let buf = Buffer.create beglen in

	object
		inherit writer

		method contents = Buffer.contents buf

		method write_char c = Buffer.add_char buf c
		method write_string s = Buffer.add_string buf s
	end;;

let string_out_buffer beglen = new string_out_buffer beglen;;
let contents b = b#contents;;


type fd_mode =
		Read
	|	Write
	|	Create;;

class fd_type fname mode =
	let flags =
		match mode with
			Read -> [Unix.O_RDONLY]
		|	Write -> [Unix.O_RDWR]
		|	Create -> [Unix.O_CREAT; Unix.O_RDWR]
	in
	let fd = Unix.openfile fname flags 0o644 in

	object (self)
		inherit reader
		inherit writer

		method close = Unix.close fd


		(* get/set position *)
		method getpos = Unix.lseek fd 0 Unix.SEEK_CUR
		method getpos64 = Unix.LargeFile.lseek fd Int64.zero Unix.SEEK_CUR

		method setpos pos =
			if Unix.lseek fd pos Unix.SEEK_SET <> pos then
				invalid_arg "lseek offset"

		method setpos64 pos =
			if Unix.LargeFile.lseek fd pos Unix.SEEK_SET <> pos then
				invalid_arg "lseek offset"


		(* readers *)
		method read_char () =
			let s = String.create 1 in
			if Unix.read fd s 0 1 = 1 then s.[0]
			else raise End_of_file

		method read_string len =
			if len <= 0 then ""
			else
				let s = String.create len in
				let rec read_more offset =
					let nr = len - offset in
					let nr2 = Unix.read fd s offset nr in
					if nr2 = 0 then raise End_of_file
					else if nr2 < nr then read_more (offset + nr2)
					else s
				in
				read_more 0


		(* writers *)
		method write_char c =
			let s = String.make 1 c in
			if Unix.write fd s 0 1 <> 1 then raise End_of_file

		method write_string s =
			let len = String.length s in
			if len > 0 then
					if Unix.write fd s 0 len <> len then raise End_of_file
	end;;


let fd_type fname mode = new fd_type fname mode;;
let close_fd f = f#close;;
let getpos64 f = f#getpos64;;
let setpos64 f = f#setpos64;;
