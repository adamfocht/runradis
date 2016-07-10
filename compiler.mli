
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

type tscrfunc = Equity.equity list -> Equity.equity list;;

val compile_all : string -> string list -> tscrfunc;;
val parse_all : string -> string list -> string;;
val screen_names : unit -> string list;;

val compile : string -> tscrfunc;;
val gen : Op.op -> tscrfunc;;
val set_prefilter : unit -> unit;;
