
/*
	RunRadis: Run RadiScript screens
	Copyright (C) 2009  James Hahn

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
 */

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/fail.h>
#include "bzlib.h"

value caml_bz2_compress(value dest, value src)
{
  CAMLparam2 (dest, src);

  char* destBuf = String_val(dest);
  unsigned int destlen = caml_string_length(dest);

  char* srcBuf = String_val(src);
  unsigned int srclen = caml_string_length(src);

  switch(BZ2_bzBuffToBuffCompress(destBuf, &destlen, srcBuf, srclen,
				9, 0, 0)) {
  case BZ_OK:
	  break;

  case BZ_OUTBUFF_FULL:
	  caml_failwith("dest is too small for bz2_compress");
	  break;

  default:
	  caml_failwith("bz2_compress failed");
	  break;
  }

  CAMLreturn (Val_int(destlen));
}


value caml_bz2_uncompress(value dest, value src)
{
  CAMLparam2 (dest, src);

  char* destBuf = String_val(dest);
  unsigned int destlen = caml_string_length(dest);

  char* srcBuf = String_val(src);
  unsigned int srclen = caml_string_length(src);

  switch(BZ2_bzBuffToBuffDecompress(destBuf, &destlen, srcBuf, srclen,
				0, 0)) {
  case BZ_OK:
	  break;

  case BZ_OUTBUFF_FULL:
	  caml_failwith("dest is too small for bz2_uncompress");
	  break;

  default:
	  caml_failwith("bz2_uncompress failed");
	  break;
  }

  CAMLreturn (Val_int(destlen));
}
