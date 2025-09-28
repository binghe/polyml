(*
    Copyright (c) 2025  Chun Tian (binghe) <binghe.lisp@gmail.com>

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    Licence version 2.1 as published by the Free Software Foundation.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public Licence for more details.

    You should have received a copy of the GNU Lesser General Public
    Licence along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
*)

functor Riscv64Assembly (
    structure Debug: DEBUG
    and       Pretty: PRETTY
    and       CodeArray: CODEARRAY
) : RISCV64ASSEMBLY =

struct
    open CodeArray Address

    val wordsPerNativeWord: word = Address.nativeWordSize div Address.wordSize

    local
        val isBigEndian: unit -> bool = RunCall.rtsCallFast1 "PolyIsBigEndian"
    in
        val isBigEndian = isBigEndian()
    end

    exception InternalError = Misc.InternalError

    infix 5 << <<+ <<- >> >>+ >>- ~>> ~>>+ ~>>- (* Shift operators *)
    infix 3 andb orb xorb andbL orbL xorbL andb8 orb8 xorb8

    val op << = Word32.<< and op >> = Word32.>> and op ~>> = Word32.~>>
    and op andb = Word32.andb and op orb = Word32.orb

    val word32ToWord8 = Word8.fromLargeWord o Word32.toLargeWord
    and word8ToWord32 = Word32.fromLargeWord o Word8.toLargeWord
    and word32ToWord = Word.fromLargeWord o Word32.toLargeWord
    and wordToWord32 = Word32.fromLargeWord o Word.toLargeWord
    and word8ToWord = Word.fromLargeWord o Word8.toLargeWord

    (* XReg is used for fixed point registers *)
    datatype xReg = XReg of Word8.word
    (* FReg is used for the floating point registers *)
    and      fReg = FReg of Word8.word

    structure Sharing =
    struct
        type closureRef = closureRef
        type instr = instr
        type xReg = xReg
        type fReg = fReg
    end
end;
