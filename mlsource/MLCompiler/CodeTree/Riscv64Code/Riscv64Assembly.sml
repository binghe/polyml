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

    infix 5 << <<+ <<- >> >>+ >>- ~>> ~>>+ ~>>- (* shift operators *)
    infix 3 andb orb xorb andbL orbL xorbL andb8 orb8 xorb8

    val op << = Word32.<< and op >> = Word32.>> and op ~>> = Word32.~>>
    and op andb = Word32.andb and op orb = Word32.orb

    val word32ToWord8 = Word8.fromLargeWord o Word32.toLargeWord
    and word8ToWord32 = Word32.fromLargeWord o Word8.toLargeWord
    and word32ToWord = Word.fromLargeWord o Word32.toLargeWord
    and wordToWord32 = Word32.fromLargeWord o Word.toLargeWord
    and word8ToWord = Word.fromLargeWord o Word8.toLargeWord

    (* XReg is used for fixed point registers (XZero is encoded as 0) *)
    datatype xReg = XReg of Word8.word | XZero
    (* VReg is used for floating point registers *)
    and      vReg = VReg of Word8.word

    fun xRegOrXZ (XReg w) = w
    |   xRegOrXZ XZero = 0w0
    and xRegOnly (XReg w) = w
    |   xRegOnly XZero = raise InternalError "XZero not valid here"

    val                     X1  = XReg 0w1  and X2  = XReg 0w2   and X3  = XReg 0w3
    and X4  = XReg 0w4  and X5  = XReg 0w5  and X6  = XReg 0w6   and X7  = XReg 0w7
    and X8  = XReg 0w8  and X9  = XReg 0w9  and X10 = XReg 0w10  and X11 = XReg 0w11
    and X12 = XReg 0w12 and X13 = XReg 0w13 and X14 = XReg 0w14  and X15 = XReg 0w15
    and X16 = XReg 0w16 and X17 = XReg 0w17 and X18 = XReg 0w18  and X19 = XReg 0w19
    and X20 = XReg 0w20 and X21 = XReg 0w21 and X22 = XReg 0w22  and X23 = XReg 0w23
    and X24 = XReg 0w24 and X25 = XReg 0w25 and X26 = XReg 0w26  and X27 = XReg 0w27
    and X28 = XReg 0w28 and X29 = XReg 0w29 and X30 = XReg 0w30  and X31 = XReg 0w31

    val X_MLHeapLimit       = X25 (* ML Heap limit pointer *)
    and X_MLAssemblyInt     = X26 (* ML assembly interface pointer. *)
    and X_MLHeapAllocPtr    = X27 (* ML Heap allocation pointer. *)
    and X_MLStackPtr        = X28 (* ML Stack pointer. *)
    and X_LinkReg           = X30 (* Link reg - return address *)

    fun vReg (VReg v) = v
    (* Only the first eight registers are currently used by ML. *)
    val V0 = VReg 0w0 and V1 = VReg 0w1 and V2 = VReg 0w2 and V3 = VReg 0w3
    and V4 = VReg 0w4 and V5 = VReg 0w5 and V6 = VReg 0w6 and V7 = VReg 0w7

    datatype instr =
        SimpleInstr of Word32.word
    |   Label of labels

    structure Sharing =
    struct
        type closureRef = closureRef
        type instr = instr
        type xReg = xReg
        type vReg = vReg
    end
end;
