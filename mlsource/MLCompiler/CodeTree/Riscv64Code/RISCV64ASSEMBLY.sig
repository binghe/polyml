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

signature ARM64ASSEMBLY =
sig
    type closureRef
    type instr
    type machineWord = Address.machineWord

    datatype xReg = XReg of Word8.word
    and      fReg = FReg of Word8.word

    val X0:  xReg   and X1:  xReg   and X2:  xReg   and X3: xReg
    and X4:  xReg   and X5:  xReg   and X6:  xReg   and X7: xReg
    and X8:  xReg   and X9:  xReg   and X10: xReg   and X11: xReg
    and X12: xReg   and X13: xReg   and X14: xReg   and X15: xReg
    and X16: xReg   and X17: xReg   and X18: xReg   and X19: xReg
    and X20: xReg   and X21: xReg   and X22: xReg   and X23: xReg
    and X24: xReg   and X25: xReg   and X26: xReg   and X27: xReg
    and X28: xReg   and X29: xReg   and X30: xReg   and X31: xReg

    (* NOTE: There are totally 32 floating-point registers in RISC-V 64, we
       only use 8 of them?
     *)
    val F0:  fReg   and F1:  fReg   and F2:  fReg   and F3: fReg
    and F4:  fReg   and F5:  fReg   and F6:  fReg   and F7: fReg

    (* Integer Register-Immediate Instructions *)
    val addImmediate:         {rd: xReg, rs1: xReg, imm: word} -> instr
    and setLessThanImmediate: {rd: xReg, rs1: xReg, imm: word} -> instr
    and andImmediate:         {rd: xReg, rs1: xReg, imm: word} -> instr
    and orImmediate:          {rd: xReg, rs1: xReg, imm: word} -> instr
    and xorImmediate:         {rd: xReg, rs1: xReg, imm: word} -> instr

    val shiftLeftLogicalImmediate:     {rd: xReg, rs1: xReg, imm: word} -> instr
    and shiftRightLogicalImmediate:    {rd: xReg, rs1: xReg, imm: word} -> instr
    and shiftRightArithmeticImmediate: {rd: xReg, rs1: xReg, imm: word} -> instr

    val loadUpperImmediate:    {rd: xRef, imm: word} -> instr
    and addUpperImmediateToPC: {rd: xRef, imm: word} -> instr

    (* Integer Register-Register Operations *)
    val add:                  {rd: xReg, rs1: xReg, rs2: xReg} -> instr
    and sub:                  {rd: xReg, rs1: xReg, rs2: xReg} -> instr
    and setLessThan:          {rd: xReg, rs1: xReg, rs2: xReg} -> instr
    and setLessThanUnsigned:  {rd: xReg, rs1: xReg, rs2: xReg} -> instr
    and andRegister:          {rd: xReg, rs1: xReg, rs2: xReg} -> instr
    and orRegister:           {rd: xReg, rs1: xReg, rs2: xReg} -> instr
    and xorRegister:          {rd: xReg, rs1: xReg, rs2: xReg} -> instr
    and shiftLeftLogical:     {rd: xReg, rs1: xReg, rs2: xReg} -> instr
    and shiftRightLogical:    {rd: xReg, rs1: xReg, rs2: xReg} -> instr
    and shiftRightArithmetic: {rd: xReg, rs1: xReg, rs2: xReg} -> instr

    structure Sharing:
    sig
        type closureRef = closureRef
        type instr = instr
        type xReg = xReg
        type fReg = fReg
    end
end;
