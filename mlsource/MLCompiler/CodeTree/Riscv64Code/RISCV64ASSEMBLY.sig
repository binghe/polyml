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

signature RISCV64ASSEMBLY =
sig
    type closureRef
    type instr
    type machineWord = Address.machineWord
    type xReg
    type vReg

 (* The missing X0 is XZero *)
    val                 X1:  xReg   and X2:  xReg   and X3: xReg
    and X4:  xReg   and X5:  xReg   and X6:  xReg   and X7: xReg
    and X8:  xReg   and X9:  xReg   and X10: xReg   and X11: xReg
    and X12: xReg   and X13: xReg   and X14: xReg   and X15: xReg
    and X16: xReg   and X17: xReg   and X18: xReg   and X19: xReg
    and X20: xReg   and X21: xReg   and X22: xReg   and X23: xReg
    and X24: xReg   and X25: xReg   and X26: xReg   and X27: xReg
    and X28: xReg   and X29: xReg   and X30: xReg   and X31: xReg

    val X_MLHeapLimit:    xReg (* ML Heap limit pointer *)
    and X_MLAssemblyInt:  xReg (* ML assembly interface pointer. *)
    and X_MLHeapAllocPtr: xReg (* ML Heap allocation pointer. *)
    and X_MLStackPtr:     xReg (* ML Stack pointer. *)
    and X_LinkReg:        xReg (* Link reg - return address *)

 (* V8 - V31 is currently not used in ML *)
    val V0:  vReg   and V1:  vReg   and V2:  vReg   and V3: vReg
    and V4:  vReg   and V5:  vReg   and V6:  vReg   and V7: vReg

 (* Integer Register-Immediate Instructions *)
    val addImmediate:         {rd: xReg, rs1: xReg, imm: word} -> instr
    val addImmediateWord:     {rd: xReg, rs1: xReg, imm: word} -> instr
    and setLessThanImmediate: {rd: xReg, rs1: xReg, imm: word} -> instr
    and andImmediate:         {rd: xReg, rs1: xReg, imm: word} -> instr
    and orImmediate:          {rd: xReg, rs1: xReg, imm: word} -> instr
    and xorImmediate:         {rd: xReg, rs1: xReg, imm: word} -> instr

    val shiftLeftLogicalImmediate:         {rd: xReg, rs1: xReg, imm: word} -> instr
    and shiftLeftLogicalImmediateWord:     {rd: xReg, rs1: xReg, imm: word} -> instr
    and shiftRightLogicalImmediate:        {rd: xReg, rs1: xReg, imm: word} -> instr
    and shiftRightLogicalImmediateWord:    {rd: xReg, rs1: xReg, imm: word} -> instr
    and shiftRightArithmeticImmediate:     {rd: xReg, rs1: xReg, imm: word} -> instr
    and shiftRightArithmeticImmediateWord: {rd: xReg, rs1: xReg, imm: word} -> instr

    val loadUpperImmediate:    {rd: xRef, imm: word} -> instr
    and addUpperImmediateToPC: {rd: xRef, imm: word} -> instr

 (* Integer Register-Register Operations *)
    val add:                      {rd: xReg, rs1: xReg, rs2: xReg} -> instr
    val addWord:                  {rd: xReg, rs1: xReg, rs2: xReg} -> instr
    and sub:                      {rd: xReg, rs1: xReg, rs2: xReg} -> instr
    and subWord:                  {rd: xReg, rs1: xReg, rs2: xReg} -> instr
    and setLessThan:              {rd: xReg, rs1: xReg, rs2: xReg} -> instr
    and setLessThanUnsigned:      {rd: xReg, rs1: xReg, rs2: xReg} -> instr
    and andRegister:              {rd: xReg, rs1: xReg, rs2: xReg} -> instr
    and orRegister:               {rd: xReg, rs1: xReg, rs2: xReg} -> instr
    and xorRegister:              {rd: xReg, rs1: xReg, rs2: xReg} -> instr
    and shiftLeftLogical:         {rd: xReg, rs1: xReg, rs2: xReg} -> instr
    and shiftLeftLogicalWord:     {rd: xReg, rs1: xReg, rs2: xReg} -> instr
    and shiftRightLogical:        {rd: xReg, rs1: xReg, rs2: xReg} -> instr
    and shiftRightLogicalWord:    {rd: xReg, rs1: xReg, rs2: xReg} -> instr
    and shiftRightArithmetic:     {rd: xReg, rs1: xReg, rs2: xReg} -> instr
    and shiftRightArithmeticWord: {rd: xReg, rs1: xReg, rs2: xReg} -> instr

 (* Unconditional Jumps *)
    val jumpAndLink:         {rd: xReg, imm: word} -> instr
    and jumpAndLinkRegister: {rd: xReg, rs1: xReg, imm: word} -> instr

 (* Conditional Branches *)
    val branchEqual:        {rs1: xReg, rs2: xReg, imm: word} -> instr
    and branchNotEqual:     {rs1: xReg, rs2: xReg, imm: word} -> instr
    and branchLessThan:     {rs1: xReg, rs2: xReg, imm: word} -> instr
    and branchGreaterEqual: {rs1: xReg, rs2: xReg, imm: word} -> instr

 (* Load and Store Instructions *)
    val load:              {rd: xReg, rs1: xReg, imm: word} -> instr
    val loadWord:          {rd: xReg, rs1: xReg, imm: word} -> instr
    and loadHigh:          {rd: xReg, rs1: xReg, imm: word} -> instr
    and loadHighUnsigned:  {rd: xReg, rs1: xReg, imm: word} -> instr
    and loadByte:          {rd: xReg, rs1: xReg, imm: word} -> instr
    and loadByteUnsigned:  {rd: xReg, rs1: xReg, imm: word} -> instr
    and store:             {rs1: xReg, rs2: xReg, imm: word} -> instr
    and storeWord:         {rs1: xReg, rs2: xReg, imm: word} -> instr
    and storeHigh:         {rs1: xReg, rs2: xReg, imm: word} -> instr
    and storeByte:         {rs1: xReg, rs2: xReg, imm: word} -> instr

 (* Memory Ordering Instructions *)
    val fence: {rd: xReg, rs1: xReg, imm: word} -> instr

 (* Environment Call and Breakpoints *)
    val environmentCall:  {rd: xReg, rs1: xReg}
    and environmentBreak: {rd: xReg, rs1: xReg}

    structure Sharing:
    sig
        type closureRef = closureRef
        type instr = instr
        type xReg = xReg
        type vReg = vReg
    end
end;
