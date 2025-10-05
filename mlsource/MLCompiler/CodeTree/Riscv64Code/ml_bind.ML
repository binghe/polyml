(*
    Copyright (c) 2021-2 David C. J. Matthews
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

local
    structure Riscv64Assembly =
        Riscv64Assembly (
            structure Debug         = Debug
            and       Pretty        = Pretty
            and       CodeArray     = CodeArray
        )

    structure Riscv64PreAssembly =
        Riscv64PreAssembly (
            structure Riscv64Assembly = Riscv64Assembly
            and       Debug         = Debug
            and       Pretty        = Pretty
        )

    structure Riscv64Foreign =
        Arm64ForeignCall (
            structure CodeArray     = CodeArray
            and       Arm64PreAssembly = Riscv64PreAssembly
            and       Debug         = Debug
        )

    structure Riscv64ICode =
        Arm64ICode (
            structure Arm64Code     = Riscv64PreAssembly
        )

    structure Riscv64ICodeIdentify =
        Arm64IdentifyReferences (
            structure Debug         = Debug
            structure Arm64ICode    = Riscv64ICode
            structure IntSet        = IntSet
        )

    structure Riscv64PushRegs =
        Arm64PushRegisters (
            structure Arm64ICode    = Riscv64ICode
            structure IntSet        = IntSet
            structure Identify      = Riscv64ICodeIdentify
        )

    structure Riscv64Opt =
        Arm64ICodeOptimise (
            structure Arm64ICode    = Riscv64ICode
            structure IntSet        = IntSet
            structure Identify      = Riscv64ICodeIdentify
            structure Debug         = Debug
            structure Pretty        = Pretty
        )

    structure Riscv64IAllocate =
        Arm64AllocateRegisters (
            structure Arm64ICode    = Riscv64ICode
            structure Identify      = Riscv64ICodeIdentify
            structure IntSet        = IntSet
        )

    structure Riscv64ICodeGenerate =
        Arm64ICodeToArm64Code (
            structure Debug         = Debug
            structure Arm64ICode    = Riscv64ICode
            structure Identify      = Riscv64ICodeIdentify
            structure Pretty        = Pretty
            structure IntSet        = IntSet
            structure Arm64PreAssembly = Riscv64PreAssembly
            structure Arm64Assembly = Riscv64Assembly
            structure Strongly      = StronglyConnected
        )

    structure Riscv64ICodeTransform =
        Arm64ICodeTransform (
            structure Debug         = Debug
            structure Arm64ICode    = Riscv64ICode
            structure Identify      = Riscv64ICodeIdentify
            structure Allocate      = Riscv64IAllocate
            structure PushRegisters = Riscv64PushRegs
            structure Optimise      = Riscv64Opt
            structure Pretty        = Pretty
            structure IntSet        = IntSet
            structure Codegen       = Riscv64ICodeGenerate
        )

in

    structure Riscv64Code =
        Arm64CodetreeToICode (
            structure BackendTree   = BackendIntermediateCode
            structure Debug         = Debug
            structure Arm64ICode    = Arm64ICode
            structure Arm64Foreign  = Arm64Foreign
            structure ICodeTransform = Riscv64ICodeTransform
            structure CodeArray     = CodeArray
            and       Pretty        = Pretty
        )

end;
