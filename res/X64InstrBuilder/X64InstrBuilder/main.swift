//
//  main.swift
//  X64InstrBuilder
//
//  Created by Jiten on 6/2/22.
//

import Foundation

struct Instruction {
    enum Prefix: Int {
        case rex        = 0x40
        case rexW       = 0x48
        case rexR       = 0x44
        case rexX       = 0x42
        case rexB       = 0x41
        case opOvrrd    = 0x66
        case addOvrrd   = 0x67
        case lock       = 0xf0
        case repe       = 0xf3
        case repne      = 0xf2
        case csOvrd     = 0x2e
        case ssOvrd     = 0x36
        case dsOvrd     = 0x3e
        case esOvrd     = 0x26
        case fsOvrd     = 0x64
        case gsOvrd     = 0x65
        
        init?(from str: String) {
            if str == "REX" { self = .rex }
            else if str == "REX.W" || str == "REX.w" { self = .rexW }
            else if str == "REX.R" || str == "REX.r" { self = .rexR }
            else if str == "REX.X" || str == "REX.x" { self = .rexX }
            else if str == "REX.B" || str == "REX.b" { self = .rexB }
            else if str == "LOCK" { self = .lock }
            else { return nil }
        }
        
        static var strings: [String] {
            [
                "REX +",
                "REX.W +",
                "REX.w +",
                "REX.R +",
                "REX.r +",
                "REX.X +",
                "REX.x +",
                "REX.B +",
                "REX.b +",
                "REX.W",
                "REX.w",
                "REX.R",
                "REX.r",
                "REX.X",
                "REX.x",
                "REX.B",
                "REX.b",
                "REX",
            ]
        }
        
        var strRepr: String {
            switch self {
            case .rex:
                return "Prefix.Rex"
            case .rexW:
                return "Prefix.RexW"
            case .rexR:
                return "Prefix.RexR"
            case .rexX:
                return "Prefix.RexX"
            case .rexB:
                return "Prefix.RexB"
            case .opOvrrd:
                return "Prefix.OpOvrd"
            case .addOvrrd:
                return "Prefix.AddOvrd"
            case .lock:
                return "Prefix.Lock"
            case .repe:
                return "Prefix.Repez"
            case .repne:
                return "Prefix.Repnez"
            case .csOvrd:
                return "Prefix.CsOvrd"
            case .ssOvrd:
                return "Prefix.SsOvrd"
            case .dsOvrd:
                return "Prefix.DsOvrd"
            case .esOvrd:
                return "Prefix.EsOvrd"
            case .fsOvrd:
                return "Prefix.FsOvrd"
            case .gsOvrd:
                return "Prefix.GsOvrd"
            }
        }
        
        static let lockMnemonics: [String] = [
            "ADC",
            "ADD",
            "AND",
            "BTC",
            "BTR",
            "BTS",
            "CMPXCHG",
            "CMPXCHG8B",
            "CMPXCHG16B",
            "DEC",
            "INC",
            "NEG",
            "NOT",
            "OR",
            "SBB",
            "SUB",
            "XADD",
            "XCHG",
            "XOR"
        ]
        
        static let repMnemonics: [String] = [
            "INS",
            "LODS",
            "MOVS",
            "OUTS",
            "STOS"
        ]
        
        static let repeMnemonics: [String] = [
            "CMPS",
            "CMPSB",
            "CMPSW",
            "CMPSD",
            "SCAS",
            "SCASB",
            "SCASW",
            "SCASD",
        ]
        
        static let repneMnemonics: [String] = repeMnemonics
    }
    
    enum Encoding {
        case po, d(Int), i, ri, rm, mr, rmo, mor
        
        init(from str: String, memOpIdx: Int?, hasMOfst: Bool, hasReg: Bool) {
            if str.contains("/0") { self = .d(0) }
            else if str.contains("/1") { self = .d(1) }
            else if str.contains("/2") { self = .d(2) }
            else if str.contains("/3") { self = .d(3) }
            else if str.contains("/4") { self = .d(4) }
            else if str.contains("/5") { self = .d(5) }
            else if str.contains("/6") { self = .d(6) }
            else if str.contains("/7") { self = .d(7) }
            else if str.contains("/r") { self = (memOpIdx ?? 1) == 1 ? .rm : .mr }
            else if hasMOfst {
                if hasReg {
                    self = (memOpIdx ?? 1) == 1 ? .rmo : .mor
                }
                else {
                    self = .i
                }
            }
            else if str.contains("+i") || str.contains("+rb") || str.contains("+rw") || str.contains("+rd") { self = .ri }
            else if str.contains("ib") || str.contains("iw") || str.contains("id") || str.contains("io") || str.contains("cb") || str.contains("cw") || str.contains("cd") || str.contains("cp") { self = .i }
            else { self = .po }
        }
        
        static var opcodeStrings: [String] {
            [
                "/0",
                "/1",
                "/2",
                "/3",
                "/4",
                "/5",
                "/6",
                "/7",
                "/r",
                "+i",
                "+rb",
                "+rw",
                "+rd"
            ]
        }
        
        var strRepr: String {
            switch self {
            case .po:
                return ".po"
            case .d(let int):
                return ".{ .d = \(int) }"
            case .i:
                return ".i"
            case .ri:
                return ".ri"
            case .rm:
                return ".rm"
            case .mr:
                return ".mr"
            case .rmo:
                return ".rmo"
            case .mor:
                return ".mor"
            }
        }
    }
    
    enum Register {
        case reg8A
        case reg16A
        case reg32A
        case reg64A
        case reg8Gp
        case reg16Gp
        case reg32Gp
        case reg64Gp
        case regDR
        case regCR
        case regST
        case regCS
        case regDS
        case regES
        case regFS
        case regGS
        case regSS
        case regXMM
        
        static var opcodeStrings: [String] {
            [
                "cb",
                "cw",
                "cd",
                "cp",
            ]
        }
        
        var strRepr: String {
            switch self {
            case .reg8A:
                return ".reg8A"
            case .reg16A:
                return ".reg16A"
            case .reg32A:
                return ".reg32A"
            case .reg64A:
                return ".reg64A"
            case .reg8Gp:
                return ".{ .reg8Gp = undefined }"
            case .reg16Gp:
                return ".{ .reg16Gp = undefined }"
            case .reg32Gp:
                return ".{ .reg32Gp = undefined }"
            case .reg64Gp:
                return ".{ .reg64Gp = undefined }"
            case .regDR:
                return ".{ .regDR = undefined }"
            case .regCR:
                return ".{ .regCR = undefined }"
            case .regST:
                return ".{ .regST = undefined }"
            case .regCS:
                return ".regCS"
            case .regDS:
                return ".regDS"
            case .regES:
                return ".regES"
            case .regFS:
                return ".regFS"
            case .regGS:
                return ".regGS"
            case .regSS:
                return ".regSS"
            case .regXMM:
                return ".{ .regXMM = undefined }"
            }
        }
        
        var size: Int {
            switch self {
            case .reg8A, .reg8Gp:
                return 8
            case .reg16A, .reg16Gp, .regCS, .regES, .regDS, .regFS, .regGS, .regSS:
                return 16
            case .reg32A, .reg32Gp, .regCR, .regDR:
                return 32
            case .reg64A, .reg64Gp:
                return 64
            case .regST:
                return 80
            case .regXMM:
                return 128
            }
        }
        
        var isGpReg: Bool {
            switch self {
            case .reg8A, .reg8Gp, .reg16A, .reg16Gp, .reg32A, .reg32Gp, .reg64A, .reg64Gp:
                return true
            default:
                return false
            }
        }
    }
    
    enum Immediate {
        case imm8, imm16, imm32, imm64
        
        static var opcodeStrings: [String] {
            [
                "ib",
                "iw",
                "id",
                "io",
            ]
        }
        
        var strRepr: String {
            switch self {
            case .imm8:
                return ".{ .imm8 = undefined }"
            case .imm16:
                return ".{ .imm16 = undefined }"
            case .imm32:
                return ".{ .imm32 = undefined }"
            case .imm64:
                return ".{ .imm64 = undefined }"
            }
        }
        
        var size: Int {
            switch self {
            case .imm8:
                return 8
            case .imm16:
                return 16
            case .imm32:
                return 32
            case .imm64:
                return 64
            }
        }
    }
    
    enum Memory {
        case mem8, mem16, mem32, mem48, mem64, mem80, mem94m108, mem14m28, mem128, mem512
        
        static var opcodeStrings: [String] {
            [
                "m8",
                "m16",
                "m32",
                "m64",
            ]
        }
        
        var strRepr: String {
            switch self {
            case .mem8:
                return ".{ .mem8 = undefined }"
            case .mem16:
                return ".{ .mem16 = undefined }"
            case .mem32:
                return ".{ .mem32 = undefined }"
            case .mem48:
                return ".{ .mem48 = undefined }"
            case .mem64:
                return ".{ .mem64 = undefined }"
            case .mem80:
                return ".{ .mem80 = undefined }"
            case .mem94m108:
                return ".{ .mem94m108 = undefined }"
            case .mem14m28:
                return ".{ .mem14m28 = undefined }"
            case .mem128:
                return ".{ .mem128 = undefined }"
            case .mem512:
                return ".{ .mem512 = undefined }"
            }
        }
        
        var size: Int {
            switch self {
            case .mem8:
                return 8
            case .mem16:
                return 16
            case .mem32:
                return 32
            case .mem48:
                return 48
            case .mem64:
                return 64
            case .mem80:
                return 80
            case .mem94m108:
                return 108
            case .mem14m28:
                return 28
            case .mem128:
                return 128
            case .mem512:
                return 512
            }
        }
    }
    
    enum MOffset {
        case mofst8, mofst16, mofst32, mofst64
        
        var strRepr: String {
            switch self {
            case .mofst8:
                return ".{ .mofst8 = undefined }"
            case .mofst16:
                return ".{ .mofst16 = undefined }"
            case .mofst32:
                return ".{ .mofst32 = undefined }"
            case .mofst64:
                return ".{ .mofst64 = undefined }"
            }
        }
        
        var size: Int {
            switch self {
            case .mofst8:
                return 8
            case .mofst16:
                return 16
            case .mofst32:
                return 32
            case .mofst64:
                return 64
            }
        }
    }
    
    enum Rel {
        case rel8, rel16, rel32
        
        var strRepr: String {
            switch self {
            case .rel8:
                return ".{ .rel8 = undefined }"
            case .rel16:
                return ".{ .rel16 = undefined }"
            case .rel32:
                return ".{ .rel32 = undefined }"
            }
        }
        
        var size: Int {
            switch self {
            case .rel8:
                return 8
            case .rel16:
                return 16
            case .rel32:
                return 32
            }
        }
    }
    
    enum Pntr {
        case pntr16_16, pntr16_32
        
        var strRepr: String {
            switch self {
            case .pntr16_16:
                return ".{ .pntr16_16 = undefined }"
            case .pntr16_32:
                return ".{ .pntr16_32 = undefined }"
            }
        }
        
        var size: Int {
            switch self {
            case .pntr16_16:
                return 32
            case .pntr16_32:
                return 48
            }
        }
    }
    
    enum RegMem {
        case reg(Register)
        case mem(Memory)
        
        var isMem: Bool {
            switch self {
            case .mem(_):
                return true
            default:
                return false
            }
        }
        
        var strRepr: String {
            switch self {
            case .reg(let register):
                return ".{ .reg = \(register.strRepr) }"
            case .mem(let memory):
                return ".{ .mem = \(memory.strRepr) }"
            }
        }
        
        var size: Int {
            switch self {
            case .reg(let register):
                return register.size
            case .mem(let memory):
                return memory.size
            }
        }
    }
    
    enum Operand {
        case reg(Register)
        case mem(Memory)
        case mofst(MOffset)
        case rel(Rel)
        case rm(RegMem)
        case imm(Immediate)
        case pntr(Pntr)
        
        init?(from str: String?) throws {
            guard let str = str else {
                return nil
            }

            if (str == "AL") { self = .reg(.reg8A) }
            else if (str == "AX") { self = .reg(.reg16A) }
            else if (str == "EAX") { self = .reg(.reg32A) }
            else if (str == "RAX") { self = .reg(.reg64A) }
            else if (str == "imm8") { self = .imm(.imm8) }
            else if (str == "imm16") { self = .imm(.imm16) }
            else if (str == "imm32") { self = .imm(.imm32) }
            else if (str == "imm64") { self = .imm(.imm64) }
            else if (str == "r8") { self = .reg(.reg8Gp) }
            else if (str == "CL") { self = .reg(.reg8Gp) }
            else if (str == "r16") { self = .reg(.reg16Gp) }
            else if (str == "r32") { self = .reg(.reg32Gp) }
            else if (str == "r64") { self = .reg(.reg64Gp) }
            else if (str == "reg") { self = .reg(.reg64Gp) }
            else if (str == "DX") { self = .reg(.reg16Gp) }
            else if (str == "CS") { self = .reg(.regCS) }
            else if (str == "DS") { self = .reg(.regDS) }
            else if (str == "ES") { self = .reg(.regES) }
            else if (str == "GS") { self = .reg(.regGS) }
            else if (str == "FS") { self = .reg(.regFS) }
            else if (str == "SS") { self = .reg(.regSS) }
            else if (str == "Sreg") { self = .reg(.regSS) }
            else if (str == "xmm" || str == "mm1" || str == "mm2") { self = .reg(.regXMM) }
            else if (str == "r/m8") { self = .rm(.mem(.mem8)) }
            else if (str == "r/m16") { self = .rm(.mem(.mem16)) }
            else if (str == "r/m32") { self = .rm(.mem(.mem32)) }
            else if (str == "r/m64") { self = .rm(.mem(.mem64)) }
            else if (str == "r16/m16") { self = .rm(.mem(.mem16)) }
            else if (str == "r32/m32") { self = .rm(.mem(.mem32)) }
            else if (str == "r64/m64") { self = .rm(.mem(.mem64)) }
            else if (str == "r32/m16") { self = .rm(.mem(.mem16)) }
            else if (str == "r64/m16") { self = .rm(.mem(.mem16)) }
            else if (str == "m8") { self = .mem(.mem8) }
            else if (str == "m16") { self = .mem(.mem16) }
            else if (str == "m32") { self = .mem(.mem32) }
            else if (str == "m64") { self = .mem(.mem64) }
            else if (str == "mm/m64" || str == "mm2/m64") { self = .mem(.mem64) }
            else if (str == "m128") { self = .mem(.mem128) }
            else if (str == "m512byte") { self = .mem(.mem512) }
            else if (str == "xmm/m64") { self = .rm(.mem(.mem64)) }
            else if (str == "xmm/m128") { self = .rm(.mem(.mem128)) }
            else if (str == "mem") { self = .mem(.mem64) }
            else if (str == "mm") { self = .mem(.mem64) }
            else if (str == "m16&32") { self = .mem(.mem32) }
            else if (str == "m16&64") { self = .mem(.mem64) }
            else if (str == "rel8") { self = .rel(.rel8) }
            else if (str == "rel16") { self = .rel(.rel16) }
            else if (str == "rel32") { self = .rel(.rel32) }
            else if (str == "moffs8") { self = .mofst(.mofst8) }
            else if (str == "moffs16") { self = .mofst(.mofst16) }
            else if (str == "moffs32") { self = .mofst(.mofst32) }
            else if (str == "moffs64") { self = .mofst(.mofst64) }
            else if (str == "m32fp") { self = .mem(.mem32) }
            else if (str == "m64fp") { self = .mem(.mem64) }
            else if (str == "m80fp") { self = .mem(.mem80) }
            else if (str == "ptr16:16") { self = .pntr(.pntr16_16) }
            else if (str == "ptr16:32") { self = .pntr(.pntr16_32) }
            else if (str == "m16&16") { self = .mem(.mem16) }
            else if (str == "m32&32") { self = .mem(.mem32) }
            else if (str == "m16:16") { self = .mem(.mem16) }
            else if (str == "m16:32") { self = .mem(.mem48) }
            else if (str == "m16:64") { self = .mem(.mem80) }
            else if (str == "m16int") { self = .mem(.mem16) }
            else if (str == "m32int") { self = .mem(.mem32) }
            else if (str == "m64int") { self = .mem(.mem64) }
            else if (str == "m80dec") { self = .mem(.mem80) }
            else if (str == "m80bcd") { self = .mem(.mem80) }
            else if (str == "m94/108byte") { self = .mem(.mem94m108) }
            else if (str == "m2byte") { self = .mem(.mem16) }
            else if (str == "m14/28byte") { self = .mem(.mem14m28) }
            else if (str == "ST(i)" || str == "ST(0)" || str == "ST(1)" || str == "ST") { self = .reg(.regST) }
            else if (str == "CR0-CR7" || str == "CR8") { self = .reg(.regCR) }
            else if (str == "DR0-DR7") { self = .reg(.regCR) }
            else if (str == "3" || str == "0" || str == "1") { return nil } // "INT 3"
            else if (str == "m") { self = .mem(.mem32) } // "INT 3"
            else { throw NSError(domain: "com.GtenIOS.X64InstrBuilder.OperandParser", code: -1001, userInfo: [NSLocalizedDescriptionKey: "Unrecognised operand: \(dump(str))"])}
        }
        
        var isMem: Bool {
            switch self {
            case .mofst(_), .mem(_):
                return true
            case .rm(let rm):
                return rm.isMem
            default:
                return false
            }
        }
        
        var isReg: Bool {
            switch self {
            case .reg(_):
                return true
            default:
                return false
            }
        }
        
        var isMofst: Bool {
            switch self {
            case .mofst(_):
                return true
            default:
                return false
            }
        }
        
        var strRepr: String {
            switch self {
            case .reg(let register):
                return ".{ .reg = \(register.strRepr) }"
            case .mem(let memory):
                return ".{ .mem = \(memory.strRepr) }"
            case .mofst(let mOffset):
                return ".{ .mofst = \(mOffset.strRepr) }"
            case .rel(let rel):
                return ".{ .rel = \(rel.strRepr) }"
            case .rm(let regMem):
                return ".{ .rm = \(regMem.strRepr) }"
            case .imm(let immediate):
                return ".{ .imm = \(immediate.strRepr) }"
            case .pntr(let pntr):
                return ".{ .pntr = \(pntr.strRepr) }"
            }
        }
        
        var size: Int {
            switch self {
            case .reg(let register):
                return register.size
            case .mem(let memory):
                return memory.size
            case .mofst(let mOffset):
                return mOffset.size
            case .rel(let rel):
                return rel.size
            case .rm(let regMem):
                return regMem.size
            case .imm(let immediate):
                return immediate.size
            case .pntr(let pntr):
                return pntr.size
            }
        }
    }
    
    struct Instr {
        let mnemonic: String
        let operand1: Operand?
        let operand2: Operand?
        let operand3: Operand?
        let operand4: Operand?
        
        let operandsCount: Int
        init(from str: String) throws {
            let str = str.trimmingCharacters(in: .whitespaces)
            if let space_idx = str.firstIndex(of: " ") {
                self.mnemonic = String(str[str.startIndex...space_idx])
                var comp = String(str[space_idx..<str.endIndex]).replacingOccurrences(of: " ", with: "").components(separatedBy: ",")
                comp = comp.reversed()
                self.operand1 = try Operand(from: comp.popLast())
                self.operand2 = try Operand(from: comp.popLast())
                self.operand3 = try Operand(from: comp.popLast())
                self.operand4 = try Operand(from: comp.popLast())
            }
            else {
                self.mnemonic = str
                self.operand1 = nil
                self.operand2 = nil
                self.operand3 = nil
                self.operand4 = nil
            }
            
            var count = 0;
            if self.operand1 != nil { count += 1 }
            if self.operand2 != nil { count += 1 }
            if self.operand3 != nil { count += 1 }
            if self.operand4 != nil { count += 1 }
            self.operandsCount = count
        }
        
        var hasNoOperand: Bool {
            operandsCount == 0
        }
        
        var hasMOfst: Bool {
            guard operandsCount > 0 else { return false }
            if operand1?.isMofst ?? false { return true }
            else if operand2?.isMofst ?? false { return true }
            else if operand3?.isMofst ?? false { return true }
            else if operand4?.isMofst ?? false { return true }
            else { return false }
        }
        
        var hasReg: Bool {
            guard operandsCount > 0 else { return false }
            if operand1?.isReg ?? false { return true }
            else if operand2?.isReg ?? false { return true }
            else if operand3?.isReg ?? false { return true }
            else if operand4?.isReg ?? false { return true }
            else { return false }
        }
        
        var memIdx: Int? {
            guard operandsCount > 0 else { return nil }
            if operand1?.isMem ?? false { return 0 }
            else if operand2?.isMem ?? false { return 1 }
            else if operand3?.isMem ?? false { return 2 }
            else if operand4?.isMem ?? false { return 3 }
            else { return nil }
        }
        
        var operands: [Operand] {
            var result: [Operand] = []
            if let operand1 = operand1 { result.append(operand1) }
            if let operand2 = operand2 { result.append(operand2) }
            if let operand3 = operand3 { result.append(operand3) }
            if let operand4 = operand4 { result.append(operand4) }
            
            return result
        }
        
        var opStrRepr: String {
            let opers = operands
            guard !opers.isEmpty else { return "[0]Operand{}" }
            var resultStr = "[\(opers.count)]Operand{\n            "
            resultStr += opers.compactMap({ $0.strRepr }).joined(separator: ",\n            ")
            resultStr += "\n        }"
            
            return resultStr
        }
        
        var regOpnd: Register? {
            for operand in operands {
                switch operand {
                case .reg(let register):
                    return register
                default:
                    continue
                }
            }
            
            return nil
        }
        
        var immOpnd: Immediate? {
            for operand in operands {
                switch operand {
                case .imm(let immediate):
                    return immediate
                default:
                    continue
                }
            }
            
            return nil
        }
        
        var is16Bit: Bool {
            if let regOpnd = regOpnd {
                return regOpnd.size == 16
            }
            else if let immOpnd = immOpnd {
                return immOpnd.size == 16
            }
            else { return false }
        }
        
        var is32Bit: Bool {
            if let regOpnd = regOpnd {
                return regOpnd.size == 32
            }
            else if let immOpnd = immOpnd {
                return immOpnd.size == 32
            }
            else { return false }
        }
        
        var is64Bit: Bool {
            if let regOpnd = regOpnd {
                return regOpnd.size == 64
            }
            else if let immOpnd = immOpnd {
                return immOpnd.size == 64
            }
            else { return false }
        }
        
        var hasGPReg: Bool {
            if let regOpnd = regOpnd {
                return regOpnd.isGpReg
            }
            else { return false }
        }
        
        var hasImmOpnd: Bool {
            if let _ = immOpnd { return true }
            else { return false }
        }
        
        var operatingSize: Int {
            operand1?.size ?? 32
        }
        
        var validPrefixes: [Prefix] {
            var result: [Prefix] = []
            if Prefix.lockMnemonics.contains(mnemonic.trimmingCharacters(in: .whitespacesAndNewlines)) {
                result.append(Prefix.lock)
            }
            if Prefix.repMnemonics.contains(mnemonic.trimmingCharacters(in: .whitespacesAndNewlines)) {
                result.append(Prefix.repe)
            }
            if Prefix.repeMnemonics.contains(mnemonic.trimmingCharacters(in: .whitespacesAndNewlines)) {
                result.append(Prefix.repe)
            }
            if Prefix.repneMnemonics.contains(mnemonic.trimmingCharacters(in: .whitespacesAndNewlines)) {
                result.append(Prefix.repne)
            }
            return result
        }
    }
    
    struct OpCode {
        let po: [String]
        let prefix: Prefix?
        let encoding: Instruction.Encoding
        
        init(from str: String, memOpIdx: Int?, hasMOfst: Bool, hasReg: Bool) {
            self.encoding = .init(from: str, memOpIdx: memOpIdx, hasMOfst: hasMOfst, hasReg: hasReg)
            var str = str
            if str.contains("REX.W") { self.prefix = .rexW }
            else if str.contains("REX.w") { self.prefix = .rexW }
            else if str.contains("REX.R") { self.prefix = .rexR }
            else if str.contains("REX.r") { self.prefix = .rexR }
            else if str.contains("REX.X") { self.prefix = .rexX }
            else if str.contains("REX.x") { self.prefix = .rexX }
            else if str.contains("REX.B") { self.prefix = .rexB }
            else if str.contains("REX.b") { self.prefix = .rexB }
            else if str.contains("REX") { self.prefix = .rex }
            else { self.prefix = nil }
            
            str = str.removePrefixes
            
            str = str.removeImmediates
            
            str = str.removeMemory
            
            str = str.removeEncodings
            
            str = str.removeControlRegs
            
            str = str.replacingOccurrences(of: "NP", with: "")
            
            str = str.replacingOccurrences(of: "+", with: "")
            str = str.trimmingCharacters(in: .whitespaces)
            self.po = str.components(separatedBy: " ")
        }
        
        var opCodeStrRepr: String {
            "&[_]u8{\(po.filter({ !$0.trimmingCharacters(in: .whitespaces).isEmpty }).compactMap({ "0x\($0)" }).joined(separator: ", "))}"
        }
    }
    
    let instr: Instr
    let opcode: OpCode
    let valid64: Bool
    let valid32: Bool
    let valid16: Bool
    let featureFlags: String
    let operand1: String?
    let operand2: String?
    let operand3: String?
    let operand4: String?
    
    init(from dict: [String: String]) throws {
        self.instr = try Instr(from: dict["Instruction"]!)
        self.opcode = OpCode(from: dict["Opcode"]!, memOpIdx: instr.memIdx, hasMOfst: instr.hasMOfst, hasReg: instr.hasReg)
        self.valid64 = dict["Valid 64-bit"]! == "Valid"
        self.valid32 = dict["Valid 32-bit"]! == "Valid"
        self.valid16 = dict["Valid 16-bit"]! == "Valid"
        self.featureFlags = dict["Feature Flags"]!
        
        let operand1 = dict["Operand 1"]!
        self.operand1 = operand1 == "NA" ? nil : operand1
        let operand2 = dict["Operand 2"]!
        self.operand2 = operand2 == "NA" ? nil : operand2
        let operand3 = dict["Operand 3"]!
        self.operand3 = operand3 == "NA" ? nil : operand3
        let operand4 = dict["Operand 4"]!
        self.operand4 = operand4 == "NA" ? nil : operand4
    }
    
    var isValid: Bool {
        valid16 || valid32 || valid64
    }
    
    var opModeStrRepr: String {
        var result = ""
        if valid16 {
            result += "@enumToInt(OperatingMode.Bits16)"
        }
        if valid32 {
            if result != "" { result += " | " }
            result += "@enumToInt(OperatingMode.Bits32)"
        }
        if valid64 {
            if result != "" { result += " | " }
            result += "@enumToInt(OperatingMode.Bits64)"
        }
        return result
    }
    
    var prefix32BitStrRepr: String {
        if instr.is16Bit && (instr.hasGPReg || instr.hasImmOpnd) {
            return "&[_]Prefix{\(Prefix.opOvrrd.strRepr)}"
        }
        else { return "null" }
    }
    
    var prefix64BitStrRepr: String {
        if instr.is16Bit && (instr.hasGPReg || instr.hasImmOpnd) {
            return "&[_]Prefix{\(Prefix.opOvrrd.strRepr)}"
        }
        else if let prefix = opcode.prefix {
            return "&[_]Prefix{\(prefix.strRepr)}"
        }
        else { return "null" }
    }
    
    var encoding: Self.Encoding {
        return opcode.encoding
    }
    
    var strRepr: String {
        let opndCount = instr.operands.count
        return "Instruction{ .instr\(opndCount) = Instr(\(opndCount)).init(\"\(instr.mnemonic.lowercased().trimmingCharacters(in: .whitespaces))\", \(opcode.opCodeStrRepr), \(opcode.encoding.strRepr), \n        \(instr.opStrRepr), \n        \(opModeStrRepr), \n        &[_]Prefix{ \(instr.validPrefixes.compactMap({ $0.strRepr }).joined(separator: ", ")) }, \(instr.operatingSize), \(prefix32BitStrRepr), \(prefix64BitStrRepr)) \n    }"
    }
}

if let opcodesCSV = try? String(contentsOfFile: "../../res/x64.csv") {
    var lines: [String] = opcodesCSV.components(separatedBy: "\n")
    let header = lines.removeFirst()    // Remove header
    let headerComponents: [String] = header.components(separatedBy: ",")
    var opcodesDictionary: [[String: String]] = []
    for line in lines {
        let components: [String] = line.toCommaSeparatedComponents

        guard components.count == headerComponents.count else {
            print("Mismatch comp: \(components.count) against \(headerComponents.count)\n\(line)")
            components.forEach({ print($0) })
            continue
        }
        var opcodeDict: [String: String] = [:]
        for i in 0..<headerComponents.count {
            opcodeDict[headerComponents[i]] = components[i].replacingOccurrences(of: "\"", with: "")
        }
        
        guard opcodeDict["Feature Flags"]! == "" || opcodeDict["Feature Flags"] == nil else { continue }
        opcodesDictionary.append(opcodeDict)
    }

    print("Found \(opcodesDictionary.count) opcodes")
    do {
        let instructions = try opcodesDictionary.map({ try Instruction(from: $0) }).filter({ $0.isValid })
        var resultString: String = """
        // This file is autogenerated using `X64InstrBuilder` swift project. Do not edit this file!!
        
        const instr = @import("instr.zig");
        const Instruction = instr.Instruction;
        const Instr = instr.Instr;
        const Operand = @import("opnd.zig").Operand;
        const RegGpIdx = @import("reg.zig").RegGpIdx;
        const Prefix = @import("pfx.zig").Prefix;
        const OperandEncoding = @import("opnd.zig").OperandEncoding;
        const OperatingMode = @import("common").OperatingMode;\n\n
        """
//        var i = 0
//        let instrLimit = 500
//        var startIdx = i * instrLimit
//        var endIdx = min(startIdx + instrLimit, instructions.count)
//        while startIdx < instructions.count {
//            let instrs = instructions[startIdx..<endIdx]
//            print("const x64_instrs\(i + 1) = [_]Instruction{\n    \(instrs.compactMap({ $0.strRepr }).joined(separator: ",\n    "))\n};\n")
//            i += 1;
//            startIdx = i * instrLimit
//            endIdx = min(startIdx + instrLimit, instructions.count)
//        }
        let aAsciiVal: Int = Int(Character("a").asciiValue!)
        var instructionsStrArray = [[[String]]](
            repeating: [[String]](repeating: [], count: 5),
            count: Int(Character("z").asciiValue!) - aAsciiVal + 1
        )
        instructions.forEach({
            let mnemIdx = Int($0.instr.mnemonic.lowercased().first!.asciiValue!) - aAsciiVal
            var mnemInstrs = instructionsStrArray[mnemIdx]
            mnemInstrs[$0.instr.operandsCount].append($0.strRepr)
            instructionsStrArray[mnemIdx] = mnemInstrs
        })
        
        let alphabets = Array("abcdefghijklmnopqrstuvwxyz");
        for i in 0..<instructionsStrArray.count {
            for j in 0..<instructionsStrArray[i].count {
                resultString += String(format: "const instrs%@%d = [_]Instruction{\n    %@\n};\n", String(alphabets[i]), j, instructionsStrArray[i][j].joined(separator: ",\n    "))
            }
            resultString += String(format: "const instrs%@ = [_][]const Instruction{\n    %@\n};\n\n", String(alphabets[i]), (0..<instructionsStrArray[i].count).compactMap({ String(format: "&instrs%@%d", String(alphabets[i]), $0) }).joined(separator: ", "))
        }
        resultString += "pub const instrs = [_][]const []const Instruction{ \(alphabets.compactMap({ "&instrs\(String($0))" }).joined(separator: ", ")) };"
        if FileManager.default.fileExists(atPath: "../../out/isa.zig") {
            try FileManager.default.removeItem(atPath: "../../out/isa.zig")
        }
        FileManager.default.createFile(atPath: "../../out/isa.zig", contents: resultString.data(using: .utf8), attributes: nil)
        
        #if DEBUG
        print(resultString)
        #endif
    } catch {
        print(error)
    }
}

// MARK: - Extensions
extension String {
    var removePrefixes: Self {
        var result = self
        Instruction.Prefix.strings.forEach{ result = result.replacingOccurrences(of: $0, with: "") }
        return result
    }
    
    var removeImmediates: Self {
        var result = self
        Instruction.Immediate.opcodeStrings.forEach{ result = result.replacingOccurrences(of: $0, with: "") }
        return result
    }
    
    var removeMemory: Self {
        var result = self
        Instruction.Memory.opcodeStrings.forEach{ result = result.replacingOccurrences(of: $0, with: "") }
        return result
    }
    
    var removeEncodings: Self {
        var result = self
        Instruction.Encoding.opcodeStrings.forEach{ result = result.replacingOccurrences(of: $0, with: "") }
        return result
    }
    
    var removeControlRegs: Self {
        var result = self
        Instruction.Register.opcodeStrings.forEach{ result = result.replacingOccurrences(of: $0, with: "") }
        return result
    }
    
    // Reference: https://stackoverflow.com/questions/43295163/swift-3-1-how-to-get-array-or-dictionary-from-csv
    var toCommaSeparatedComponents: [String] {
        var values: [String] = []
        if self != "" {
            if self.range(of: "\"") != nil {
                var textToScan:String = self
                var value:String?
                var textScanner:Scanner = Scanner(string: textToScan)
                while !textScanner.isAtEnd {
                    if (textScanner.string as NSString).substring(to: 1) == "\"" {
                        textScanner.currentIndex = textScanner.string.index(after: textScanner.currentIndex)
                        value = textScanner.scanUpToString("\"")
                        textScanner.currentIndex = textScanner.string.index(after: textScanner.currentIndex)
                    } else {
                        value = textScanner.scanUpToString(",")
                    }
                    if let value = value {
                        values.append(value)
                    }
                    else {
                        values.append("")
                    }

                    if !textScanner.isAtEnd{
                        let indexPlusOne = textScanner.string.index(after: textScanner.currentIndex)
                        textToScan = String(textScanner.string[indexPlusOne...])
                    } else {
                        textToScan = ""
                    }
                    textScanner = Scanner(string: textToScan)
               }

               // For a line without double quotes, we can simply separate the string
               // by using the delimiter (e.g. comma)
           } else  {
               values = self.components(separatedBy: ",")
           }
        }
        return values
    }
}
