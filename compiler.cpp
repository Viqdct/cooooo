
#include "compiler.h"

#include <set>
#include <cstdlib>

// static const std::set<std::string> BUILTIN_FUNCS {
//     "getint",
//     "getdouble",
//     "getchar",
//     "putint",
//     "putdouble",
//     "putchar",
//     "putstr",
//     "putln",
// };

uint64_t ToBigEndian64(uint64_t x) {
    uint64_t y = x;
    y = ((y & 0x00000000ffffffffull) << 32) | (y >> 32);
    y = ((y & 0x0000ffff0000ffffull) << 16) | ((y & 0xffff0000ffff0000ull) >> 16);
    y = ((y & 0x00ff00ff00ff00ffull) << 8) | ((y & 0xff00ff00ff00ff00ull) >> 8);
    return y;
}

uint32_t ToBigEndian32(uint32_t x) {
    uint32_t y = x;
    y = ((y & 0x0000fffful) << 16) | (y >> 16);
    y = ((y & 0xff00ff00fful) << 8) | ((y & 0xff00ff00ul) >> 8);
    return y;
}

void Instruction::PackInt32Param(int32_t x) {
    param = ToBigEndian32(static_cast<uint32_t>(x));
    param_size = 32;
}

void Instruction::PackUint32Param(uint32_t x) {
    param_size = 32;
    param = ToBigEndian32(x);
}

void Instruction::PackUint64Param(int64_t x) {
    param_size = 64;
    param = x;
}

void ProgramBinary::AddGlobalVar(const std::string &name, VarType type) {
    GlobalDef def;
    def.value.resize(4);

    Variable var;
    var.offset = globals.size();
    var.scope = kGlobal;
    var.type = type;

    global_vars.emplace(name, var);

    globals.push_back(std::move(def));
}

void ProgramBinary::AddGlobalFuncName(const std::string &func_name) {
    GlobalDef def;
    
    def.value.resize(func_name.size());
    for (int i = 0; i < func_name.size(); ++i)
        def.value[i] = func_name[i];

    globals.push_back(std::move(def));
}

void ProgramBinary::AddFuncDef(const std::string &func_name, Ptr<FuncDef> func) {
    Function fn;
    fn.def = func.get();
    fn.offset = functions.size();
    fn.has_return = func->return_slots > 0;

    functions.push_back(std::move(func));
    function_map.emplace(func_name, fn);

    fn.def->name = globals.size();
    AddGlobalFuncName(func_name);
}

void FuncDef::CalculateJmpOffset() {
    num_insts = 0;

    for (const auto &block : body) {
        block->offset = num_insts;
        num_insts += block->instructions.size();
    }

    for (const auto &block : body) {
        if (block->br) {
            BasicBlock *br = block->br;
            block->instructions.back().PackInt32Param(br->offset - block->offset - 1);
        }
    }
}

void FuncDef::AddLocalVar(const std::string &name, VarType type, VarScope scope) {
    Variable var;
    var.type = type;

    if (scope == kLocal) {
        var.offset = loc_slots;
        ++loc_slots;
    } else {
        var.offset = return_slots + param_slots;
        ++param_slots;
    }
    local_vars.emplace(name, var);

}

Compiler::Compiler(std::ostream &out) : out_(out) {
}

void Compiler::Compile(ProgramNode *program) {
    phase_ = kVarAlloc;
    program->Accept(*this);
    phase_ = kCodeGen;
    program->Accept(*this);
    GenerateCode();
}

void Compiler::WriteByte(uint8_t x) {
    out_.write(reinterpret_cast<char *>(&x), sizeof(x));
}

void Compiler::WriteLit32(uint32_t value) {
    value = ToBigEndian32(value);
    out_.write(reinterpret_cast<char *>(&value), sizeof(value));
}

void Compiler::WriteLit64(uint64_t value) {
    value = ToBigEndian64(value);
    out_.write(reinterpret_cast<char *>(&value), sizeof(value));
}

// templateWriteArray

void Compiler::GenerateCode() {
    WriteLit32(0x72303b3eul);
    WriteLit32(0x1ul);
    WriteLit32(program_.globals.size());

    for (const auto &global : program_.globals) {
        WriteByte(global.is_const);
        WriteLit32(global.value.size());
        for (uint8_t byte : global.value) {
            WriteByte(byte);
        }
    }

    WriteLit32(program_.functions.size());

    for (const auto &func : program_.functions) {
        WriteLit32(func->name);
        WriteLit32(func->return_slots);
        WriteLit32(func->param_slots);
        WriteLit32(func->loc_slots);
        WriteLit32(func->num_insts);

        for (const auto &block : func->body) {
            for (const auto &inst : block->instructions) {
                WriteByte(inst.opcode);
                if (inst.param_size == 32) {
                    WriteLit32(inst.param);
                } else {
                    WriteLit64(inst.param);
                }
            }            
        }
    }
}

void Compiler::GenCondBody(CondBody &cond_body, BasicBlock *next, BasicBlock *end) {
    cond_body.condition->Accept(*this);
    GenCodeU32(kOpCodeBrFalse, 0);
    codes_->br = next;

    CreateNewCodeBlock();
    cond_body.body->Accept(*this);
    GenCodeU32(kOpCodeBr, 0);
    codes_->br = end;
}

void Compiler::CreateNewCodeBlock() {
    if (codes_)
        func_->body.push_back(std::move(codes_));
    codes_ = MakePtr<BasicBlock>();
}

void Compiler::AddStartFunc() {
    auto func = MakePtr<FuncDef>();
    program_.AddFuncDef("_start", std::move(func));
}

void Compiler::GenStartFunc(ProgramNode *node) {
    codes_ = MakePtr<BasicBlock>();

    for (const auto &var : node->global_vars) {
        if (!var->initializer)
            continue;

        AssignToVar(var->name, var->initializer.get());
    }

    auto func = program_.function_map.at("_start").def;
    func->body.push_back(std::move(codes_));
}

void Compiler::Visit(ProgramNode *node) {
    if (phase_ == kVarAlloc) {
        for (const auto &var : node->global_vars) {
            program_.AddGlobalVar(var->name, var->type);
        }

        for (const auto &func : node->functions) {
            func->Accept(*this);
        }
        AddStartFunc();
    } else {
        GenStartFunc(node);
        for (const auto &func : node->functions) {
            func->Accept(*this);
        }
    }
}

void Compiler::Visit(ExprStmtNode *node) {
    if (phase_ != kCodeGen)
        return;
    node->expr->Accept(*this);
}

void Compiler::Visit(DeclStmtNode *node) {
    if (phase_ == kVarAlloc) {
        if (func_) {
            func_->AddLocalVar(node->name, node->type, kLocal);
        }
    } else {
        if (node->initializer) {
            AssignToVar(node->name, node->initializer.get());
        }
    }
}

void Compiler::Visit(IfStmtNode *node) {
    if (phase_ != kCodeGen)
        return;

    auto next = MakePtr<BasicBlock>();
    auto end = MakePtr<BasicBlock>();

    GenCondBody(node->if_part, next.get(), end.get());

    for (auto &cond_body : node->elif_part) {
        func_->body.push_back(std::move(codes_));
        codes_ = std::move(next);
        next = MakePtr<BasicBlock>();
        GenCondBody(cond_body, next.get(), end.get());
    }

    func_->body.push_back(std::move(codes_));
    codes_ = std::move(next);

    if (node->else_part) {
        node->else_part->Accept(*this);
    }

    func_->body.push_back(std::move(codes_));
    codes_ = std::move(end);
}

void Compiler::Visit(WhileStmtNode *node) {
    if (phase_ != kCodeGen)
        return;

    CreateNewCodeBlock();
    auto cond_block = codes_.get();
    node->condition->Accept(*this);
    GenCodeU32(kOpCodeBrFalse, 0);

    CreateNewCodeBlock();
    node->body->Accept(*this);
    GenCodeU32(kOpCodeBr, 0);
    codes_->br = cond_block;

    CreateNewCodeBlock();
    cond_block->br = codes_.get();
}

void Compiler::Visit(ReturnStmtNode *node) {
    if (phase_ != kCodeGen)
        return;

    if (node->expr) {
        GenCodeU32(kOpCodeArga, 0);
        StoreExpr(node->expr.get());
    }
    Ret();
}

void Compiler::Visit(BlockStmtNode *node) {
    if (phase_ != kCodeGen)
        return;
    for (const auto &stmt : node->statements) {
        stmt->Accept(*this);
    }
}

void Compiler::Visit(OperatorExprNode *node) {
    node->left->Accept(*this);
    node->right->Accept(*this);

    switch (node->op) {
    case kMul:
        Mul(node->type.type);
        break;
    case kDiv:
        Div(node->type.type);
        break;
    case kMinus:
        Sub(node->type.type);
        break;
    case kPlus:
        Add(node->type.type);
        break;
    case kGt:
        Gt(node->type.type);
        break;
    case kLt:
        Lt(node->type.type);
        break;
    case kGe:
        Ge(node->type.type);
        break;
    case kLe:
        Le(node->type.type);
        break;
    case kEq:
        Eq(node->type.type);
        break;
    case kNeq:
        Neq(node->type.type);
        break;
    default:
        break;
    }
}

void Compiler::Visit(NegateExpr *node) {
    node->operand->Accept(*this);
    if (node->type.type == kInt) {
        GenCode(kOpCodeNegI);
    } else {
        GenCode(kOpCodeNegF);
    }
}

void Compiler::Visit(AssignExprNode *node) {
    AssignToVar(node->lhs, node->rhs.get());
}

void Compiler::Visit(CallExprNode *node) {
    const Function &func = program_.function_map.at(node->func_name);
    if (func.has_return) {
        StackAlloc(1 + node->args.size());
    } else {
        StackAlloc(node->args.size());
    }

    if (func.def == nullptr) {
        GenCodeU32(kOpCodeCallname, func.offset);
    } else {
        GenCodeU32(kOpCodeCall, func.offset);
    }
}

void Compiler::Visit(LiteralExprNode *node) {
    if (node->type.type == kInt) {
        PushInt(strtoll(node->lexeme.c_str(), nullptr, 10));
    } else {
        PushDouble(strtod(node->lexeme.c_str(), nullptr));
    }
}

void Compiler::Visit(IdentExprNode *node) {
    PushVarAddr(node->var_name);
    GenCode(kOpCodeLoad64);
}

void Compiler::Visit(FuncDefNode *node) {
    if (phase_ == kVarAlloc) {
        auto func = MakePtr<FuncDef>();
        func_ = func.get();

        // Handle return value.
        if (node->return_type != kVoid) {
            func->return_slots = 1;
        }

        // Handle parameters.
        for (const auto &param : node->params) {
            func->AddLocalVar(param->name, param->type, kParam);
        }

        // Allocate space for local variables.
        for (const auto &stmt : node->body->statements) {
            stmt->Accept(*this);
        }

        program_.AddFuncDef(node->name, std::move(func));
        func_ = nullptr;
    } else {
        const Function &func = program_.function_map.at(node->name);
        func_ = func.def;
        codes_ = MakePtr<BasicBlock>();
        node->body->Accept(*this);

        if (codes_->instructions.empty() || codes_->instructions.back().opcode != kOpCodeRet) {
            GenCode(kOpCodeRet);
        }

        func_->body.push_back(std::move(codes_));

        func_ = nullptr;
    }
}


const Variable &Compiler::LookUpVar(const std::string &name) {
    if (func_) {
        auto it = func_->local_vars.find(name);
        if (it != func_->local_vars.end()) {
            return it->second;
        }
    }

    return program_.global_vars.at(name);
}

void Compiler::PushInt(int64_t x) {
    GenCodeU64(kOpCodePush, static_cast<uint64_t>(x));
}

void Compiler::PushDouble(double x) {
    uint64_t v = *reinterpret_cast<uint64_t*>(&x);
    GenCodeU64(kOpCodePush, v);
}

void Compiler::PushVarAddr(const std::string &name) {
    const Variable &var = LookUpVar(name);
    if (var.scope == kLocal) {
        GenCodeU32(kOpCodeLoca, var.offset);
    } else if (var.scope == kGlobal) {
        GenCodeU32(kOpCodeGloba, var.offset);
    } else {
        // This is a function parameter.
        GenCodeU32(kOpCodeArga, var.offset);
    }
}

void Compiler::AssignToVar(const std::string &name, ExprNode *expr) {
    PushVarAddr(name);
    StoreExpr(expr);
}

void Compiler::StoreExpr(ExprNode *expr) {
    expr->Accept(*this);
    GenCode(kOpCodeStore64);
}

void Compiler::Ret() {
    GenCode(kOpCodeRet);
}

void Compiler::GenCode(OpCode opcode) {
    Instruction inst;
    inst.opcode = opcode;
    codes_->instructions.push_back(inst);
}

void Compiler::GenCodeI32(OpCode opcode, int32_t x) {
    Instruction inst;
    inst.opcode = opcode;
    inst.PackInt32Param(x);
    inst.param_size = 32;
    codes_->instructions.push_back(inst);
}

void Compiler::GenCodeU32(OpCode opcode, uint32_t x) {
    Instruction inst;
    inst.opcode = opcode;
    inst.PackUint32Param(x);
    inst.param_size = 32;
    codes_->instructions.push_back(inst);
}

void Compiler::GenCodeU64(OpCode opcode, uint64_t x) {
    Instruction inst;
    inst.opcode = opcode;
    inst.PackUint64Param(x);
    inst.param_size = 64;
    codes_->instructions.push_back(inst);
}

void Compiler::Add(VarType type) {
    if (type == kInt) {
        GenCode(kOpCodeAddI);
    } else {
        GenCode(kOpCodeAddF);
    }
}

void Compiler::Sub(VarType type) {
    if (type == kInt) {
        GenCode(kOpCodeSubI);
    } else {
        GenCode(kOpCodeSubF);
    }
}

void Compiler::Mul(VarType type) {
    if (type == kInt) {
        GenCode(kOpCodeMulI);
    } else {
        GenCode(kOpCodeMulF);
    }
}

void Compiler::Div(VarType type) {
    if (type == kInt) {
        GenCode(kOpCodeDivI);
    } else {
        GenCode(kOpCodeDivF);
    }
}

void Compiler::Lt(VarType type) {
    Compare(type);

    GenCode(kOpCodeSetLt);
}

void Compiler::Le(VarType type) {
    Compare(type);

    GenCode(kOpCodeSetGt);
    GenCode(kOpCodeNot);
}

void Compiler::Gt(VarType type) {
    Compare(type);

    GenCode(kOpCodeSetGt);
}

void Compiler::Ge(VarType type) {
    Compare(type);

    GenCode(kOpCodeSetLt);
    GenCode(kOpCodeNot);
}

void Compiler::Eq(VarType type) {
    Compare(type);

    GenCode(kOpCodeNot);
}

void Compiler::Neq(VarType type) {
    Compare(type);
}

void Compiler::Compare(VarType type) {
    if (type == kInt) {
        GenCode(kOpCodeCmpI);
    } else {
        GenCode(kOpCodeCmpF);
    }
}

void Compiler::StackAlloc(uint32_t n) {
    GenCodeU32(kOpCodeStackalloc, n);
}
