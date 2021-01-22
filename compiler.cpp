
#include "compiler.h"

#include <set>
#include <cstdlib>

static const std::map<std::string, FunctionDecl> BUILTIN_FUNCS {
    {"getint", {0, 1, 0}},
    {"getdouble", {0, 1, 0}},
    {"getchar", {0, 1, 0}},
    {"putint", {0, 0, 1}},
    {"putdouble", {0, 0, 1}},
    {"putchar", {0, 0, 1}},
    // {"putstr", {0, 0, 1}},
    {"putln", {0, 0, 0}},
};

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
    param = static_cast<uint32_t>(x);
    param_size = 32;
}

void Instruction::PackUint32Param(uint32_t x) {
    param_size = 32;
    param = x;
}

void Instruction::PackUint64Param(int64_t x) {
    param_size = 64;
    param = x;
}

void ProgramBinary::AddGlobalVar(const std::string &name, VarType type) {
    GlobalDef def;
    def.value.resize(8);

    Variable var;
    var.offset = globals.size();
    var.scope = kGlobal;
    var.type = type;

    global_vars.emplace(name, var);

    globals.push_back(std::move(def));
}

void ProgramBinary::AddFuncName(const std::string &func_name) {
    GlobalDef def;
    
    def.value.resize(func_name.size());
    for (int i = 0; i < func_name.size(); ++i)
        def.value[i] = func_name[i];

    globals.push_back(std::move(def));
}

void ProgramBinary::AddFuncDecl(const std::string &func_name, Ptr<FunctionDecl> func) {
    function_map.emplace(func_name, func.get());

    func->name = globals.size();
    AddFuncName(func_name);
    function_decls.push_back(std::move(func));
}

FuncDef *ProgramBinary::AddFuncDeclWithDef(const std::string &func_name, Ptr<FunctionDecl> func_decl) {
    auto func_def = MakePtr<FuncDef>();
    func_def->index = function_defs.size();
    func_def->decl = func_decl.get();
    func_decl->def = func_def.get();

    FuncDef *p = func_def.get();

    function_defs.push_back(std::move(func_def));

    AddFuncDecl(func_name, std::move(func_decl));

    return p;
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
            block->instructions.back().PackInt32Param(br->offset - (block->offset + block->instructions.size()));
        }
    }
}

Compiler::Compiler(std::ostream &out) : out_(out) {
}

void Compiler::Compile(ProgramNode *program) {
    program->Accept(*this);
    for (const auto &func : program_.function_defs) {
        func->CalculateJmpOffset();
    }
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

    WriteLit32(program_.function_defs.size());

    for (int i = 0; i < program_.function_defs.size(); ++i) {
        const auto &func = program_.function_defs[i];


        WriteLit32(func->decl->name);
        WriteLit32(func->decl->return_slots);
        WriteLit32(func->decl->param_slots);
        WriteLit32(func->loc_slots);

        WriteLit32(func->num_insts);

        for (const auto &block : func->body) {
            for (const auto &inst : block->instructions) {
                WriteByte(inst.opcode);
                if (inst.param_size == 32) {
                    WriteLit32(inst.param);
                } else if (inst.param_size == 64) {
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
    auto func_decl = MakePtr<FunctionDecl>();
    program_.AddFuncDeclWithDef("_start", std::move(func_decl));
}

void Compiler::GenStartFunc(ProgramNode *node) {
    codes_ = MakePtr<BasicBlock>();
    FuncDef *func = LookUpFunction("_start")->def;

    for (const auto &var : node->global_vars) {
        if (!var->initializer)
            continue;

        AssignToVar(var->name, var->initializer.get());
    }

    FunctionDecl *main_func = LookUpFunction("main");

    if (main_func->return_slots > 0) {
        GenCodeU32(kOpCodeStackalloc, 1);
    } else {
        GenCodeU32(kOpCodeStackalloc, 0);
    }

    GenCodeU32(kOpCodeCall, main_func->def->index);

    if (main_func->return_slots > 0) {
        GenCode(kOpCodePop);
    }

    func->body.push_back(std::move(codes_));
}

void Compiler::Visit(ProgramNode *node) {
        for (const auto &var : node->global_vars) {
            program_.AddGlobalVar(var->name, var->type);
        }

        AddStartFunc();
        for (const auto &func : node->functions) {
            AddFunction(func.get());
        }

        // Generate code.
        GenStartFunc(node);
        for (const auto &func : node->functions) {
            func->Accept(*this);
        }
}

void Compiler::Visit(ExprStmtNode *node) {
    node->expr->Accept(*this);
}

void Compiler::Visit(DeclStmtNode *node) {
    AddLocalVar(node->name, node->type);

    if (node->initializer) {
        AssignToVar(node->name, node->initializer.get());
    }
}

void Compiler::Visit(IfStmtNode *node) {
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
    if (node->expr) {
        GenCodeU32(kOpCodeArga, 0);
        StoreExpr(node->expr.get());
    }
    Ret();
}

void Compiler::Visit(BlockStmtNode *node) {
    if (!node->is_func_body)
        EnterScope();

    for (const auto &stmt : node->statements) {
        stmt->Accept(*this);
    }

    if (!node->is_func_body)
        LeaveScope();
}

void Compiler::Visit(OperatorExprNode *node) {
    node->left->Accept(*this);
    node->right->Accept(*this);

    switch (node->op) {
    case kMul:
        Mul(node->type);
        break;
    case kDiv:
        Div(node->type);
        break;
    case kMinus:
        Sub(node->type);
        break;
    case kPlus:
        Add(node->type);
        break;
    case kGt:
        Gt(node->left->type);
        break;
    case kLt:
        Lt(node->left->type);
        break;
    case kGe:
        Ge(node->left->type);
        break;
    case kLe:
        Le(node->left->type);
        break;
    case kEq:
        Eq(node->left->type);
        break;
    case kNeq:
        Neq(node->left->type);
        break;
    default:
        break;
    }
}

void Compiler::Visit(NegateExpr *node) {
    node->operand->Accept(*this);
    if (node->type == kInt) {
        GenCode(kOpCodeNegI);
    } else {
        GenCode(kOpCodeNegF);
    }
}

void Compiler::Visit(AssignExprNode *node) {
    AssignToVar(node->lhs, node->rhs.get());
}

void Compiler::Visit(CallExprNode *node) {
    const FunctionDecl *func = LookUpFunction(node->func_name);

    if (func->return_slots > 0) {
        StackAlloc(1);
    } else {
        StackAlloc(0);
    }

    for (const auto &arg : node->args) {
        arg->Accept(*this);
    }

    if (func->def == nullptr) {
        // This is a built in function
        GenCodeU32(kOpCodeCallname, func->name);
    } else {
        GenCodeU32(kOpCodeCall, func->def->index);
    }
}

void Compiler::Visit(LiteralExprNode *node) {
    if (node->type == kInt) {
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
    FunctionDecl *func_decl = LookUpFunction(node->name);
    func_ = func_decl->def;

    EnterScope();
    // Add parameters to the symbol table.
    for (int i = 0; i < node->params.size(); ++i) {
        const auto &param = node->params[i];
        AddLocalParam(param->name, param->type, i);
    }

    codes_ = MakePtr<BasicBlock>();

    // Generate code for the function body.
    node->body->Accept(*this);

    if (codes_->instructions.empty() || codes_->instructions.back().opcode != kOpCodeRet) {
        // Add return stmt to the end of the function if it does not exist.
        GenCode(kOpCodeRet);
    }
    func_->body.push_back(std::move(codes_));

    LeaveScope();

    func_ = nullptr;
}


const Variable &Compiler::LookUpVar(const std::string &name) {
    for (int i = static_cast<int>(symbole_tables_.size()) - 1; i >= 0; --i) {
        auto it = symbole_tables_[i].find(name);
        if (it != symbole_tables_[i].end()) {
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

void Compiler::AddLocalVar(const std::string &name, VarType type) {
    Variable var;
    var.type = type;
    var.scope = kLocal;

    var.offset = func_->loc_slots;
    ++func_->loc_slots;
    SymTab().emplace(name, var);
}

void Compiler::AddLocalParam(const std::string &name, VarType type, int index) {
    Variable var;
    var.type = type;
    var.scope = kParam;

    var.offset = func_->decl->param_slots + index;

    SymTab().emplace(name, var);
}

void Compiler::AddFunction(FuncDefNode *node) {
    auto func = MakePtr<FunctionDecl>();

    // Handle return value.
    if (node->return_type != kVoid) {
        func->return_slots = 1;
    }

    func->param_slots = node->params.size();

    program_.AddFuncDeclWithDef(node->name, std::move(func));
}

FunctionDecl *Compiler::LookUpFunction(const std::string &name) {
    auto it = program_.function_map.find(name);
    if (it != program_.function_map.end()) {
        return it->second;
    }

    // This must be a built in function.
    FunctionDecl func = BUILTIN_FUNCS.at(name);
    auto f = std::make_unique<FunctionDecl>(func);
    FunctionDecl *p = f.get();
    program_.AddFuncDecl(name, std::move(f));

    return p;
}
