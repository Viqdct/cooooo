#ifndef COMPILER_H
#define COMPILER_H

#include <vector>
#include <map>

#include "ast.h"
#include "opcode.h"

template <typename T>
using Array = std::vector<T>;

struct FunctionDecl;

enum VarScope {
    kLocal,
    kGlobal,
    kParam,
};

struct Variable {
    VarScope scope;
    VarType type;
    int32_t offset;
};

struct GlobalDef {
    uint8_t is_const = 0;
    Array<uint8_t> value;
};

struct Instruction {
    OpCode opcode;
    uint64_t param = 0;
    uint8_t param_size = 0;

    void PackInt32Param(int32_t x);
    void PackUint32Param(uint32_t x);
    void PackUint64Param(int64_t x);
};

struct BasicBlock {
    Array<Instruction> instructions;
    BasicBlock *br = nullptr;
    int offset = 0;
};

struct FuncDef {
    uint32_t loc_slots = 0;
    uint32_t num_insts = 0;
    FunctionDecl *decl;
    int index = 0;

    PtrVec<BasicBlock> body;

    void CalculateJmpOffset();

    void AddLocalVar(const std::string &name, VarType type, VarScope scope);
};

struct FunctionDecl {
    uint32_t name = 0;
    uint32_t return_slots = 0;
    uint32_t param_slots = 0;
    FuncDef *def = nullptr;
};

struct ProgramBinary {
    Array<GlobalDef> globals;
    PtrVec<FunctionDecl> function_decls;
    PtrVec<FuncDef> function_defs;

    void AddGlobalVar(const std::string &name, VarType type);
    void AddFuncDecl(const std::string &func_name, Ptr<FunctionDecl> func);
    FuncDef *AddFuncDeclWithDef(const std::string &func_name, Ptr<FunctionDecl> func);

    std::map<std::string, Variable> global_vars;
    std::map<std::string, FunctionDecl*> function_map;

private:
    void AddFuncName(const std::string &func_name);
};

class Compiler : public AstVisitor {
public:
    Compiler(std::ostream &out);
    void Compile(ProgramNode *program);

private:
    void Visit(ProgramNode *node) override;
    void Visit(ExprStmtNode *node) override;
    void Visit(DeclStmtNode *node) override;
    void Visit(IfStmtNode *node) override;
    void Visit(WhileStmtNode *node) override;
    void Visit(ReturnStmtNode *node) override;
    void Visit(BlockStmtNode *node) override;
    void Visit(OperatorExprNode *node) override;
    void Visit(NegateExpr *node) override;
    void Visit(AssignExprNode *node) override;
    void Visit(CallExprNode *node) override;
    void Visit(LiteralExprNode *node) override;
    void Visit(IdentExprNode *node) override;
    void Visit(FuncDefNode *node) override;

private:

    using SymbolTable = std::map<std::string, Variable>;

private:
    void WriteByte(uint8_t);
    void WriteLit32(uint32_t value);
    void WriteLit64(uint64_t value);
    void GenerateCode();
    void GenCondBody(CondBody &cond_body, BasicBlock *next, BasicBlock *end);
    void CreateNewCodeBlock();
    void AddStartFunc();
    void GenStartFunc(ProgramNode *node);
    const Variable &LookUpVar(const std::string &name);
    void PushInt(int64_t);
    void PushDouble(double);
    void PushVarAddr(const std::string &name);
    void AssignToVar(const std::string &name, ExprNode *expr);
    void StoreExpr(ExprNode *expr);
    void Ret();
    void GenCode(OpCode opcode);
    void GenCodeI32(OpCode opcode, int32_t x);
    void GenCodeU32(OpCode opcode, uint32_t x);
    void GenCodeU64(OpCode opcode, uint64_t x);
    void Add(VarType type);
    void Sub(VarType type);
    void Mul(VarType type);
    void Div(VarType type);
    void Lt(VarType type);
    void Le(VarType type);
    void Gt(VarType type);
    void Ge(VarType type);
    void Eq(VarType type);
    void Neq(VarType type);
    void Compare(VarType type);
    void StackAlloc(uint32_t n);

    void EnterScope() { symbole_tables_.emplace_back(); }
    SymbolTable &SymTab() { return symbole_tables_.back(); }
    void LeaveScope() { symbole_tables_.pop_back(); }
    void AddLocalVar(const std::string &name, VarType type);
    void AddLocalParam(const std::string &name, VarType type, int index);
    void AddFunction(FuncDefNode *func);
    FunctionDecl *LookUpFunction(const std::string &name);

private:
    std::ostream &out_;
    FuncDef *func_;
    ProgramBinary program_;

    Ptr<BasicBlock> codes_;
    PtrVec<FuncDef> functions_;
    std::vector<SymbolTable> symbole_tables_;
};

#endif // COMPILER_H
