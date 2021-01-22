#ifndef ANALYZER_H
#define ANALYZER_H

#include <string>
#include <unordered_map>
#include <vector>
#include <sstream>

#include "ast.h"
#include "parser.h"
#include "symbol_table.h"

class TypeChecker : public AstVisitor {
public:
    TypeChecker(const std::string &filename);
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
    void CreateAllBuiltinFunctions();
    void CreateBuiltinFunction(std::string func_name, VarType return_type, std::vector<VarType> params);

    SymbolTable &SymTab() { return symbol_tables_.back(); }
    template <typename T>
    T *LookUp(const std::string &name) const;
    void Error(Position error_pos);
    void EnterScope();
    void LeaveScope();

private:
    std::string filename_;
    std::ostringstream error_;
    std::vector<SymbolTable> symbol_tables_;
    std::vector<Ptr<FuncDefNode>> builtin_funcs_;
};

template <typename T>
T *TypeChecker::LookUp(const std::string &name) const {
    for (int i = symbol_tables_.size() - 1; i >= 0; --i) {
        T *p = dynamic_cast<T*>(symbol_tables_[i].LookUp(name));
        if (p != nullptr)
            return p;
    }

    return nullptr;
}

#endif // ANALYZER_H

