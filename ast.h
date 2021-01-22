#ifndef AST_H_
#define AST_H_

#include <memory>
#include <vector>
#include <ostream>
#include "scanner.h"

enum VarType {
    kInt,
    kDouble,
    kBool,
    kVoid,
};

const std::string &TypeToString(VarType type);

struct ExprType {
    VarType type = kVoid;
    bool is_const = false;
};

struct ProgramNode;
struct ExprStmtNode;
struct DeclStmtNode;
struct IfStmtNode;
struct WhileStmtNode;
struct ReturnStmtNode;
struct BlockStmtNode;

struct OperatorExprNode;
struct NegateExpr;
struct AssignExprNode;
// struct AsExprNode;
struct CallExprNode;
struct LiteralExprNode;
struct IdentExprNode;

struct FuncDefNode;

class AstVisitor {
public:
    virtual void Visit(ProgramNode *node) = 0;
    virtual void Visit(ExprStmtNode *node) = 0;
    virtual void Visit(DeclStmtNode *node) = 0;
    virtual void Visit(IfStmtNode *node) = 0;
    virtual void Visit(WhileStmtNode *node) = 0;
    virtual void Visit(ReturnStmtNode *node) = 0;
    virtual void Visit(BlockStmtNode *node) = 0;
    virtual void Visit(OperatorExprNode *node) = 0;
    virtual void Visit(NegateExpr *node) = 0;
    virtual void Visit(AssignExprNode *node) = 0;
    virtual void Visit(CallExprNode *node) = 0;
    virtual void Visit(LiteralExprNode *node) = 0;
    virtual void Visit(IdentExprNode *node) = 0;
    virtual void Visit(FuncDefNode *node) = 0;
};

template <typename T>
using Ptr = std::unique_ptr<T>;

template <typename T>
const auto MakePtr = std::make_unique<T>;

template <typename T>
using PtrVec = std::vector<std::unique_ptr<T>>;

struct Node {
    Position pos;

    virtual ~Node() {};
    virtual void Print(std::ostream &out, int depth=0) const = 0;
    virtual void Accept(AstVisitor &visitor) = 0;
};

struct ExprNode : public Node {
    ExprType type = {kVoid, false};
};

struct StmtNode : public Node {};

struct ProgramNode : public Node {
    void Print(std::ostream &out, int depth=0) const override;
    void Accept(AstVisitor &v) override { v.Visit(this); }

    PtrVec<DeclStmtNode> global_vars;
    PtrVec<FuncDefNode> functions;
};

struct BlockStmtNode : public StmtNode {
    void Print(std::ostream &out, int depth=0) const override;
    void Accept(AstVisitor &v) override { v.Visit(this); }

    PtrVec<StmtNode> statements;
    bool is_func_body = false;
};

struct FuncDefNode : public Node {
    void Print(std::ostream &out, int depth=0) const override;    
    void Accept(AstVisitor &v) override { v.Visit(this); }

    std::string name;
    PtrVec<DeclStmtNode> params;
    Ptr<BlockStmtNode> body;
    VarType return_type = kVoid;
};

struct DeclStmtNode : public StmtNode {
    void Print(std::ostream &out, int depth=0) const override;
    void Accept(AstVisitor &v) override { v.Visit(this); }

    std::string name;
    VarType type = kVoid;
    bool is_const = false;
    Ptr<ExprNode> initializer;
};

struct CondBody {
    Ptr<ExprNode> condition;
    Ptr<BlockStmtNode> body;
};

struct IfStmtNode : public StmtNode {
    void Print(std::ostream &out, int depth=0) const override;
    void Accept(AstVisitor &v) override { v.Visit(this); }

    CondBody if_part;
    std::vector<CondBody> elif_part;
    Ptr<BlockStmtNode> else_part;
};

struct WhileStmtNode : public StmtNode {
    void Print(std::ostream &out, int depth=0) const override;
    void Accept(AstVisitor &v) override { v.Visit(this); }

    Ptr<ExprNode> condition;
    Ptr<BlockStmtNode> body;
};

struct ReturnStmtNode : public StmtNode {
    void Print(std::ostream &out, int depth=0) const override;
    void Accept(AstVisitor &v) override { v.Visit(this); }

    FuncDefNode *func;
    Ptr<ExprNode> expr;
};

struct ExprStmtNode : public StmtNode {
    void Print(std::ostream &out, int depth=0) const override;
    void Accept(AstVisitor &v) override { v.Visit(this); }

    Ptr<ExprNode> expr;
};

struct IdentExprNode : public ExprNode {
    void Print(std::ostream &out, int depth=0) const override;
    void Accept(AstVisitor &v) override { v.Visit(this); }

    std::string var_name;
};

struct AssignExprNode : public ExprNode {
    void Print(std::ostream &out, int depth=0) const override;
    void Accept(AstVisitor &v) override { v.Visit(this); }

    std::string lhs;
    Ptr<ExprNode> rhs;
};

struct LiteralExprNode : public ExprNode {
    void Print(std::ostream &out, int depth=0) const override;
    void Accept(AstVisitor &v) override { v.Visit(this); }

    std::string lexeme;
};

struct OperatorExprNode : public ExprNode {
    void Print(std::ostream &out, int depth=0) const override;
    void Accept(AstVisitor &v) override { v.Visit(this); }

    TokenType op;
    Ptr<ExprNode> left;
    Ptr<ExprNode> right;
};

struct NegateExpr : public ExprNode {
    void Print(std::ostream &out, int depth=0) const override;
    void Accept(AstVisitor &v) override { v.Visit(this); }

    Ptr<ExprNode> operand;
};

struct CallExprNode : public ExprNode {
    void Print(std::ostream &out, int depth=0) const override;
    void Accept(AstVisitor &v) override { v.Visit(this); }

    std::string func_name;
    PtrVec<ExprNode> args;
};

#endif // AST_H_
