#ifndef PARSER_H_
#define PARSER_H_

#include "ast.h"
#include "scanner.h"

constexpr int kMinBinaryOpPrecedence = 2;

class Parser {
public:
    Ptr<ProgramNode> ParseFile(const std::string &filename);
    const std::string &Filename() const { return scanner_.Filename(); }

private:
    PtrVec<StmtNode> ParseStmtList(FuncDefNode *func);
    Ptr<StmtNode> ParseStmt(FuncDefNode *func);
    Ptr<FuncDefNode> ParseFuncDef();
    Ptr<DeclStmtNode> ParseDeclStmt(bool is_const);
    Ptr<BlockStmtNode> ParseBlockStmt(FuncDefNode *func);
    Ptr<ExprStmtNode> ParseExprStmt();
    Ptr<IfStmtNode> ParseIfStmt(FuncDefNode *func);
    Ptr<WhileStmtNode> ParseWhileStmt(FuncDefNode *func);
    Ptr<ReturnStmtNode> ParseReturnStmt(FuncDefNode *func);
    PtrVec<DeclStmtNode> ParseParams();
    Ptr<ExprNode> ParseExpression(int min_precedence=kMinBinaryOpPrecedence);
    Ptr<ExprNode> ParseBinaryOpExpr(Ptr<ExprNode> left, int min_precedence);
    Ptr<ExprNode> ParseNegateExpr();
    Ptr<ExprNode> ParseAssignExpr();
    Ptr<ExprNode> ParseLiteralExpr(VarType type);
    Ptr<ExprNode> ParseIdentExpr();
    Ptr<ExprNode> ParseFactor();
    PtrVec<ExprNode> ParseArgs();
    Ptr<ExprNode> ParseFuncCall();
    // Ptr<ArrayExprNode> ParseArrayLiteral();

    VarType ParseType();
    VarType ParseVarType();
    void ExpectToken(TokenType);
    void ConsumeToken(TokenType);
    void ConsumeToken() { scanner_.GetToken(); }
    void Error(Position pos);

private:
    std::ostringstream error_;
    Scanner scanner_;
};

#endif // PARSER_H_
