#include "parser.h"

#include <iostream>
#include <cstdlib>
#include <utility>

static int GetOpPrecedence(TokenType op) {
    switch (op) {
    case kAs:
        return 5;
    case kMul:
    case kDiv:
        return 4;
    case kMinus:
    case kPlus:
        return 3;
    case kGt:
    case kLt:
    case kGe:
    case kLe:
    case kEq:
    case kNeq:
        return 2;
    case kAssign:
        return 1;
    default:
        return 0;
    }
}

static bool IsBinaryOp(TokenType tk) {
    switch (tk) {
    case kMul:
    case kDiv:
    case kMinus:
    case kPlus:
    case kGt:
    case kLt:
    case kGe:
    case kLe:
    case kEq:
    case kNeq:
        return true;
    default:
        return false;
    }
}

Ptr<ProgramNode> Parser::ParseFile(const std::string &filename) {
    scanner_.ScanFile(filename);
    Ptr<ProgramNode> program = std::make_unique<ProgramNode>();

    while (true) {
        const Token &tk = scanner_.Peek(0);
        if (tk.type == kLet) {
            program->global_vars.push_back(ParseDeclStmt(false));
        } else if (tk.type == kConst) {
            program->global_vars.push_back(ParseDeclStmt(true));
        } else if (tk.type == kFn) {
            break;
        } else {
            error_ << "Unexpected token " << TokenToString(scanner_.Peek(0).type);
            Error(scanner_.Peek(0).pos);
        }
    }

    while (scanner_.Peek(0).type == kFn) {
        program->functions.push_back(ParseFuncDef());
    }

    if (scanner_.Peek(0).type != kEof) {
        error_ << "Unexpected token " << TokenToString(scanner_.Peek(0).type)
               << " at end of program";
        Error(scanner_.Peek(0).pos);
    }

    return program;
}

Ptr<StmtNode> Parser::ParseStmt(FuncDefNode *func) {
    Ptr<StmtNode> stmt;

    switch (scanner_.Peek(0).type) {
    case kLet:
        stmt = ParseDeclStmt(false);
        break;
    case kConst:
        stmt = ParseDeclStmt(true);
        break;
    case kIf:
        stmt = ParseIfStmt(func);
        break;
    case kWhile:
        stmt = ParseWhileStmt(func);
        break;
    case kReturn:
        stmt = ParseReturnStmt(func);
        break;
    case kL_brace:
        stmt = ParseBlockStmt(func);
        break;
    default:
        stmt = ParseExprStmt();
        break;
    }

    return stmt;
}

Ptr<FuncDefNode> Parser::ParseFuncDef() {
    ConsumeToken();         // Skip 'fn'
    ExpectToken(kIdent);
    Ptr<FuncDefNode> func = std::make_unique<FuncDefNode>();
    func->pos = scanner_.Peek(0).pos;

    func->name = scanner_.Peek(0).lexeme;
    ConsumeToken();         // Skip the function name

    ConsumeToken(kL_paren);
    func->params = ParseParams();
    ConsumeToken(kR_paren);

    ConsumeToken(kArrow);
    func->return_type = ParseType();

    func->body = ParseBlockStmt(func.get());
    func->body->is_func_body = true;

    return func;
}

Ptr<DeclStmtNode> Parser::ParseDeclStmt(bool is_const) {
    auto stmt = std::make_unique<DeclStmtNode>();
    stmt->pos = scanner_.Peek(0).pos;

    ConsumeToken();         // Skip 'let' or 'const'
    stmt->name = scanner_.Peek(0).lexeme;
    ConsumeToken();         // Skip the variable name.
    ConsumeToken(kColon);
    stmt->type = ParseVarType();

    if (scanner_.Peek(0).type == kAssign) {
        ConsumeToken();     // Skip '='
        stmt->initializer = ParseExpression();
    }

    if (is_const) {
        if (!stmt->initializer) {
            error_ << "Uninitialized constat " << stmt->name;
            Error(stmt->pos);    // Constant must be initialized.
        }
        stmt->is_const = true;
    }

    ConsumeToken(kSemicolon);

    return stmt;
}

Ptr<BlockStmtNode> Parser::ParseBlockStmt(FuncDefNode *func) {
    ConsumeToken(kL_brace);
    auto stmt = std::make_unique<BlockStmtNode>();
    stmt->pos = scanner_.Peek(0).pos;

    while (scanner_.Peek(0).type != kR_brace) {
        stmt->statements.push_back(ParseStmt(func));
    }
    ConsumeToken(kR_brace);
    return stmt;
}

Ptr<ExprStmtNode> Parser::ParseExprStmt() {
    auto stmt = std::make_unique<ExprStmtNode>();
    stmt->pos = scanner_.Peek(0).pos;
    stmt->expr = ParseExpression();
    ConsumeToken(kSemicolon);
    return stmt;
}

Ptr<ExprNode> Parser::ParseExpression(int min_precedence) {
    TokenType tk1 = scanner_.Peek(0).type;
    TokenType tk2 = scanner_.Peek(1).type;

    Ptr<ExprNode> left;
    switch (tk1) {
    case kL_paren:
        ConsumeToken();
        left = ParseExpression(kMinBinaryOpPrecedence);
        ConsumeToken(kR_paren);
        break;
    case kMinus:
        left = ParseNegateExpr();
        break;
    case kIntLiteral:
        left = ParseLiteralExpr(kInt);
        break;
    case kDoubleLiteral:
        left = ParseLiteralExpr(kDouble);
        break;
    case kIdent:
        switch (tk2) {
        case kL_paren:
            left = ParseFuncCall();
            break;
        case kAssign:
            left = ParseAssignExpr();
            break;
        default:
            left = ParseIdentExpr();
            break;
        }
        break;
    default:
        error_ << "Invalid expression";
        Error(scanner_.Peek(0).pos);
        break;
    }

    return ParseBinaryOpExpr(std::move(left), min_precedence);
}

Ptr<ExprNode> Parser::ParseBinaryOpExpr(Ptr<ExprNode> left, int min_precedence) {
    while (true) {
        TokenType op = scanner_.Peek(0).type;
        int precedence = GetOpPrecedence(op);
        if (!IsBinaryOp(op) || precedence < min_precedence)
            break;

        Ptr<OperatorExprNode> expr = std::make_unique<OperatorExprNode>();
        expr->pos = scanner_.Peek(0).pos;

        expr->left = std::move(left);
        expr->op = op;
        ConsumeToken();     // Skip the operator.
        expr->right = ParseExpression(precedence + 1);

        left = std::move(expr);
    }

    return left;
}

Ptr<ExprNode> Parser::ParseNegateExpr() {
    Ptr<NegateExpr> expr = std::make_unique<NegateExpr>();
    expr->pos = scanner_.Peek(0).pos;

    ConsumeToken(kMinus);
    expr->operand = ParseExpression();

    return expr;
}

Ptr<ExprNode> Parser::ParseAssignExpr() {
    Ptr<AssignExprNode> expr = std::make_unique<AssignExprNode>();
    expr->lhs = scanner_.Peek(0).lexeme;
    ConsumeToken();
    expr->pos = scanner_.Peek(0).pos;
    ConsumeToken(kAssign); // Skip '='
    expr->rhs = ParseExpression();

    return expr;
}

Ptr<ExprNode> Parser::ParseLiteralExpr(VarType type) {
    Ptr<LiteralExprNode> expr = std::make_unique<LiteralExprNode>();
    expr->pos = scanner_.Peek(0).pos;
    expr->type.type = type;
    expr->type.is_const = true;
    expr->lexeme = scanner_.GetToken().lexeme;

    return expr;
}

Ptr<ExprNode> Parser::ParseIdentExpr() {
    Ptr<IdentExprNode> expr = std::make_unique<IdentExprNode>();
    expr->pos = scanner_.Peek(0).pos;
    expr->var_name = scanner_.Peek(0).lexeme;
    ConsumeToken();

    return expr;
}

PtrVec<ExprNode> Parser::ParseArgs() {
    PtrVec<ExprNode> args;
    if (scanner_.Peek(0).type == kR_paren)
        return args;

    while (true) {
        args.push_back(ParseExpression());

        if (scanner_.Peek(0).type == kComma) {
            ConsumeToken();     // Skip ','
        } else {
            break;
        }
    }

    return args;
}

Ptr<ExprNode> Parser::ParseFuncCall() {
    auto expr = std::make_unique<CallExprNode>();
    expr->func_name = scanner_.Peek(0).lexeme;
    expr->pos = scanner_.Peek(0).pos;
    ConsumeToken();
    ConsumeToken(kL_paren);     // Skip '('
    expr->args = ParseArgs();
    ConsumeToken(kR_paren);     // Skip ')'
    return expr;
}

Ptr<IfStmtNode> Parser::ParseIfStmt(FuncDefNode *func) {
    auto stmt = std::make_unique<IfStmtNode>();
    stmt->pos = scanner_.Peek(0).pos;
    ConsumeToken();         // Skip if
    stmt->if_part.condition = ParseExpression();
    stmt->if_part.body = ParseBlockStmt(func);

    while (scanner_.Peek(0).type == kElse) {
        ConsumeToken();         // Skip else
        TokenType tk = scanner_.Peek(0).type;
        if (tk == kL_brace) {
            stmt->else_part = ParseBlockStmt(func);
            break;
        } else if (tk == kIf) {
            ConsumeToken();     // Skip "if"
            CondBody cond_body;
            cond_body.condition = ParseExpression();
            cond_body.body = ParseBlockStmt(func);

            stmt->elif_part.push_back(std::move(cond_body));
        } else {
            error_ << "Expected an 'if' or '{'";
            Error(scanner_.Peek(0).pos);
        }
    }
    return stmt;
}

Ptr<WhileStmtNode> Parser::ParseWhileStmt(FuncDefNode *func) {
    auto stmt = std::make_unique<WhileStmtNode>();
    stmt->pos = scanner_.Peek(0).pos;

    ConsumeToken();
    stmt->condition = ParseExpression();
    stmt->body = ParseBlockStmt(func);
    return stmt;
}

Ptr<ReturnStmtNode> Parser::ParseReturnStmt(FuncDefNode *func) {
    auto stmt = std::make_unique<ReturnStmtNode>();
    stmt->pos = scanner_.Peek(0).pos;
    stmt->func = func;
    ConsumeToken();

    if (scanner_.Peek(0).type != kSemicolon) {
        stmt->expr = ParseExpression();
    }

    ConsumeToken(kSemicolon);

    return stmt;
}

PtrVec<DeclStmtNode>  Parser::ParseParams() {
    PtrVec<DeclStmtNode> params;

    while (true) {
        TokenType tk = scanner_.Peek(0).type;
        if (tk != kConst && tk != kIdent)
            break;

        Ptr<DeclStmtNode> param = std::make_unique<DeclStmtNode>();
        if (tk == kConst) {
            ConsumeToken();
            param->is_const = true;
        }

        param->pos = scanner_.Peek(0).pos;
        ExpectToken(kIdent);
        param->name = scanner_.Peek(0).lexeme;
        ConsumeToken();
        ConsumeToken(kColon);
        param->type = ParseVarType();

        params.push_back(std::move(param));

        if (scanner_.Peek(0).type == kComma) {
            ConsumeToken();
        } else {
            break;
        }
    }
    return params;
}

VarType Parser::ParseType() {
    ExpectToken(kIdent);

    const std::string &type_name = scanner_.Peek(0).lexeme;
    VarType type = kVoid;

    if (type_name == "int") {
        type = kInt;
    } else if (type_name == "double") {
        type = kDouble;
    } else if (type_name == "void") {
        type = kVoid;
    } else {
        error_ << "Expected a type specifier, got"
               << scanner_.Peek(0).lexeme;
        Error(scanner_.Peek(0).pos);
    }

    ConsumeToken();
    return type;
}


VarType Parser::ParseVarType() {
    ExpectToken(kIdent);

    const std::string &type_name = scanner_.Peek(0).lexeme;
    VarType type = kVoid;

    if (type_name == "int") {
        type = kInt;
    } else if (type_name == "double") {
        type = kDouble;
    } else {
        error_ << "Expected an int or double type specifier, got"
               << scanner_.Peek(0).lexeme;
        Error(scanner_.Peek(0).pos);
    }

    ConsumeToken();
    return type;
}

void Parser::ExpectToken(TokenType type) {
    if (scanner_.Peek(0).type != type) {
        const Token &tk = scanner_.Peek(0);
        error_ << "Expected a " << TokenToString(type)
               << ", got " << TokenToString(tk.type);
        Error(tk.pos);
    }
}

void Parser::ConsumeToken(TokenType type) {
    ExpectToken(type);
    scanner_.GetToken();
}

void Parser::Error(Position pos) {
    std::cout << Filename() << ":"
              << pos.LineNo() << ":"
              << pos.ColNo() << ": syntax error: "
              << error_.str()
              << std::endl;
    exit(1);
}
