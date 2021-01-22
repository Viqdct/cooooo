#include "analyzer.h"

#include <cstdlib>
#include <iostream>

TypeChecker::TypeChecker(const std::string &filename) : filename_(filename) {
    EnterScope();
    CreateAllBuiltinFunctions();
}

void TypeChecker::CreateAllBuiltinFunctions() {
    CreateBuiltinFunction("getint", kInt, {});
    CreateBuiltinFunction("getdouble", kDouble, {});
    CreateBuiltinFunction("getchar", kInt, {});
    CreateBuiltinFunction("putint", kVoid, {kInt});
    CreateBuiltinFunction("putdouble", kVoid, {kDouble});
    CreateBuiltinFunction("putchar", kVoid, {kInt});
    // CreateBuiltinFunction("putstr", kVoid, {kInt});
    CreateBuiltinFunction("putln", kVoid, {});
}

void TypeChecker::CreateBuiltinFunction(std::string func_name, VarType return_type, std::vector<VarType> params) {
    Ptr<FuncDefNode> func = std::make_unique<FuncDefNode>();
    func->name = std::move(func_name);
    func->return_type = return_type;

    for (VarType param : params) {
        Ptr<DeclStmtNode> p = std::make_unique<DeclStmtNode>();
        p->type = param;
        func->params.push_back(std::move(p));
    }

    SymTab().InsertSymbol(func->name, func.get());
    builtin_funcs_.push_back(std::move(func));
}


void TypeChecker::Visit(ProgramNode *node) {
    // Add all functions to the symbol table.
    for (const auto &fn : node->functions) {
        if (!SymTab().InsertSymbol(fn->name, fn.get())) {
            error_ << "Redeclare function " << fn->name;
            Error(fn->pos);
        }
    }

    for (const auto &var : node->global_vars) {
        var->Accept(*this);
    }

    for (const auto &fn : node->functions) {
        fn->Accept(*this);
    }
}

void TypeChecker::Visit(ExprStmtNode *node) {
    node->expr->Accept(*this);
}

void TypeChecker::Visit(DeclStmtNode *node) {
    if (!SymTab().InsertSymbol(node->name, node)) {
        error_ << "Redeclaration of symbol " << node->name;
        Error(node->pos);
    }

    if (node->initializer) {
        node->initializer->Accept(*this);
        if (node->type != node->initializer->type.type) {
            error_ << "Cannot assign expresion of type "
                   << TypeToString(node->initializer->type.type)
                   << " to variable "
                   << node->name
                   << " which has type "
                   << TypeToString(node->type);
            Error(node->initializer->pos);
        }
    }
}

void TypeChecker::Visit(IfStmtNode *node) {
    node->if_part.condition->Accept(*this);
    node->if_part.body->Accept(*this);

    for (const auto &cond_body : node->elif_part) {
        cond_body.condition->Accept(*this);
        cond_body.body->Accept(*this);
    }

    if (node->else_part)
        node->else_part->Accept(*this);
}

void TypeChecker::Visit(WhileStmtNode *node) {
    node->condition->Accept(*this);
    node->body->Accept(*this);
}

void TypeChecker::Visit(ReturnStmtNode *node) {
    VarType return_type = node->func->return_type;
    if (return_type == kVoid) {
        if (node->expr) {
            error_ << "Return non empty expression in function "
                   << node->func->name
                   << " that returns void";
            Error(node->pos);
        }
        return;
    }

    node->expr->Accept(*this);
    if (return_type != node->expr->type.type) {
        error_ << "Return type mismatch in function "
               << node->func->name;
        Error(node->pos);
    }
}

void TypeChecker::Visit(BlockStmtNode *node) {
    if (!node->is_func_body)
        EnterScope();
    for (const auto &stmt : node->statements) {
        stmt->Accept(*this);
    }
    if (!node->is_func_body)
        LeaveScope();
}

void TypeChecker::Visit(OperatorExprNode *node) {
    node->left->Accept(*this);
    node->right->Accept(*this);
    VarType left_type = node->left->type.type;
    VarType right_type = node->right->type.type;

    if (left_type != right_type || left_type == kVoid || left_type == kBool) {
        error_ << "The type of both operands of an binary operator '"
               << TokenToString(node->op)
               << "' must be the same and cannot be void or bool.";
        Error(node->pos);
    }

    switch (node->op) {
    case kMul:
    case kDiv:
    case kMinus:
    case kPlus:
        node->type.type = left_type;
        break;
    case kGt:
    case kLt:
    case kGe:
    case kLe:
    case kEq:
    case kNeq:
        node->type.type = kBool;
        break;
    default:
        break;
    }
}

void TypeChecker::Visit(NegateExpr *node) {
    node->operand->Accept(*this);
    VarType operand_type = node->operand->type.type;
    if (operand_type == kVoid || operand_type == kBool) {
        error_ << "The operand of '-' cannot be of type void or bool";
        Error(node->pos);
    }
    node->type.type = node->operand->type.type;
}

void TypeChecker::Visit(AssignExprNode *node) {
    DeclStmtNode *var = LookUp<DeclStmtNode>(node->lhs);
    if (var == nullptr) {
        // Assign to an undeclared variable.
        error_ << "Cannot assign to an undefined variable "
               << node->lhs;
        Error(node->pos);
    }

    if (var->is_const) {
        // Assign to a const variable.
        error_ << "Cannot assign to const variable "
               << node->lhs;
        Error(node->pos);
    }

    node->rhs->Accept(*this);
    if (var->type != node->rhs->type.type) {
        error_ << "Cannot assign expression of type "
               << TypeToString(node->rhs->type.type)
               << " to the variable " << node->lhs
               << " which has type " + TypeToString(var->type);
        Error(node->rhs->pos);
    }

    // Assignment expression has void type.
    node->type.type = kVoid;
}

void TypeChecker::Visit(CallExprNode *node) {
    FuncDefNode *func = LookUp<FuncDefNode>(node->func_name);
    if (func == nullptr) {
        error_ << "Undefined function " << node->func_name;
        Error(node->pos);
    }

    if (func->params.size() != node->args.size()) {
        error_ << "Parameter size mismatch when calling function "
               << node->func_name;
        Error(node->pos);
    }

    for (int i = 0; i < func->params.size(); ++i) {
        node->args[i]->Accept(*this);
        if (func->params[i]->type != node->args[i]->type.type) {
            error_ << "Type mismatch, expected "
                   << TypeToString(func->params[i]->type)
                   << ", got " << TypeToString(node->args[i]->type.type)
                   << " when calling function " << func->name;
            Error(node->args[i]->pos);
        }
    }

    node->type.type = func->return_type;
}

void TypeChecker::Visit(LiteralExprNode *node) {
}

void TypeChecker::Visit(IdentExprNode *node) {
    DeclStmtNode *var = LookUp<DeclStmtNode>(node->var_name);
    if (var == nullptr) {
        // Reference to an undeclared variable.
        error_ << "Undeclared variable " << node->var_name;
        Error(node->pos);
    }

    node->type.type = var->type;
}

void TypeChecker::Visit(FuncDefNode *node) {
    EnterScope();
    // Insert parameters to the symbol table.
    for (const auto &param : node->params) {
        if (!SymTab().InsertSymbol(param->name, param.get())) {
            error_ << "Duplicated parameter name " << param->name;
            Error(param->pos);
        }
    }

    node->body->Accept(*this);

    LeaveScope();
}

void TypeChecker::Error(Position error_pos) {
    std::cout << filename_ << ":"
              << error_pos.LineNo() << ":"
              << error_pos.ColNo()
              << ": semantic error: " << error_.str() << std::endl;
    exit(1);
}

void TypeChecker::EnterScope() {
    symbol_tables_.emplace_back();
}

void TypeChecker::LeaveScope() {
    symbol_tables_.pop_back();
}
