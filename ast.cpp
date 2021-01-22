#include "ast.h"

#include <map>

static const std::map<VarType, std::string> TYPE_NAMES {
    {kInt, "int"},
    {kDouble, "double"},
    {kBool, "bool"},
    {kVoid, "void"},
};

const std::string &TypeToString(VarType type) {
    return TYPE_NAMES.at(type);
}

static void PrintSpaces(std::ostream &os, int n) {
    n *= 2;

    while (n > 4) {
        os << "    ";
        n -= 4;
    }

    while (n > 0) {
        os << ' ';
        --n;
    }
}

const char *VarTypeToString(VarType type) {
    switch(type) {
    case kInt:
        return "int";
    case kDouble:
        return "double";
    case kBool:
        return "bool";
    default:
        return "void";
    }
}

void ProgramNode::Print(std::ostream &out, int depth) const {
    PrintSpaces(out, depth);
    out << "Program:\n";

    PrintSpaces(out, depth + 1);
    out << "Global Variables:\n";
    for (const auto &var : global_vars) {
        var->Print(out, depth + 2);;
    }

    PrintSpaces(out, depth + 1);
    out << "Functions:\n";
    for (const auto &func : functions) {
        func->Print(out, depth + 2);
    }
}

void BlockStmtNode::Print(std::ostream &out, int depth) const {
    PrintSpaces(out, depth);
    out << "Block Stmt:\n";
    for (const auto &stmt : statements) {
        stmt->Print(out, depth + 1);
    }
}

void FuncDefNode::Print(std::ostream &out, int depth) const {
    PrintSpaces(out, depth);
    out << "Function: " << name
        << "(";

    for (int i = 0; i < params.size(); ++i) {
        const auto &param = params[i];

        if (i > 0)
            out << ", ";

        if (param->is_const)
            out << "const ";
        out << param->name
            << ": " << VarTypeToString(param->type);
    }

    out << ") -> " << VarTypeToString(return_type) << "\n";

    body->Print(out, depth + 1);
}

void DeclStmtNode::Print(std::ostream &out, int depth) const {
    PrintSpaces(out, depth);
    out << "Declare: ";

    if (is_const)
        out << "const ";

    out << name << ": " << VarTypeToString(type) << "\n";

    if (initializer) {
        PrintSpaces(out, depth + 1);
        out << "Initializer:\n";
        initializer->Print(out, depth + 2);
    }
}

void IfStmtNode::Print(std::ostream &out, int depth) const {
    PrintSpaces(out, depth);
    out << "If stmt:\n";

    PrintSpaces(out, depth + 1);
    out << "Condition:\n";
    if_part.condition->Print(out, depth + 2);
    PrintSpaces(out, depth + 1);
    out << "Body:\n";
    if_part.body->Print(out, depth + 2);

    for (const auto &cond_body : elif_part) {
        PrintSpaces(out, depth);
        out << "ElseIf:\n";
        PrintSpaces(out, depth + 1);
        out << "Condition:\n";
        cond_body.condition->Print(out, depth + 2);
        PrintSpaces(out, depth + 1);
        out << "Body:\n";
        cond_body.body->Print(out, depth + 2);
    }

    if (else_part) {
        PrintSpaces(out, depth);
        out << "Else:\n";
        else_part->Print(out, depth + 2);
    }
}

void WhileStmtNode::Print(std::ostream &out, int depth) const {
    PrintSpaces(out, depth);
    out << "While stmt:\n";
    PrintSpaces(out, depth + 1);
    out << "Condition:\n";
    condition->Print(out, depth + 2);

    PrintSpaces(out, depth + 1);
    out << "Condition:\n";
    body->Print(out, depth + 2);
}

void ReturnStmtNode::Print(std::ostream &out, int depth) const {
    PrintSpaces(out, depth);
    if (expr) {
        out << "Return:\n";
        expr->Print(out, depth + 1);
    } else {
        out << "Return\n";
    }
}

void ExprStmtNode::Print(std::ostream &out, int depth) const {
    PrintSpaces(out, depth);
    out << "Expression stmt:\n";
    expr->Print(out, depth + 1);
}

void IdentExprNode::Print(std::ostream &out, int depth) const {
    PrintSpaces(out, depth);
    out << "ID: " << var_name << '\n';
}

void AssignExprNode::Print(std::ostream &out, int depth) const {
    PrintSpaces(out, depth);

    out << "Assignment: " << lhs << " = :\n";
    rhs->Print(out, depth + 1);
}

void LiteralExprNode::Print(std::ostream &out, int depth) const {
    PrintSpaces(out, depth);

    out << "Literal(" << VarTypeToString(type.type) << "): " << lexeme << '\n';
}

void OperatorExprNode::Print(std::ostream &out, int depth) const {
    PrintSpaces(out, depth);

    out << "Operator: " << TokenToString(op) << '\n';
    left->Print(out, depth + 1);
    right->Print(out, depth + 1);
}

void NegateExpr::Print(std::ostream &out, int depth) const {
    PrintSpaces(out, depth);

    out << "Negate:\n";
    operand->Print(out, depth + 1);
}

void CallExprNode::Print(std::ostream &out, int depth) const {
    PrintSpaces(out, depth);

    out << "Call function: " << func_name << ", ";
    if (args.empty()) {
        out << "without arguments.\n";
    } else {
        out << "with arguments:\n";
        for (const auto &arg : args) {
            arg->Print(out, depth + 1);
        }
    }
}
