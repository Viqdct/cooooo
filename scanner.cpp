#include "scanner.h"

#include <cstdlib>
#include <cctype>
#include <utility>
#include <iostream>
#include <map>
#include <unordered_map>

static const std::map<std::string, TokenType> KEYWORDS{
    {"fn", kFn},
    {"let", kLet},
    {"const", kConst},
    {"as", kAs},
    {"while", kWhile},
    {"if", kIf},
    {"else", kElse},
    {"return", kReturn},
};

static const std::map<TokenType, std::string> TOKEN_NAMES {
    {kFn, "Fn"},
    {kLet, "Let"},
    {kConst, "Const"},
    {kAs, "As"},
    {kWhile, "While"},
    {kIf, "If"},
    {kElse, "Else"},
    {kReturn, "Return"},
    {kBreak, "Break"},
    {kContinue, "Continue"},
    {kIdent, "Ident"},
    {kPlus, "+"},
    {kMinus, "-"},
    {kMul, "*"},
    {kDiv, "/"},
    {kAssign, "="},
    {kEq, "=="},
    {kNeq, "!="},
    {kLt, "<"},
    {kGt, ">"},
    {kLe, "<="},
    {kGe, ">="},
    {kL_paren, "("},
    {kR_paren, ")"},
    {kL_brace, "{"},
    {kR_brace, "}"},
    {kArrow, "->"},
    {kComma, ","},
    {kColon, ":"},
    {kSemicolon, ";"},
    {kIntLiteral, "IntLiteral"},
    {kDoubleLiteral, "DoubleLiteral"},
    {kEof,  "Eof"},
};

const std::string &TokenToString(TokenType type) {
    return TOKEN_NAMES.at(type);
}

// Remove trailing spaces from the given string.
static void RTrim(std::string &s) {
    if (s.empty())
        return;

    for (int i = s.size() - 1; i >= 0; --i) {
        if (!isspace(s[i]))
            break;
        s.pop_back();
    }
}

static bool IsLetter(char c) {
    return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z');
}

void Scanner::ScanFile(const std::string &filename) {
    filename_ = filename;
    input_.open(filename);

    if (!input_.is_open()) {
        error_ << "Cannot open the file " << filename;
        Error();
    }

    ScanAllTokens();
    input_.close();
}

const Token &Scanner::GetToken() {
    if (index_ < tokens_.size()) {
        return tokens_[index_++];
    } else {
        return eop_;
    }
}

const Token &Scanner::Peek(int i) const {
    if (index_ + i < tokens_.size())
        return tokens_[index_ + i];
    return eop_;
}

bool Scanner::ScanToken(Token &tk) {
    SkipSpaceOrComment();
    if (col_ >= line_.size())
        return false;

    tk.lexeme = "";
    tk.type = kEof;
    tk.pos.SetLineNo(line_no_);
    tk.pos.SetColNo(col_ + 1);

    int token_len = 1;

    switch (line_[col_]) {
    case '+':
        tk.type = kPlus;
        break;
    case '-':
        if (col_ + 1 < line_.size() && line_[col_ + 1] == '>') {
            token_len = 2;
            tk.type = kArrow;
        } else {
            tk.type = kMinus;
        }
        break;
    case '*':
        tk.type = kMul;
        break;
    case '/':
        tk.type = kDiv;
        break;
    case '=':
        if (col_ + 1 < line_.size() && line_[col_ + 1] == '=') {
            tk.type = kEq;
            token_len = 2;
        } else {
            tk.type = kAssign;
        }
        break;
    case '!':
        if (col_ + 1 < line_.size() && line_[col_ + 1] == '=') {
            tk.type = kNeq;
            token_len = 2;
        } else {
            error_ << "Invalid character !";
            Error();
        }
        break;
    case '<':
        if (col_ + 1 < line_.size() && line_[col_ + 1] == '=') {
            tk.type = kLe;
            token_len = 2;
        } else {
            tk.type = kLt;
        }
        break;
    case '>':
        if (col_ + 1 < line_.size() && line_[col_ + 1] == '=') {
            tk.type = kGe;
            token_len = 2;
        } else {
            tk.type = kGt;
        }
        break;
    case '(':
        tk.type = kL_paren;
        break;
    case ')':
        tk.type = kR_paren;
        break;
    case '{':
        tk.type = kL_brace;
        break;
    case '}':
        tk.type = kR_brace;
        break;
    case ',':
        tk.type = kComma;
        break;
    case ':':
        tk.type = kColon;
        break;
    case ';':
        tk.type = kSemicolon;
        break;
    default:
        token_len = 0;
        if (isdigit(line_[col_])) {
            ScanDoubleOrInt(tk);
        } else if (IsLetter(line_[col_]) || line_[col_] == '_') {
            ScanIDOrKeyword(tk);
        } else {
            error_ << "Invalid character " << line_[col_];
            Error();
        }
        break;
    }

    col_ += token_len;
    return true;;
}

void Scanner::ScanDoubleOrInt(Token &tk) {
    const int tk_start = col_;
    while (col_ < line_.size() && isdigit(line_[col_]))
        ++col_;

    if (col_ < line_.size() && line_[col_] == '.') {
        ++col_;
        if (col_ >= line_.size() || !isdigit(line_[col_])) {
            error_ << "Expected digit";
            Error();
        }

        while (col_ < line_.size() && isdigit(line_[col_]))
            ++col_;

        // Parse the exponent
        if (col_ < line_.size() && line_[col_] == 'e' || line_[col_] == 'E') {
            ++col_;
            if (col_ >= line_.size()) {
                error_ << "Unexpected end of line";
                Error();
            }

            if (line_[col_] == '+' || line_[col_] == '-')
                ++col_;

            if (col_ >= line_.size() || !isdigit(line_[col_])) {
                error_ << "Expected digit";
                Error();
            }

            while (col_ < line_.size() && isdigit(line_[col_]))
                ++col_;
        }

        tk.type = kDoubleLiteral;
    } else {
        tk.type = kIntLiteral;
    }

    tk.lexeme = line_.substr(tk_start, col_ - tk_start);
}

void Scanner::ScanIDOrKeyword(Token &tk) {
    const int start = col_;
    while (col_ < line_.size() && (IsLetter(line_[col_]) || isdigit(line_[col_]) || line_[col_] == '_'))
        ++col_;

    tk.lexeme = line_.substr(start, col_ - start);

    auto it = KEYWORDS.find(tk.lexeme);
    if (it != KEYWORDS.end()) {
        tk.type = it->second;
    } else {
        tk.type = kIdent;
    }
}

void Scanner::ScanAllTokens() {
    while (std::getline(input_, line_)) {
        RTrim(line_);
        ++line_no_;

        if (line_.empty())
            continue;   // Skip empty line.

        Token tk;
        col_ = 0;

        while (ScanToken(tk)) {
            tokens_.push_back(std::move(tk));
        }
    }
}

void Scanner::SkipSpaceOrComment() {
    while (col_ < line_.size() && isspace(line_[col_]))
        ++col_;

    // Skip comment.
    if (col_ + 1 < line_.size() && line_[col_] == '/' && line_[col_ + 1] == '/')
        col_ = line_.size();
}

void Scanner::Error() {
    std::cout << filename_ << ":"
              << line_no_ << ":"
              << col_ + 1 << ": lexical error: "
              << error_.str()
              << std::endl;
    exit(1);
}
