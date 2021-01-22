#ifndef SCANNER_H_
#define SCANNER_H_

#include <string>
#include <vector>
#include <fstream>
#include <cstdint>
#include <sstream>

enum TokenType {
    // Begin of keywords
    kFn,
    kLet,
    kConst,
    kAs,
    kWhile,
    kIf,
    kElse,
    kReturn,
    kBreak,
    kContinue,
    // End of keywords
    // Begin of punctuations
    kIdent,
    kPlus,
    kMinus,
    kMul,
    kDiv,
    kAssign,
    kEq,
    kNeq,
    kLt,
    kGt,
    kLe,
    kGe,
    kL_paren,
    kR_paren,
    kL_brace,
    kR_brace,
    kArrow,
    kComma,
    kColon,
    kSemicolon,
    // End of punctuations
    kIntLiteral,
    kDoubleLiteral,
    kEof,        // End of file.
};

const std::string &TokenToString(TokenType type);

struct Position {
    uint64_t pos = 0;

    uint32_t LineNo() const { return pos >> 32u; }
    uint32_t ColNo() const { return pos & 0xffffffffull; }
    void SetLineNo(uint32_t line_no) { pos = (pos & 0xffffffffull) | (static_cast<uint64_t>(line_no) << 32u); }
    void SetColNo(uint32_t col) { pos = (pos & 0xffffffff00000000ull) | col; }
};

struct Token {
    TokenType type;
    std::string lexeme;
    Position pos;
};

class Scanner {
public:
    void ScanFile(const std::string &filename);
    const Token &GetToken();
    const Token &Peek(int i) const;
    const std::string &Filename() const { return filename_; }

private:
    bool ScanToken(Token &tk);
    void ScanDoubleOrInt(Token &tk);
    void ScanIDOrKeyword(Token &tk);
    void ScanAllTokens();
    void SkipSpaceOrComment();
    void Error();

private:
    std::string filename_;
    std::ifstream input_;
    std::vector<Token> tokens_;
    int index_ = 0;
    std::string line_;
    int col_ = 0;
    int line_no_ = 0;

    std::ostringstream error_;
    Token eop_{kEof, ""};
};

#endif // SCANNER_H_
