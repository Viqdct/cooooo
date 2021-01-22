#include <memory>
#include <unordered_map>
#include "ast.h"

class SymbolTable {
public:
    bool InsertSymbol(const std::string &name, Node *node);
    Node *LookUp(const std::string &name) const;

private:
    std::unordered_map<std::string, Node*> table_;
};
