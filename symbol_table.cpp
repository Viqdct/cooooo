#include "symbol_table.h"

bool SymbolTable::InsertSymbol(const std::string &name, Node *node) {
    auto res = table_.emplace(name, node);
    return res.second;
}

Node *SymbolTable::LookUp(const std::string &name) const {
    auto it = table_.find(name);
    if (it == table_.end())
        return nullptr;

    return it->second;
}
