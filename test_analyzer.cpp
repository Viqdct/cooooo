#include "analyzer.h"
#include <iostream>

using namespace std;

int main(int argc, char const *argv[]) {
    if (argc != 2) {
        cout << "Usage: " << argv[0] << " filename" << endl;
        return 1;
    }

    Parser parser;
    Ptr<ProgramNode> program = parser.ParseFile(argv[1]);

    TypeChecker checker(parser.Filename());
    program->Accept(checker);

    cout << "No errors found" << endl;

    return 0;
}