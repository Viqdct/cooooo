#include <iostream>

#include "analyzer.h"
#include "compiler.h"

using namespace std;

int main(int argc, char const *argv[]) {
    if (argc != 3) {
        cout << "Usage: " << argv[0] << " <input> <output>" << endl;
        return 1;
    }

    Parser parser;
    Ptr<ProgramNode> program = parser.ParseFile(argv[1]);

    TypeChecker checker(parser.Filename());
    program->Accept(checker);

    std::ofstream out(argv[2]);

    if (!out.is_open()) {
        cout << "Cannot open the file " << argv[2] << endl;
    }
    Compiler compiler(out);
    compiler.Compile(program.get());

    cout << "No errors found" << endl;

    return 0;
}