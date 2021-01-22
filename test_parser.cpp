#include "parser.h"
#include <iostream>

using namespace std;

int main(int argc, char const *argv[]) {
    if (argc != 2) {
        cout << "Usage: " << argv[0] << " filename" << endl;
        return 1;
    }

    Parser parser;
    Ptr<ProgramNode> program = parser.ParseFile(argv[1]);

    cout << "Scanning and parsing are successful." << endl;
    cout << "\nThe syntax tree:\n\n";

    program->Print(cout);

    return 0;
}
