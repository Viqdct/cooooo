#include "scanner.h"
#include <iostream>

using namespace std;

int main(int argc, char const *argv[])
{
    if (argc != 2) {
        cout << "Usage: " << argv[0] << " <inputfile>" << endl;
        return 1;
    }

    Scanner scanner;
    scanner.ScanFile(argv[1]);

    while (true) {
        Token tk = scanner.GetToken();
        if (tk.type == kEof) {
            break;
        }

        cout << TokenToString(tk.type);
        if (tk.type == kIdent || tk.type == kIntLiteral || tk.type == kDoubleLiteral) 
            cout << "(" << tk.lexeme << ")";
        cout << " -- (" << tk.pos.LineNo() << ", " << tk.pos.ColNo() << ")" << endl;
    }
    return 0;
}

