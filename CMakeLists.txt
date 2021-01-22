cmake_minimum_required(VERSION 3.15)
project(C0Compiler)

add_executable(compiler
    compiler_main.cpp
    compiler.cpp
    analyzer.cpp
    symbol_table.cpp
    scanner.cpp
    parser.cpp
    ast.cpp
)

add_executable(test_scanner
    test_scanner.cpp
    scanner.cpp
    ast.cpp
)

add_executable(test_parser
    test_parser.cpp
    scanner.cpp
    parser.cpp
    ast.cpp
)

add_executable(test_analyzer
    test_analyzer.cpp
    analyzer.cpp
    symbol_table.cpp
    scanner.cpp
    parser.cpp
    ast.cpp
)
