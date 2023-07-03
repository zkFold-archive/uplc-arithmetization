#include "parser.hh"
#include "lexer.h"
#include "ast.h"
#include "eval.h"
#include <iostream>
#include <string>
#include <fstream>

#include <unistd.h>

int main(int argc, char **argv)
{
    assert(argc >= 2);
    std::ifstream prog;
    prog.open(argv[1]);
    translator::Eval eval;
    std::string input;

    std::string error;
    std::string consts;
    std::string terms;
    while (!prog.eof()) {
        std::string line;
        std::getline(prog, line);
        input += line;
        if (input.empty()) {
            continue;
        }
        input += '\n';
    }
    prog.close();

    translator::Lexer lexer(input.c_str(), input.c_str() + input.size());
    std::shared_ptr<translator::Ast> ast;
    translator::Parser parser(lexer, ast, error);
    parser.parse();
    if (ast) {
        // std::cout << "====== AST ======" << std::endl;
        // std::cout << to_string(ast) << std::endl;
        // std::cout << "=================" << std::endl;
        eval.eval(ast);
        std::tie(consts, terms) = eval.toString();
    }

    if (!error.empty()) {
        std::cout << "Parse error: " << error << std::endl;
    } else if (argc == 4) {
        std::ofstream consts_json;
        std::ofstream terms_json;
        consts_json.open (argv[2]);
        terms_json.open (argv[3]);
        consts_json << consts;
        terms_json << terms;
        consts_json.close();
        terms_json.close();
    } else if (argc == 2) {
        std::cout << consts << std::endl;
        std::cout << terms << std::endl;
    } else {
        std::cout << "Provide 1 or 3 files" << error << std::endl;
    }
}
