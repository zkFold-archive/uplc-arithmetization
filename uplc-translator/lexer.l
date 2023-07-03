#include "lexer.h"

namespace translator {

%%{
    machine lexer;
    main := |*
        digit+"."digit+"."digit+ => {
            ret = Parser::token::VERSION;
            Parser::semantic_type str(std::string(ts, te));
            val->move<std::string>(str);
            fbreak;
        };

        "-"?(digit)+ => {
            ret = Parser::token::NUM;
            Parser::semantic_type num(strtol(std::string(ts, te).c_str(), 0, 10));
            val->move<int>(num);
            fbreak;
        };

        'integer' => { ret = Parser::token::INTEGER; fbreak; };
        'bytestring' => { ret = Parser::token::BYTESTRING; fbreak; };
        'list' => { ret = Parser::token::LIST; fbreak; };
        'pair' => { ret = Parser::token::PAIR; fbreak; };

        'force' => { ret = Parser::token::FORCE; fbreak; };
        'delay' => { ret = Parser::token::DELAY; fbreak; };
        'lam' => { ret = Parser::token::LAM; fbreak; };
        'builtin' => { ret = Parser::token::BUILTIN; fbreak; };
        'program' => { ret = Parser::token::PROGRAM; fbreak; };
        'con' => { ret = Parser::token::CON; fbreak; };

        alpha.alnum* => {
            ret = Parser::token::NAME;
            Parser::semantic_type str(std::string(ts, te));
            val->move<std::string>(str);
            fbreak;
        };

        '(' => { ret = Parser::token::LPAR; fbreak; };
        ')' => { ret = Parser::token::RPAR; fbreak; };

        '[' => { ret = Parser::token::LBR; fbreak; };
        ']' => { ret = Parser::token::RBR; fbreak; };

        '<' => { ret = Parser::token::LTK; fbreak; };
        '>' => { ret = Parser::token::RTK; fbreak; };

        '\n';

        space;
    *|;
    write data;
}%%


Lexer::Lexer(const char *p, const char *pe)
    : p(p), pe(pe), eof(pe)
{ 
    %%write init;
}

Parser::token_type Lexer::lex(Parser::semantic_type* val)
{
    Parser::token_type ret = Parser::token::END;
    %%write exec;

    if (ret == Parser::token::END && p != pe && te != pe) {
        std::cerr << "Unexpected input: \"" << std::string(te, pe) << "\"" << std::endl;
        ret = Parser::token::ERROR;
    }

    return ret;
}

}
