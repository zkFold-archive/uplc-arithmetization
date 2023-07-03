#include "ast.h"

#include <cassert>

namespace translator {

Ast::Ast(Type type)
    : type(type)
{ }

Integer::Integer(int value)
    : Ast(Type::integer), value(value)
{ }

Builtin::Builtin(std::string name)
    : Ast(Type::builtin), name(name)
{ }

Apply::Apply(std::shared_ptr<Ast> lhs, std::shared_ptr<Ast> rhs)
    : Ast(Type::apply), lhs(lhs), rhs(rhs)
{ }

Lambda::Lambda(std::string var, std::shared_ptr<Ast> term)
    : Ast(Type::lambda), var(var), term(term)
{ }


Variable::Variable(std::string name)
    : Ast(Type::variable), name(name)
{ }

Force::Force(std::shared_ptr<Ast> term)
    : Ast(Type::force), term(term)
{ }

Delay::Delay(std::shared_ptr<Ast> term)
    : Ast(Type::delay), term(term)
{ }

std::shared_ptr<Ast> new_integer(int value)
{
    return std::make_shared<Integer>(value);
}

std::shared_ptr<Ast> new_builtin(std::string name)
{
    return std::make_shared<Builtin>(name);
}

std::shared_ptr<Ast> new_lambda(std::string var, std::shared_ptr<Ast> term)
{
    return std::make_shared<Lambda>(var, term);
}

std::shared_ptr<Ast> new_apply(std::shared_ptr<Ast> lhs, std::shared_ptr<Ast> rhs)
{
    return std::make_shared<Apply>(lhs, rhs);
}

std::shared_ptr<Ast> new_variable(std::string name)
{
    return std::make_shared<Variable>(name);
}

std::shared_ptr<Ast> new_force(std::shared_ptr<Ast> term)
{
    return std::make_shared<Force>(term);
}

std::shared_ptr<Ast> new_delay(std::shared_ptr<Ast> term)
{
    return std::make_shared<Delay>(term);
}

std::string to_string(std::shared_ptr<Ast> ast)
{
    if (!ast) {
        return {};
    }

    switch (ast->type) {
        case Type::integer: {
            auto integer = std::static_pointer_cast<Integer>(ast);
            return "(con integer " + std::to_string(integer->value) + ")";
        } case Type::builtin: {
            auto builtin = std::static_pointer_cast<Builtin>(ast);
            return "(builtin " + builtin->name + ")";
        } case Type::apply: {
            auto apply = std::static_pointer_cast<Apply>(ast);
            return "[" + to_string(apply->lhs) + " " + to_string(apply->rhs) + "]";
        } default: {
            assert(0);
        }
    }
}

}
