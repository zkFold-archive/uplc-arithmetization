#pragma once

#include <string>
#include <memory>
#include <vector>

namespace translator {

enum class Type {
    apply,
    builtin,
    variable,
    lambda,
    force,
    delay,
    integer,
};

struct Ast
{
    Type type;

    Ast(Type type);
    virtual ~Ast() = default;
};

struct Integer
    : public Ast
{
    int value;

    explicit Integer(int value);
};

struct Builtin
    : public Ast
{
    std::string name;

    explicit Builtin(std::string name);
};

struct Apply
    : public Ast
{
    std::shared_ptr<Ast> lhs;
    std::shared_ptr<Ast> rhs;

    explicit Apply(std::shared_ptr<Ast> lhs, std::shared_ptr<Ast> rhs);
};

struct Lambda
    : public Ast
{
    std::string var;
    std::shared_ptr<Ast> term;

    explicit Lambda(std::string var, std::shared_ptr<Ast> term);
};

struct Variable
    : public Ast
{
    std::string name;

    explicit Variable(std::string name);
};

struct Force
    : public Ast
{
    std::shared_ptr<Ast> term;

    explicit Force(std::shared_ptr<Ast> term);
};

struct Delay
    : public Ast
{
    std::shared_ptr<Ast> term;

    explicit Delay(std::shared_ptr<Ast> term);
};

std::shared_ptr<Ast> new_integer(int value);
std::shared_ptr<Ast> new_builtin(std::string name);
std::shared_ptr<Ast> new_apply(std::shared_ptr<Ast> lhs, std::shared_ptr<Ast> rhs);
std::shared_ptr<Ast> new_lambda(std::string var, std::shared_ptr<Ast> term);
std::shared_ptr<Ast> new_variable(std::string name);
std::shared_ptr<Ast> new_force(std::shared_ptr<Ast> term);
std::shared_ptr<Ast> new_delay(std::shared_ptr<Ast> term);

std::string to_string(std::shared_ptr<Ast> ast);

}
