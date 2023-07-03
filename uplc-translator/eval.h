#pragma once

#include "ast.h"

#include <unordered_map>
#include <vector>
#include <tuple>
#include <variant>
#include <iostream>
#include <functional>

namespace translator {

class Eval
{
public:
    Eval() = default;

    size_t eval(std::shared_ptr<Ast> stmt);

    std::tuple<std::string, std::string> toString() {
        return {listToString(consts), listToString(terms)};
    }

private:
    std::string listToString(const std::vector<std::tuple<int, int, int>> &l) {
        std::string ans = "[";
        for(size_t i = 0; i < l.size(); ++i) {
            ans += tripletToString(l[i]);
            if (i + 1 < l.size()) {
                ans += ", ";
            }
        }
        ans += "]";
        return ans;
    }

    std::string tripletToString(const std::tuple<int, int, int> &tr) {
        int a, b, c;
        std::tie(a, b, c) = tr;
        return "[" + std::to_string(a) + ", " + std::to_string(b) + ", " + std::to_string(c) + "]";
    }

    std::hash<std::string> var_hasher;

    std::vector<std::tuple<int, int, int>> consts;
    std::vector<std::tuple<int, int, int>> terms;

    std::unordered_map<int, int> ints;

    const static std::unordered_map<std::string, size_t> builtins;
};

}
