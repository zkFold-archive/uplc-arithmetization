#include "eval.h"

#include <iostream>
#include <cassert>

namespace translator {

const std::unordered_map<std::string, size_t> Eval::builtins = {
    {"addInteger", 0},
    {"subtractInteger", 1},
    {"multiplyInteger", 2},
    {"divideInteger", 3},
    {"quotientInteger", 4},
    {"remainderInteger", 5},
    {"modInteger", 6},
    {"equalsInteger", 7},
    {"lessThanInteger", 8},
    {"lessThanEqualsInteger", 9},
    {"appendByteString", 10},
    {"consByteString", 11},
    {"sliceByteString", 12},
    {"lengthOfByteString", 13},
    {"indexByteString", 14},
    {"equalsByteString", 15},
    {"lessThanByteString", 16},
    {"lessThanEqualsByteString", 17},
    {"sha2_256", 18},
    {"sha2_256", 19},
    {"blake2b_256", 20},
    {"verifyEd25519Signature", 21},
    {"appendString", 22},
    {"equalsString", 23},
    {"encodeUtf8", 24},
    {"decodeUtf8", 25},
    {"ifThenElse", 26},
    {"chooseUnit", 27},
    {"trace", 28},
    {"fstPair", 29},
    {"sndPair", 30},
    {"chooseList", 31},
    {"mkCons", 32},
    {"headList", 33},
    {"tailList", 34},
    {"nullList", 35},
    {"chooseData", 36},
    {"constrData", 37},
    {"mapData", 38},
    {"listData", 39},
    {"iData", 40},
    {"bData", 41},
    {"unConstrData", 42},
    {"unMapData", 43},
    {"unListData", 44},
    {"unIData", 45},
    {"unBData", 46},
    {"equalsData", 47},
    {"mkPairData", 48},
    {"mkNilData", 49},
    {"mkNilPairData", 50},
    {"serialiseData", 51},
    {"verifyEcdsaSecp256k1Signature", 52},
    {"verifySchnorrSecp256k1Signature", 53},
};

size_t Eval::eval(std::shared_ptr<Ast> ast)
{
    if (!ast) {
        return 0;
    }

    size_t addr = terms.size();
    switch (ast->type) {
        case Type::integer: {
            auto integer = std::static_pointer_cast<Integer>(ast);
            if (ints.count(integer->value) == 0) {
                ints[integer->value] = consts.size();
                consts.push_back({0, integer->value, 0});
            }
            terms.push_back({1, ints[integer->value], 0});
            break;
        } case Type::builtin: {
            auto builtin = std::static_pointer_cast<Builtin>(ast);
            addr = terms.size();
            terms.push_back({6, builtins.at(builtin->name), 0});
            break;
        } case Type::apply: {
            auto apply = std::static_pointer_cast<Apply>(ast);
            terms.push_back({5, 0, 0});
            size_t left_addr = eval(apply->lhs);
            size_t right_addr = eval(apply->rhs);
            terms[addr] = {5, left_addr, right_addr};
            break;
        } case Type::lambda: {
            auto lambda = std::static_pointer_cast<Lambda>(ast);
            terms.push_back({2, 0, 0});
            size_t var_addr = var_hasher(lambda->var);
            size_t def_addr = eval(lambda->term);
            terms[addr] = {2, var_addr, def_addr};
            break;
        } case Type::variable: {
            auto variable = std::static_pointer_cast<Variable>(ast);
            terms.push_back({0, var_hasher(variable->name), 0});
            break;
        } case Type::force: {
            auto force = std::static_pointer_cast<Force>(ast);
            terms.push_back({4, 0, 0});
            size_t term_addr = eval(force->term);
            terms[addr] = {4, term_addr, 0};
            break;
        } case Type::delay: {
            auto delay = std::static_pointer_cast<Delay>(ast);
            terms.push_back({3, 0, 0});
            size_t term_addr = eval(delay->term);
            terms[addr] = {3, term_addr, 0};
            break;
        } default: {
            assert(0);
        }
    }
    return addr;
}

}
