#pragma once

#include "NamespaceDefines.hpp"
#include "Loggable.hpp"
#include "Util/NewExceptionType.hpp"
#include "Util/Util.hpp"
#include "NvimApi/NvimFunction.hpp"
#include "NvimApi/CustomType.hpp"

#include <msgpack.hpp>

#include <functional>
#include <map>
#include <set>
#include <vector>
#include <unordered_set>
#include <string>
#include <algorithm>
#include <memory>

#include "nonstd/optional.hpp"

using namespace nonstd;

BUFSTACK_BEGIN_NAMESPACE

class ApiParser : public Loggable
{
    using CustomTypePtr = std::shared_ptr<CustomType>;

    class CustomTypeCmp
    {
        bool operator()(CustomTypePtr p1, CustomTypePtr p2) const
        {
            return ptrs_equal(p1, p2);
        }
        bool operator()(const CustomTypePtr& p1, const CustomTypePtr& p2) const
        {
            return ptrs_equal(p1, p2);
        }
    };
    class CustomTypeHash
    {
        std::size_t operator()(CustomTypePtr p) const
        {
            return hash_ptr(p);
        }

        std::size_t operator()(const CustomTypePtr& p) const
        {
            return hash_ptr(p);
        }
    };

    using CustomTypeSet = std::unordered_set<
        CustomTypePtr, 
        CustomTypeHash,
        CustomTypeCmp>;

    std::unordered_set<NvimFunction> functions;
    CustomTypeSet customTypes;

    class Keys
    {
    public:
        //TODO: a macro would probably make declaring this easier
        class ApiInfo
        {
        public:
            static const std::string functions,
                                     types,
                                     version,
                                     errorTypes;

            static const std::set<std::string> keys;
        };

        class Function
        {
        public:
            static const std::string returnType,
                                     since,
                                     deprecatedSince,
                                     method,
                                     parameters,
                                     name;

            //not every object needs to have every key
            static std::set<std::string> getMandatoryKeys();
            static std::set<std::string> getAllKeys();

        };

        class Type
        {
        public:
            static const std::string id, prefix;

            static const std::set<std::string> keys;
        };
    };

protected:
    NEW_EXCEPTION_TYPE(ApiParserException);
    NEW_EXCEPTION_TYPE_WITH_BASE(ParseApiInfoException, ApiParserException);
    NEW_EXCEPTION_TYPE_WITH_BASE(ParseFunctionException, ApiParserException);
    NEW_EXCEPTION_TYPE_WITH_BASE(ParseCustomTypeException, ApiParserException);

    void parseApiInfo(const std::vector<msgpack::object_handle>&);

    class ParseFunctions : public Loggable
    {
    public:
        bool keysAreApiInfo(const std::set<std::string>&);
        bool keysAreFunctionObject(const std::set<std::string>&);

        std::vector<std::string> extractFunctionNames(const msgpack::object&);

        std::unordered_set<NvimFunction> parseNvimFunctions(
         const std::vector<std::reference_wrapper<msgpack::object>>&);

        NvimFunction parseNvimFunction(const msgpack::object&);

        CustomTypeSet parseCustomTypes(
                const std::map<std::string, msgpack::object>&);

        std::shared_ptr<CustomType> parseCustomType(const std::string&, const msgpack::object&);

        ParseFunctions()
            : Loggable("ParseFunctions")
        {}

    } parseFunctions;

public:
    ApiParser(const std::vector<msgpack::object_handle>&);

    std::unordered_set<NvimFunction> getFunctions();
    CustomTypeSet getCustomTypes();

};

BUFSTACK_END_NAMESPACE
