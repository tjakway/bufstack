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

#include "nonstd/optional.hpp"

using namespace nonstd;

BUFSTACK_BEGIN_NAMESPACE

class ApiParser : public Loggable
{
    const msgpack::object_handle& handle;

    std::unordered_set<NvimFunction> functions;
    std::unordered_set<CustomType> customTypes;

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

        std::unordered_set<CustomType> parseCustomTypes(
                const std::vector<std::reference_wrapper<msgpack::object>>&);

        CustomType parseCustomType(const msgpack::object&);

        ParseFunctions()
            : Loggable("ParseFunctions")
        {}

    } parseFunctions;

public:
    ApiParser(const msgpack::object_handle& _handle)
        : Loggable("ApiParser"), handle(_handle)
    {}

    std::unordered_set<NvimFunction> getFunctions();


};

BUFSTACK_END_NAMESPACE