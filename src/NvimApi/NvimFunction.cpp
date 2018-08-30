#include "NvimApi/NvimFunction.hpp"

#include "Util/Strcat.hpp"
#include "Util/Util.hpp"
#include "Util/PrintOptional.hpp"

#include "nonstd/optional.hpp"

#include <utility>
#include <map>

BUFSTACK_BEGIN_NAMESPACE

NvimFunction::Fields NvimFunction::getFields() const noexcept
{
    return Fields {
        std::make_pair("method", printOptional(method)),
        std::make_pair("returnType", printOptional(returnType)),
        std::make_pair("sinceVersion", printOptional(sinceVersion)),
        std::make_pair("name", name),
        std::make_pair("deprecatedSince", printOptional(deprecatedSince)),
        std::make_pair("parameters", Util::printVector(parameters)),
    };
}

std::string NvimFunction::getName() const noexcept
{
    return "NvimFunction";
}


std::ostream& operator <<(std::ostream& stream, const NvimFunction& f)
{
    stream << f.printCompact();
    return stream;
}

BUFSTACK_END_NAMESPACE
