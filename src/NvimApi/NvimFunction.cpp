#include "NvimApi/NvimFunction.hpp"

#include "Util/Strcat.hpp"
#include "Util/Util.hpp"
#include "Util/PrintOptional.hpp"

#include "nonstd/optional.hpp"

BUFSTACK_BEGIN_NAMESPACE


std::string NvimFunction::print(std::string header, 
        //between a field's name and its value
        std::string fieldValueSep,
        //between the field lines themselves
        std::string fieldSep,
        std::string footer) const
{
    std::ostringstream ss;
    ss << header << 
            "name" << fieldValueSep << name << fieldSep <<
            "method" << fieldValueSep << printOptional(method) << fieldSep <<
            "return_type" << fieldValueSep << printOptional(returnType) << fieldSep <<
            "since" << fieldValueSep << printOptional(sinceVersion) << fieldSep <<
            "parameters" << fieldValueSep << Util::printVector(parameters) <<
            footer;

    return ss.str();
}

std::string NvimFunction::printCompact() const
{
    return print("NvimFunction {", ": ", ", ", "}");
}

std::string NvimFunction::printMultiline() const
{
    return print("NvimFunction\n\t", ": ", "\n\t", "");
}


std::ostream& operator <<(std::ostream& stream, const NvimFunction& f)
{
    stream << f.printCompact();
    return stream;
}

BUFSTACK_END_NAMESPACE
