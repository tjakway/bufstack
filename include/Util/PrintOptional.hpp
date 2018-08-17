#pragma once

#include <sstream>

#include "nonstd/optional.hpp"

template <typename T>
std::string printOptional(const nonstd::optional<T>& o)
{
    if(o)
    {
        std::ostringstream ss;
        ss << o.value();
        return ss.str();
    }
    else
    {
        return "nullopt";
    }
}
