#pragma once

#include <string>

#include "nonstd/optional.hpp"

template <typename T>
std::string printOptional(const nonstd::optional<T>& o)
{
    if(o)
    {
        std::to_string(o.value());
    }
    else
    {
        return "nullopt";
    }
}
