#pragma once

#include <iostream>
#include "nonstd/optional.hpp"

namespace nonstd {
    template <typename T>
    std::ostream& operator <<(std::ostream& stream, const optional<T>& o)
    {
        if(o)
        {
            stream << o.value();
        }
        else
        {
            stream << "nullopt";
        }
    }
}
