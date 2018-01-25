#pragma once

#include <random>
#include <iterator>
#include <array>
#include <functional>
#include <memory>

#include "Util/NewExceptionType.hpp"
#include "Util/Strcat.hpp"

//these functions are included in C++14 but not C++11
template<typename T, typename... Args>
std::unique_ptr<T> make_unique(Args&&... args)
{
    return std::unique_ptr<T>(new T(std::forward<Args>(args)...));
}

template<class T> 
const T& min(const T& a, const T& b)
{
    return (b < a) ? b : a;
}


//needs make_unique
template <typename T>
std::unique_ptr<T> Util::copyUniquePtrIfNotNull(const std::unique_ptr<T>& p)
{
    if(p)
    {
        return make_unique<T>(*p);
    }
    else
    {
        //return nullptr
        return std::unique_ptr<T>();
    }
}

