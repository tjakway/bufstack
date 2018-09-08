#pragma once

#include "Util/NewExceptionType.hpp"

#include <future>

/**
 * provides atomic access to an underlying type via lambdas
 */
template <typename T>
class AtomicAccess
{
    std::unique_ptr<T> val;
    NEW_EXCEPTION_TYPE(AtomicAccessException);

    std::mutex mut;

    void checkVal()
    {
        if(!val)
        {
            throw AtomicAccessException("Underlying value is null");
        }
    }

public:
    template <typename U>
    U access(std::function<U(T&)> f)
    {
        std::lock_guard<std::mutex> {mut};
        checkVal();
        return f(*val);
    }

    template <typename U>
    U access(std::function<U(T*)> f)
    {
        return access([&f](T* t) { return f(*t); });
    }

    /*
    template <typename U>
    U access(std::function<U(const T&)> f)
    {
        std::lock_guard<std::mutex> {mut};
        return f(*val);
    }
    */

    T get()
    {
        std::lock_guard<std::mutex> {mut};
        return *val;
    }

    AtomicAccess(T _val)
        : val(new T(_val))
    {}

};
