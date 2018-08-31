#pragma once

#include <random>
#include <iterator>
#include <array>
#include <functional>
#include <memory>
#include <vector>
#include <sstream>
#include <algorithm>
#include <future>
#include <set>
#include <iterator>
#include <utility>

#include <unistd.h>
#include <fcntl.h>

#include "Util/NewExceptionType.hpp"
#include "Util/Strcat.hpp"

class Util
{
    Util() = delete;
public:
    //from https://stackoverflow.com/questions/216823/whats-the-best-way-to-trim-stdstring
    class StringTrim
    {
        StringTrim() = delete;
    public:
        static void ltrim_inplace(std::string&);
        static void rtrim_inplace(std::string&);
        static void trim_inplace(std::string&);

        static std::string ltrim_copy(std::string);
        static std::string rtrim_copy(std::string);
        static std::string trim_copy(std::string);
    };


    //see https://stackoverflow.com/questions/12340695/how-to-check-if-a-given-file-descriptor-stored-in-a-variable-is-still-valid
    static int fd_is_valid(int fd)
    {
        return fcntl(fd, F_GETFD) != -1 || errno != EBADF;
    }


    template <typename T>
    static std::string printVector(std::vector<T> vec, 
            std::string separator = ", ",
            std::string header = "vec { ",
            std::string footer = " }")
    {
        std::ostringstream stream;
        stream << header;

        if(vec.size() > 1)
        {
            for(auto it = vec.begin(); it < (vec.end() - 1); ++it)
            {
                stream << *it << separator;
            }

            //insert the last item without a trailing separator
            stream << vec.back();
        }
        else if(vec.size() == 1)
        {
            //insert the only item without a separator
            stream << vec.front();
        }

        stream << footer;
        return stream.str();
    }

    /**
    * return true if rhs is a subset of lhs
    */
    template<typename T> 
    static bool leftIncludesRight(const T& lhs, const T& rhs)
    {
        return std::includes(rhs.begin(), rhs.end(),
                lhs.begin(), lhs.end());
    }


    template <typename C>
    static std::set<typename C::key_type> getKeys(const C& map)
    {
        std::set<typename C::key_type> keySet;

        //see https://stackoverflow.com/questions/681943/how-can-i-get-a-stdset-of-keys-to-a-stdmap
        std::transform(map.begin(), map.end(),
                std::inserter(keySet, keySet.begin()),
                [](std::pair<typename C::key_type, typename C::value_type> p) {
                    return p.first;
                });

        return keySet;
    }
};

//these functions are included in C++14 but not C++11
template<typename T, typename... Args>
std::unique_ptr<T> make_unique(Args&&... args)
{
    return std::unique_ptr<T>(new T(std::forward<Args>(args)...));
}

template<class T> 
T min(T a, T b)
{
    return (b < a) ? b : a;
}

/**
 * a bind operation for the std::future monad
 */
template <typename T, typename U>
std::future<U> then(std::future<T> input, 
        std::function<U(T)> f, 
        std::launch policy = std::launch::async)
{
    return std::async(policy, [f, input](){
            U res = f(input.get());
            return res;
    });
}


//from https://stackoverflow.com/questions/5889238/why-is-xor-the-default-way-to-combine-hashes/27952689#27952689
size_t hash_combine( size_t lhs, size_t rhs );

template <typename T>
bool ptrs_equal(T p1, T p2)
{
    if(p1 == nullptr && p2 == nullptr)
    {
        return true;
    }
    else if(p1 != nullptr && p2 == nullptr)
    {
        return false;
    }
    else if(p1 == nullptr && p2 != nullptr)
    {
        return false;
    }
    //dereference both and compare
    else
    {
        return *p1 == *p2;
    }
}

template <typename T>
std::size_t hash_ptr(T ptr)
{
    if(ptr == nullptr)
    {
        return std::hash<T>{}(ptr);
    }
    else
    {
        //hash the underlying object
        return std::hash<typename T::element_type>{}(*ptr);
    }
}
