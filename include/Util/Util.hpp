#pragma once

#include <random>
#include <iterator>
#include <array>
#include <functional>
#include <memory>
#include <vector>
#include <sstream>
#include <algorithm>

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
    bool leftIncludesRight(const T& lhs, T& rhs)
    {
        return std::includes(rhs.begin(), rhs.end(),
                lhs.begin(), lhs.end());
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


//from https://stackoverflow.com/questions/5889238/why-is-xor-the-default-way-to-combine-hashes/27952689#27952689
size_t hash_combine( size_t lhs, size_t rhs );
