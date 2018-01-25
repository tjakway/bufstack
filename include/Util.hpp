#pragma once

#include <random>
#include <iterator>
#include <array>
#include <functional>

#include "Util/NewExceptionType.hpp"
#include "Util/Strcat.hpp"

#include "Types.hpp"


class Util
{
    /**
     * this class shouldn't be instantiated
     */
    Util() =delete;
public:
    NEW_EXCEPTION_TYPE(SampleFromException);

    /**
     * sample 1 bit using the passed RNG and the
     * chance that bit is 1
     */
    static bool sampleFrom(rd_ptr, double);

    static double sampleUniformDistributionZeroToOne(rd_ptr);

    template <typename T>
    static T sampleRdInRange(rd_ptr rng, std::pair<T, T> bounds)
    {
        NEW_EXCEPTION_TYPE(BoundsViolationException);
        if(bounds.first > bounds.second) 
        {
            throw BoundsViolationException(
                    STRCAT("Undefined behavior in ", __func__, " caused by passing",
                        " bad bounds (expected first < second but ",
                        "first = ", bounds.first, " and second = ",
                        bounds.second));
        }
        else if(bounds.first == bounds.second)
        {
            //TODO: convert to warning
            throw BoundsViolationException(
                    STRCAT(__func__, " called with bounds lower == upper",
                        " this is probably not what you want"));
        }
        else 
        {
            return std::uniform_int_distribution<T>(bounds.first, bounds.second)(*rng);
        }
    }

    /**
     * from https://stackoverflow.com/questions/6942273/get-random-element-from-container
     */
    template<typename Iter, typename RandomGenerator>
    static Iter selectRandomly(Iter start, Iter end, RandomGenerator& g) {
        std::uniform_int_distribution<> dis(0, std::distance(start, end) - 1);
        std::advance(start, dis(g));
        return start;
    }


    template <typename T>
    static bool deepCompareUniquePtrs(const std::unique_ptr<T>& lhs, const std::unique_ptr<T>& rhs)
    {
        //compare pointer values first
        if(lhs.get() == nullptr && rhs.get() == nullptr)
        {
            return true;
        }
        else if(lhs.get() == nullptr && rhs.get() != nullptr)
        {
            return false;
        }
        else if(lhs.get() != nullptr && rhs.get() == nullptr)
        {
            return false;
        }
        else
        {
            //compare referands
            return *lhs == *rhs;
        }
    }

    template <typename U, typename V, typename It>
    static std::vector<V> accumulateWithVector(It begin, It end, U operation)
    {
        std::vector<V> vec;

        for(; begin != end; ++begin)
        {
            vec.emplace_back(operation(*begin));
        }

        return vec;
    }

    template <typename OldCollection, typename NewCollection>
    static NewCollection mapCollection(
            OldCollection src, 
            std::function<typename NewCollection::value_type(typename OldCollection::value_type)> f)
    {
        NewCollection dst;
        for(auto i : src)
        {
            dst.insert(dst.end(), f(i));
        }
        return dst;
    }

    template <typename T>
    static std::unique_ptr<T> copyUniquePtrIfNotNull(const std::unique_ptr<T>&);

    static const std::array<unsigned char, 16> hashToArray(size_t hash);

    static bool withinMargins(double value, double expected, double margin);
    static std::pair<double, double> getMargins(double expected, double margin);

    static rd_ptr rdSeededWithCurrentTime();
    static rd_ptr rdFromSeed(rd_seed_type);

    static rd_seed_type getCurrentTimeMillis();

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
};


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

