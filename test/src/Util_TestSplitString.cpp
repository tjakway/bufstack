#include <gtest/gtest.h>

#include "NamespaceDefines.hpp"

#include "Util/Util.hpp"

#include <vector>
#include <algorithm>
#include <sstream>
#include <random>


namespace {
    const char delimiter = ':';
    std::string delimiterStr = std::string({delimiter});

    void splitStringTest(std::vector<std::string> expected,
            std::string input)
    {
        ASSERT_EQ(expected, 
            Util::splitString(input, delimiter));
    }


    //see https://stackoverflow.com/questions/440133/how-do-i-create-a-random-alpha-numeric-string-in-c
    std::vector<char> charset()
    {
        //Change this to suit
        return std::vector<char>( 
        {'0','1','2','3','4',
        '5','6','7','8','9',
        'A','B','C','D','E','F',
        'G','H','I','J','K',
        'L','M','N','O','P',
        'Q','R','S','T','U',
        'V','W','X','Y','Z',
        'a','b','c','d','e','f',
        'g','h','i','j','k',
        'l','m','n','o','p',
        'q','r','s','t','u',
        'v','w','x','y','z'
        });
    };    

    // given a function that generates a random character,
    // return a string of the requested length
    std::string random_string( size_t length, std::function<char(void)> rand_char )
    {
        std::string str(length,0);
        std::generate_n( str.begin(), length, rand_char );
        return str;
    }
}

BUFSTACK_BEGIN_NAMESPACE


TEST(Util_TestSplitString, OnlyDelimiter)
{
    splitStringTest({}, std::string({delimiter}));
}

TEST(Util_TestSplitString, TestTwoItems)
{
    std::string left = "left",
        right = "right";
    splitStringTest({left, right}, 
            left + delimiterStr + right);
}

TEST(Util_TestSplitString, TestNItems)
{
    constexpr auto NUM_ENTRIES_LOWER_LIMIT = 1,
              NUM_ENTRIES_UPPER_LIMIT = 1000,
              ENTRY_LENGTH_LOWER_LIMIT=1,
              ENTRY_LENGTH_UPPER_LIMIT=100;

    std::default_random_engine gen(std::random_device{}());

    std::uniform_int_distribution<> stringEntriesDis(
            NUM_ENTRIES_LOWER_LIMIT, NUM_ENTRIES_UPPER_LIMIT);
    std::uniform_int_distribution<> thisEntryLengthDis(
            ENTRY_LENGTH_LOWER_LIMIT, ENTRY_LENGTH_UPPER_LIMIT);

    const int numEntries = stringEntriesDis(gen);

    std::stringstream genSS;
    std::vector<std::string> entries;
    entries.reserve(numEntries);


    auto ch_set = charset();
    std::uniform_int_distribution<> ch_dis(0, ch_set.size()-1);
    auto randchar = [ ch_set, &ch_dis, &gen ](){
        return ch_set[ ch_dis(gen) ];
    };

    for(int i = 0; i < numEntries; i++)
    {
        const auto thisEntryLength = thisEntryLengthDis(gen);

        const std::string thisEntry = random_string(thisEntryLength, randchar);
        entries.emplace_back(thisEntry);
        genSS << thisEntry << delimiter;
    }
    //no need to remove the last delimiter
   

    std::vector<std::string> substrings = 
        Util::splitString(genSS.str(), delimiter);

    ASSERT_EQ(entries.size(), numEntries);
    ASSERT_EQ(entries, substrings);
}

BUFSTACK_END_NAMESPACE
