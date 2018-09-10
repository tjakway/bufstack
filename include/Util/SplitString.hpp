#pragma once

//FROM https://stackoverflow.com/questions/236129/the-most-elegant-way-to-iterate-the-words-of-a-string

#include <string>
#include <sstream>
#include <vector>
#include <iterator>

std::vector<std::string> split(const std::string &s, char delim) 
{
    std::vector<std::string> substrings;
    std::stringstream ss(s);
    std::string item;
    while (std::getline(ss, item, delim)) 
    {
        substrings.emplace_back(item)
    }

    return substrings;
}
