#include "Util/Util.hpp"

#include <string>


//see https://stackoverflow.com/questions/216823/whats-the-best-way-to-trim-stdstring
#include <algorithm> 
#include <cctype>
#include <locale>

// trim from start (in place)
void Util::StringTrim::ltrim_inplace(std::string &s) {
    s.erase(s.begin(), std::find_if(s.begin(), s.end(), [](int ch) {
        return !std::isspace(ch);
    }));
}

// trim from end (in place)
void Util::StringTrim::rtrim_inplace(std::string &s) {
    s.erase(std::find_if(s.rbegin(), s.rend(), [](int ch) {
        return !std::isspace(ch);
    }).base(), s.end());
}

// trim from both ends (in place)
void Util::StringTrim::trim_inplace(std::string &s) {
    ltrim_inplace(s);
    rtrim_inplace(s);
}

// trim from start (copying)
std::string Util::StringTrim::ltrim_copy(std::string s) {
    ltrim_inplace(s);
    return s;
}

// trim from end (copying)
std::string Util::StringTrim::rtrim_copy(std::string s) {
    rtrim_inplace(s);
    return s;
}

// trim from both ends (copying)
std::string Util::StringTrim::trim_copy(std::string s) {
    trim_inplace(s);
    return s;
}
