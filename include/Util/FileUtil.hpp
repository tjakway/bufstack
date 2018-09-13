#pragma once

#include <ios>
#include <memory>
#include <fstream>
#include <utility>
#include <string>

class FileUtil
{
    FileUtil() = delete;

    using SizeType = unsigned long;
    using CharType = char;

public:
    static std::pair<std::unique_ptr<CharType[]>, SizeType>
        readBinaryFile(const std::string& inputFile)
    {
        std::ifstream in;

        //throw an exception on error
        //see http://www.cplusplus.com/reference/ios/ios/exceptions/
        in.exceptions( std::ifstream::failbit | std::ifstream::badbit);

        //open at end of file to get size
        in.open(inputFile, std::ios::binary | std::ios::in | std::ios::ate);

        SizeType fileSize = in.tellg();

        std::unique_ptr<CharType[]> buf(new CharType[fileSize]);

        //rewind
        in.seekg(std::ios::beg);

        in.read(buf.get(), fileSize);

        return std::make_pair(std::move(buf), fileSize);
    }
};
