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

public:
    static std::pair<std::unique_ptr<unsigned char[]>, SizeType>
        readBinaryFile(const std::string& filename)
    {
        ifstream in;

        //throw an exception on error
        //see http://www.cplusplus.com/reference/ios/ios/exceptions/
        in.exceptions( std::ifstream::failbit | std::ifstream::badbit);

        //open at end of file to get size
        in.open(inputFile, ios::binary | ios::in | ios::ate);

        SizeType fileSize = in.tellg();

        std::unique_ptr<unsigned char[]> buf(new unsigned char[fileSize]);

        //rewind
        in.seekg(std::ios::beg);

        in.read(buf.get(), fileSize);

        return std::make_pair(std::move(buf), fileSize);
    }
};
