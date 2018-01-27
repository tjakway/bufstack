#pragma once

#include "NamespaceDefines.hpp"

#include <iostream>
#include <ostream>
#include <streambuf>

BUFSTACK_BEGIN_NAMESPACE

class Loggable
{
    std::ostream outstream, errstream;

public:
        Loggable() 
        : outstream(std::cout.rdbuf()), 
        errstream(std::cerr.rdbuf())
        {}

        std::ostream& log() { return outstream; }
        std::ostream& info() { return outstream; }
        std::ostream& warn() 
        {  
            errstream << "WARNING:\t";
            return errstream; 
        }
        std::ostream& error() 
        { 
            errstream << "ERROR:\t";
            return errstream; 
        }
};

BUFSTACK_END_NAMESPACE
