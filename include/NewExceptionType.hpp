#pragma once

#include <string>
#include <sstream>
#include <stdexcept>


//stringification macros
//see https://stackoverflow.com/questions/2653214/stringification-of-a-macro-value
//DON'T name it str() or there WILL be name conflicts!
//any name that no one would write in a regular C/C++ file will do...
#define XSTRINGIFY_MACRO__(a) STRINGIFY_MACRO__(a)
#define STRINGIFY_MACRO__(a) #a


//suppress clang's -Winconsistent-missing-override warning just for 
//NEW_EXCEPTION_TYPE_WITH_BASE because we have no way in the #define
//of telling if we're extending from an exception that was defined with
//NEW_EXCEPTION_TYPE or just a random exception that takes a string
//in its constructor
//this way both will work without spitting out huge numbers of warnings
#ifdef __clang__
#define BEGIN_SUPPRESS_OVERRIDE_WARNING _Pragma("GCC diagnostic push") \
    _Pragma("GCC diagnostic ignored \"-Winconsistent-missing-override\"") \

#define END_SUPPRESS_OVERRIDE_WARNING _Pragma("GCC diagnostic pop")
#else
#define BEGIN_SUPPRESS_OVERRIDE_WARNING 
#define END_SUPPRESS_OVERRIDE_WARNING
#endif

//NOTE: the base class must have a constructor that takes a std::string
#define NEW_EXCEPTION_TYPE_WITH_BASE(A, B) \
    BEGIN_SUPPRESS_OVERRIDE_WARNING \
    class A : public B \
    { \
        const std::string msg;\
        const std::string whatMsg;\
    public: \
        virtual const char* getExceptionTypeName() const noexcept \
            { return XSTRINGIFY_MACRO__(A); } \
            \
    private:\
        std::string getWhatMsg() const noexcept\
        {\
            std::ostringstream os;\
            os << "Exception of type " << \
            getExceptionTypeName() << \
            " thrown: " << msg << std::endl;\
            return os.str();\
        }\
    public: \
        A(const std::string& message) \
            : B(message), msg(message), \
                whatMsg(getWhatMsg())\
        {} \
        \
        virtual const char* what() const noexcept override \
        { \
            return whatMsg.c_str(); \
        }\
    }; \
    END_SUPPRESS_OVERRIDE_WARNING

//runtime_error is a reasonable default base
#define NEW_EXCEPTION_TYPE(A) NEW_EXCEPTION_TYPE_WITH_BASE(A, std::runtime_error)
