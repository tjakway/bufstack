#include <gtest/gtest.h>

#include <msgpack.hpp>
#include <functional>
#include <fstream>
#include <ostream>
#include <map>
#include <set>

#include "NamespaceDefines.hpp"
#include "FindNeovim.hpp"
#include "MockMsgpackReaderUnpacker.hpp"
#include "Util/Util.hpp"
#include "NvimApi/ApiParser.hpp"
#include "Loggable.hpp"

#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <cstring>

BUFSTACK_BEGIN_NAMESPACE

class ReadApiInfoTests : public ::testing::Test, public Loggable
{
public:
    int readFd;
    static const std::string apiInfoFilename;

    using Callback = MsgpackReaderUnpacker::Callback;
    using ObjectList = MsgpackReaderUnpacker::ObjectList;

    //can't use assertions in CTOR
    ReadApiInfoTests()
        : Loggable("ReadApiInfoTests"), readFd(-1)
    { }

    virtual void SetUp()
    {
        readFd = open(ReadApiInfoTests::apiInfoFilename.c_str(), O_RDONLY);
        ASSERT_GT(readFd, 0) << "Could not access " << ReadApiInfoTests::apiInfoFilename <<
                ", strerror(errno): " << std::string(strerror(errno));
        return;
    }

    ~ReadApiInfoTests()
    {
        if(Util::fd_is_valid(readFd))
        {
            close(readFd);
        }
    }

    //expect is Callback but returning bool instead of void
    void readExpect(
            std::function<bool(const MsgpackReaderUnpacker::ObjectList&)> expect)
    {
        bool receivedMessage = false;
        bool expected = false;
        const auto callback = 
            [&receivedMessage, &expected, expect]
            (const MsgpackReaderUnpacker::ObjectList& vecH)
        { 
            if(!vecH.empty())
            {
                receivedMessage = true;
            }

            bool passed = expect(vecH);
            if(passed)
            {
                expected = true;
            }
        };

        MockMsgpackReaderUnpacker reader; 
        reader.readFd(readFd, callback);
        EXPECT_TRUE(receivedMessage);
        EXPECT_TRUE(expected);
    }

    static const ApiParser::CustomTypeSet expectedErrorTypes;
    static const ApiParser::CustomTypeSet expectedRegTypes;


    static ApiParser::CustomTypeSet getAllExpectedTypes();
};
const std::string ReadApiInfoTests::apiInfoFilename {"resources/api_info"};

const ApiParser::CustomTypeSet 
    ReadApiInfoTests::expectedErrorTypes {

    std::make_shared<CustomType>(0, "Exception"),
    std::make_shared<CustomType>(1, "Validation")
};

const ApiParser::CustomTypeSet 
    ReadApiInfoTests::expectedRegTypes {

    std::make_shared<PrefixType>(0, "Buffer", "nvim_buf_"),
    std::make_shared<PrefixType>(2, "Tabpage", "nvim_tabpage_"),
    std::make_shared<PrefixType>(1, "Window", "nvim_win_")
};

ApiParser::CustomTypeSet ReadApiInfoTests::getAllExpectedTypes()
{
    ApiParser::CustomTypeSet combine;
    combine.insert(expectedErrorTypes.begin(), expectedErrorTypes.end());
    combine.insert(expectedRegTypes.begin(), expectedRegTypes.end());
    return combine;
}

class MockApiParser : public ApiParser
{
public:
    MockApiParser(const std::vector<msgpack::object_handle>& handles)
        : ApiParser(handles.at(0).get())
    {}

    MockApiParser(const MsgpackReaderUnpacker::ObjectList& handles)
        : ApiParser(handles.at(0).get())
    {}

    static ApiParser::ParseFunctions getParseFunctionsInstance()
    {
        //set the log level before returning
        ApiParser::ParseFunctions parseFunctions;
        parseFunctions.getLogger()->set_level(spdlog::level::critical);
        return parseFunctions;
    }

};

//ApiParser::ParseFunctions MockApiParser::parseFunctions;

//make sure we can decode it without crashing
TEST_F(ReadApiInfoTests, TestCallbackInvoked)
{
    readExpect([](const ObjectList&){ return true; });
}

/*
TEST_F(ReadApiInfoTests, TestExtractFunctionNames)
{
    readExpect([](const std::vector<msgpack::object_handle>& handles){ 
            EXPECT_EQ(handles.size(), 1);

            std::vector<std::string> keys = ApiParser::extractFunctionNames(handles.at(0));
            return keys.size() > 0;
        });
}
*/

/*
TEST_F(ReadApiInfoTests, TestDecodeFile)
{
    //read the entire file into memory

    //ate = open at end
    std::ifstream in(ReadApiInfoTests::apiInfoFilename, 
            std::ios::binary | std::ios::in | std::ios::ate);

    std::ifstream::pos_type pos = in.tellg();

    std::vector<char> buf;
    buf.reserve(pos);

    in.seekg(0, std::ios::beg);
    in.read(buf.data(), pos);

    const auto length = pos;

    //decode msgpack objects
    msgpack::unpacker unpacker;
    unpacker.reserve_buffer(length);
    std::memcpy(unpacker.buffer(), buf.data(), length);
    unpacker.buffer_consumed(length);

    std::vector<msgpack::object_handle> handles;


    bool decodeDone = false;
    while(!decodeDone)
    {
        msgpack::object_handle oh;
        if(!unpacker.next(oh))
        {
            decodeDone = true;
        }
        else
        {
            handles.emplace_back(std::move(oh));
        }
    }

    for(const auto& h : handles)
    {
        std::cout << "TestDecodeFile type:\t" << h.get().type << std::endl
            << "TestDecodeFile object:\t" << h.get() << std::endl;
    }
}
*/

TEST_F(ReadApiInfoTests, TestReadTypeCodes)
{
    /*
    const auto isMapWithTypesKey = [](const msgpack::object_handle& h)
    {
        if(h.get().type == msgpack::type::MAP)
        {
            std::map<std::string, msgpack::object> apiKeyMap;
            h.get().convert(apiKeyMap);

            return true;
            //for(int i = 0; i < apiKeyMap.size; i++)
           // {
                //if(apiKeyMap[i].)
           // }
        }
        return false;
    };

    readExpect([isMapWithTypesKey](const std::vector<msgpack::object_handle>& vecH){
            return std::find_if(vecH.begin(), vecH.end(), 
                    [isMapWithTypesKey](std::vector<msgpack::object_handle>::iterator it){
                        return isMapWithTypesKey(*it); });
            });*/
}

template <typename T> std::vector<typename T::key_type> extractKeys(T map)
{
    std::vector<typename T::key_type> keys;

    for(const auto& thisItem : map)
    {
        keys.emplace_back(thisItem.first);
    }

    return keys;
}

TEST_F(ReadApiInfoTests, TestParseFunctions)
{
    readExpect([](const ObjectList& vecH) -> bool {
            std::map<std::string, msgpack::object> apiInfo;

            vecH.at(0).get().convert(apiInfo);
            const auto keyVec = extractKeys(apiInfo);
            const std::set<std::string> keySet(keyVec.cbegin(), keyVec.cend());

            assert(MockApiParser::getParseFunctionsInstance().keysAreApiInfo(keySet));

            std::vector<msgpack::object> functionObjects;
            apiInfo.at("functions").convert(functionObjects);

            std::vector<std::reference_wrapper<msgpack::object>> refs;
            for(auto& it : functionObjects)
            {
                refs.emplace_back(std::reference_wrapper<msgpack::object>(it));
            }

            std::unordered_set<NvimFunction> functions = 
                MockApiParser::getParseFunctionsInstance()
                    .parseNvimFunctions(refs);
            assert(functions.size() > 0);
            return true;
    });
}


TEST_F(ReadApiInfoTests, TestHasBufferMethods)
{
    const auto isFunctionList = [](const msgpack::object_handle& h) -> bool
    {
        return true;
    };

    readExpect([](const ObjectList& vecH){
            //std::find_if(vecH.begin(), vecH.end(), );
            return true;
            });
}


TEST_F(ReadApiInfoTests, TestParseCustomTypes)
{
    //make sure the constructor doesn't throw any exceptions
    readExpect([this](const ObjectList& vecH) -> bool {
            MockApiParser parser(vecH);
            const auto customTypes = parser.getCustomTypes();
            const auto expectedTypes = this->getAllExpectedTypes();

            //print all members of both sets
            std::ostringstream customTypesSS, expectedTypesSS;
            for(const auto& x : customTypes)
            {
                customTypesSS << "\t" << x->printCompact() << "\n";
            }
            for(const auto& x : expectedTypes)
            {
                expectedTypesSS << "\t" << x->printCompact() << "\n";
            }

            if(!Util::setsEqual(customTypes, expectedTypes))
            {
                this->getLogger()->set_level(spdlog::level::debug);
                this->getLogger()->debug("custom types: \n{}\nexpected types: \n{}",
                    customTypesSS.str(), expectedTypesSS.str());
                return false;
            }
            else
            {
                return true;
            }
            });
}


BUFSTACK_END_NAMESPACE
