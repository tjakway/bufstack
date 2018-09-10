#include "FindNeovim.hpp"

#if defined(__GNUC__) && !defined _GNU_SOURCE
#warning "__GNUC__ is defined but _GNU_SOURCE is not--you likely need to add a _GNU_SOURCE define somewhere"
#endif

#ifdef _GNU_SOURCE
#define GETENV_FUNC secure_getenv
#else
#define GETENV_FUNC getenv
#endif


#include "Util/Strcat.hpp"
#include "Util/Util.hpp"
#include "Util/SplitString.hpp"
#include "Loggable.hpp"

#include "CommonDefines.hpp"

#include <algorithm>
#include <iostream>

#include <cstdlib>
#include <cstring>

#include <sys/types.h>
#include <sys/stat.h>
#include <dirent.h>


BUFSTACK_BEGIN_NAMESPACE

bool FindNeovim::dontWarn(const std::string& path)
{
    //don't warn for directories with blank names (probably bogus path entries)
    return Util::StringTrim::trim_copy(path).empty();
}

bool FindNeovim::isDirectory(const std::string& path)
{
    //see https://stackoverflow.com/questions/3828192/checking-if-a-directory-exists-in-unix-system-call
    struct stat sb;

    if (stat(path.c_str(), &sb) == 0 && S_ISDIR(sb.st_mode))
    {
        return true;
    }
    else
    {
        return false;
    }
}

std::vector<std::string> FindNeovim::getPathEntries(Loggable& logger)
{
    const char* path = GETENV_FUNC(PATH_VAR_NAME);
    if(path == nullptr)
    {
        throw NoPathVariable(STRCAT("Error in ", __func__));
    }
    else
    {
        std::string pathStr(path);
        if(Util::stringIsEmpty(path))
        {
            throw NoPathVariable("Path variable is empty");
        }

        std::vector<std::string> pathEntries = 
            split(pathStr, PATH_VAR_SEPARATOR);
        const auto pathEntriesSize = pathEntries.size();
        if(pathEntriesSize == 0)
        {
            logger.getLogger()->warn("Your path variable (%s) doesn't "
                    "seem to have any entries", pathStr);
        }

        //filter bad path entries
        std::remove_if(pathEntries.begin(), pathEntries.end(), [](const std::string& p){
                const auto x = Util::StringTrim::trim_copy(p);
                return x.empty() || !isDirectory(x);
                });
        const auto newSize = pathEntries.size();
        logger.getLogger()->debug("Filtered {} path entries", pathEntriesSize - newSize);

        if(pathEntries.empty())
        {
            logger.getLogger()->warn("Your path variable (%s) doesn't "
                    "seem to have any valid entries", pathStr);
        }

        return pathEntries;
    }
}

std::vector<std::string> FindNeovim::getFilesInDirectory(const std::string& dirPath,
        Loggable& logger)
{
    DIR* dir = opendir(dirPath.c_str());
    if(dir == nullptr)
    {
        if(!dontWarn(dirPath))
        {
            logger.getLogger()->warn(
                STRCATS("Warning: opendir returned NULL in " << 
                    __func__ << " for " << dirPath <<
                    ", skipping this path entry." <<
                    std::endl << "Reason for error: " <<
                    strerror(errno)));
        }

        return std::vector<std::string>();
    }
    else
    {
        std::vector<std::string> files;
    

        dirent* ent = nullptr;
        do {
            errno = 0;
            ent = readdir(dir);
            if(ent == nullptr && errno != 0)
            {
                closedir(dir);
                throw DirectoryException(STRCAT("Error in ", __func__, " in call to readdir: ",
                        strerror(errno)));
            }
            //readdir returns null when done
            //no more entries left
            else if(ent == nullptr) {}
            else
            {
                //make sure this entry is a file and not another directory
                std::string entryName(ent->d_name);

                std::string absPath = STRCAT(dirPath, PATH_SEPARATOR, entryName);
                if(!isDirectory(absPath))
                {
                    files.push_back(absPath);
                }
            }
        } while(ent != nullptr);

        closedir(dir);
        return files;
    }
}

const std::string FindNeovim::neovimExeName {"nvim"};


Loggable FindNeovim::logger("FindNeovim");

std::unique_ptr<std::string> FindNeovim::getFirstOnPath(std::string target)
{
    logger.getLogger()->set_level(spdlog::level::debug);
    for(const std::string& pathEntry : getPathEntries(logger))
    {
        std::vector<std::string> contents = getFilesInDirectory(pathEntry, logger);
        auto res = std::find(contents.begin(), contents.end(), target);
        
        //found neovim
        if(res != contents.end())
        {
            //return now so we don't keep looking
            return make_unique<std::string>(STRCAT(pathEntry, PATH_SEPARATOR, *res));
        }
    }

    return std::unique_ptr<std::string>();
}


BUFSTACK_END_NAMESPACE
