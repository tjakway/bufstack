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
#include "CommonDefines.hpp"

#include <algorithm>

#include <cstdlib>
#include <cstring>

#include <sys/types.h>
#include <sys/stat.h>
#include <dirent.h>


BUFSTACK_BEGIN_NAMESPACE

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

std::vector<std::string> FindNeovim::getPathEntries()
{
    char* path = GETENV_FUNC(PATH_VAR_NAME);
    if(path == nullptr)
    {
        throw NoPathVariable(STRCAT("Error in ", __func__));
    }
    else
    {
        std::string pathStr(path);
        std::vector<std::string> pathEntries;

        auto pos = pathStr.find(PATH_VAR_SEPARATOR);
        while(pos != std::string::npos)
        {
            //this substring includes the path separator
            //need to remove it manually
            std::string thisEntry = pathStr.substr(0, pos);
            auto endOfEntrySeparatorPos = thisEntry.find(PATH_VAR_SEPARATOR);
            if(endOfEntrySeparatorPos != std::string::npos)
            {
                thisEntry.erase(thisEntry.begin() + endOfEntrySeparatorPos, thisEntry.end());
            }

            pathEntries.push_back(thisEntry);
        }

        return pathEntries;
    }
}

std::vector<std::string> FindNeovim::getFilesInDirectory(const std::string& dirPath)
{
    DIR* dir = opendir(dirPath.c_str());
    if(dir == nullptr)
    {
        throw DirectoryException(STRCAT("Error in ", __func__, " in call to opendir: ", 
                    strerror(errno)));
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

std::unique_ptr<std::string> FindNeovim::getFirstOnPath(std::string target)
{
    for(const std::string& pathEntry : getPathEntries())
    {
        std::vector<std::string> contents = getFilesInDirectory(pathEntry);
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