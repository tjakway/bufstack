#pragma once

//pretty sure getenv will take "PATH" on both platforms...
//try %PATH% on windows if that doesn't work
#define PATH_VAR_NAME "PATH"
#ifdef _WIN32
#define PATH_VAR_SEPARATOR ";"
#define PATH_SEPARATOR "\\"
#else
#define PATH_VAR_SEPARATOR ":"
#define PATH_SEPARATOR "/"
#endif


