#pragma once

#ifdef _WIN32
#define PATH_VAR_NAME "%PATH%"
#define PATH_VAR_SEPARATOR ";"
#define PATH_SEPARATOR "\\"
#else
#define PATH_VAR_NAME "PATH"
#define PATH_VAR_SEPARATOR ":"
#define PATH_SEPARATOR "/"
#endif


