#!/usr/bin/env python
#NOTE: on linux, the kernel only passes the first argument of the shebang line
#everything gets smashed into that one argument so #!/usr/bin/env python -i
#becomes #!/usr/bin/env 'python -i', which fails because 'python -i' isn't an executable
#the workaround to passing -i is using the code module

import subprocess
import msgpack
import code

api_info = subprocess.check_output(["nvim", "--api-info"])

ast = msgpack.unpackb(api_info)
print("Last line << {} >>".format("ast = msgpack.unpackb(api_info)"))

##
#see https://stackoverflow.com/questions/11796474/start-interactive-mode-on-a-specific-script-line
code.interact(local=locals())
