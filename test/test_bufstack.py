import random
import neovim
from nose.tools import with_setup, eq_ as eq
from test_common import *
from BufstackPlugin import *

cid = vim.channel_id

@with_setup(setup=cleanup)
def test_identify_windows():
    plugin = BufstackPlugin(vim)

    def setup_cb():
        eq(plugin.identify_windows, False)
        cmd = 'let g:result = rpcrequest(%d, "BufstackPerWindowStacks")' % cid
        vim.command(cmd)
        vim.stop_loop()

    def request_cb(name, args):
        eq(name, "BufstackPerWindowStacks")

        #stop the loop so the message gets processed
        vim.stop_loop()
        eq(plugin.identify_windows, True)

    vim.run_loop(request_cb, None, setup_cb)

