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


#can't call it test_something or nosetests will try to run it
def bufstack_assert_rpc_call(rpc_function_name, before_state, after_state):
    plugin = BufstackPlugin(vim)

    def setup_cb():
        before_state(plugin)
        cmd = 'let g:result = rpcrequest({}, "{}")'.format(cid, rpc_function_name)
        vim.command(cmd)
        vim.stop_loop()

    def request_cb(name, args):
        eq(name, rpc_function_name)

        #stop the loop so the message gets processed
        vim.stop_loop()
        after_state(plugin)

    vim.run_loop(request_cb, None, setup_cb)

@with_setup(setup=cleanup)
def bufstack_assert_rpc_call():
    test_bufstack_rpc_call("BufstackPerTabPageStacks",
            lambda plugin: eq(plugin.identify_tab_pages, False),
            lambda plugin: eq(plugin.identify_tab_pages, True))
