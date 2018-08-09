import random
import neovim
from nose.tools import with_setup, eq_ as eq
from test_common import *
from BufstackPlugin import *




#can't call it test_something or nosetests will try to run it
def bufstack_assert_rpc_call(rpc_function_name, before_state, after_state):
    cid = vim.channel_id
    plugin = BufstackPlugin(vim)

    def setup_cb():
        before_state(plugin)
        cmd = 'let g:result = rpcrequest({}, "{}")'.format(cid, rpc_function_name)
        vim.command(cmd)

    def request_cb(name, args):
        eq(name, rpc_function_name)

        #stop the loop so the message gets processed
        vim.stop_loop()
        after_state(plugin)

    def err_cb(err_msg):
        class BufstackTestException(BaseException):
            pass

        raise BufstackTestException(
                ("Error while testing rpc function << {} >> with message {}" +
                    "\nbefore_state: {},\nafter_state: {}")
                    .format(rpc_function_name, err_msg, before_state, after_state))

    vim.run_loop(request_cb, None, setup_cb, err_cb)

@with_setup(setup=cleanup)
def test_identify_tab_pages():
    bufstack_assert_rpc_call("BufstackPerTabPageStacks",
            lambda plugin: eq(plugin.identify_tab_pages, False),
            lambda plugin: eq(plugin.identify_tab_pages, True))

@with_setup(setup=cleanup)
def test_identify_windows():
    bufstack_assert_rpc_call("BufstackPerWindowStacks",
            lambda plugin: eq(plugin.identify_windows, False),
            lambda plugin: eq(plugin.identify_windows, True))
