import unittest
import random
from nose.tools import with_setup, eq_ as eq
from test_common import *
from BufstackPlugin import *

cid = vim.channel_id

@with_setup(setup=cleanup)
def test_identify_windows():
    plugin = BufstackPlugin()
    eq(plugin.identify_windows, False)
    cmd = 'rpcrequest(%d, "BufstackPerWindowStacks")' % cid
    eq(plugin.identify_windows, True)
