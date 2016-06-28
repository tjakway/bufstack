import unittest
from Bufstack import BufferStackDict
from random import randint



#emulate a vim buffer's members
class MockBuffer:
    def __init__(self, number, valid):
        self.number = number
        self.valid = valid

    @classmethod
    def RandBuffers(cls, num, upper_bound):
        rand_numbers = list()
        for i in range(num):
            rand_num = randint(0, upper_bound)
            #make sure this number is unique since we should never have 2 buffers with the same number
            while rand_num in rand_numbers:
                rand_num = randint(0, upper_bound)
            rand_numbers.insert(0, rand_num)

        #construct a list of valid MockBuffers from the random numbers
        return map(lambda n: MockBuffer(n, True), rand_numbers)

class MockWindow:
    def __init__(self, number):
        self.number = number


class BufstackTest(unittest.TestCase):
    def setUp(self):
        self.bufstack = BufferStackDict()
        self.max_rand_buffers = 200   

    def push_rand_bufs(self, num_rand_buffers):
        for i in MockBuffer.RandBuffers(num_rand_buffers, self.max_rand_buffers):
            self.bufstack.push(MockWindow(-1), i)

class PushValidBufsTest(BufstackTest):
    def setUp(self): 
        BufstackTest.setUp(self)

    def runTest(self):
        self.push_rand_bufs(100)
        self.assertEqual(len(self.bufstack.get_stack_for_window(MockWindow(-1))), 100)

class RandPushPopsTest(BufstackTest):
    def setUp(self): 
        BufstackTest.setUp(self)

    def runTest(self):
        pass


class DefaultIndexTests(BufstackTest):
    def setUp(self): 
        BufstackTest.setUp(self)

    def runTest(self):
         
        for buf, win in (MockBuffer.RandBuffers(100, self.max_rand_buffers), )
