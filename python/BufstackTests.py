import unittest
from Bufstack import BufferStackDict
from random import randint

#emulate a vim buffer's members
class MockBuffer:
    def __init__(self, number, valid):
        self.number = number
        self.valid = valid

    @classmethod
    def RandBuffers(cls, num):
        #arbitrary, but highly unlikely we'll have >100 buffers
        upper_bound = 100

        if num > upper_bound:
            raise "Will not generate >100 random buffers"

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

class PushValidBufsTest(BufstackTest):
    def setUp(self): 
        BufstackTest.setUp(self)

    def runTest(self):
        num_rand_buffers = 100
        for i in MockBuffer.RandBuffers(num_rand_buffers):
            self.bufstack.push(MockWindow(-1), i)

        self.assertEqual(len(self.bufstack.get_stack_for_window(MockWindow(-1))), num_rand_buffers)


#class DefaultIndexTests(BufstackTest):
#    def setUp(self): 
#        BufstackTest.setUp(self)
#    def runTest(self):
