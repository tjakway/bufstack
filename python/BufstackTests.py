import unittest
from Bufstack import BufferStackDict
import random
from random import randint



#emulate a vim buffer's members
class MockBuffer:
    def __init__(self, number, valid):
        self.number = number
        self.valid = valid

    #returns the passed number of buffers with random numbers 
    #(i.e. the length of the list is known but the content is random)
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
        self.mock_window = MockWindow(-1)

    
    #return a randomly size list containing buffers with random numbers
    #(random length and random content)
    def get_rand_buffers_list(self):
        return MockBuffer.RandBuffers(randint(0, self.max_rand_buffers), self.max_rand_buffers)

    def push_rand_bufs(self, num_rand_buffers):
        for i in MockBuffer.RandBuffers(num_rand_buffers, self.max_rand_buffers):
            self.bufstack.push(self.mock_window, i)

    def get_mock_window_bufs(self):
        return self.bufstack.public_get_stack_for_window(self.mock_window)

class PushValidBufsTest(BufstackTest):
    def setUp(self): 
        BufstackTest.setUp(self)

    def runTest(self):
        self.push_rand_bufs(100)
        self.assertEqual(len(self.bufstack.public_get_stack_for_window(self.mock_window)), 100)

class RandPushPopsTest(BufstackTest):
    def setUp(self): 
        BufstackTest.setUp(self)

    def runTest(self):
        pass

class PushInvalidBuffersTest(BufstackTest):
    def setUp(self):
        BufstackTest.setUp(self)

    def runTest(self):
        def random_valid(mock_buf):
            mock_buf.valid = random.choice([True, False])
            return mock_buf

        #generate the list of buffers such that it contains a random number of invalid buffers
        rand_bufs = map(
                lambda mock_buf: random_valid(mock_buf), 
                self.get_rand_buffers_list())

        num_valid_bufs = len([x for x in rand_bufs if x.valid])

        for i in rand_bufs:
            self.bufstack.push(self.mock_window, i)

        self.assertEqual(len(self.get_mock_window_bufs()), num_valid_bufs)
