from __future__ import print_function
import vim

class BufferStackDict(object):
    def __init__(self):
        self.bufdict = dict()
        self.default_key = "default"
        self.remake_default()

    #if the default key isn't in the dictionary,
    #add it with an empty stack
    def remake_default(self):
        if not self.default_key in self.bufdict:
            self.bufdict[self.default_key] = list()

    #returns None if the stack for the passed window has no valid buffers
    #***WARNING:*** assumes the window is a valid key in the dictionary
    #explicitly pass self.default_key if this is not the case
    def _pop_next_valid_buf(self, window):
        while len(win_stack) > 0:
            #python's pop() with no args returns the _last_ item in the list...
            return_buf = win_stack.pop(0)
            if return_buf.valid:
                return return_buf

        return None
             

    def new_window_stack(self, window):
        if window not in self.bufdict:
            self.bufdict[window] = list()

    #deletes the windows stack from the dictionary if it has one
    def del_window_stack(self, window):
        if window in self.bufdict:
            del self.bufdict[window]

    #checks if the window is invalid AND has its own stack and if so deletes that stack
    def remove_if_invalid(self, window):
        if not window.valid:
            self.del_window_stack(window)
        
    def push_buf(self, window, buf):
        if window in self.bufdict:
            self.bufdict[window].insert(0, buf)
        else:
            self.bufdict[self.default_key].insert(0, buf)

    def pop_buf(self, window):
        #if this window has its own non-empty stack, pop that buffer
        if window in self.bufdict and len(self.bufdict[window]) > 0:
            return self._pop_next_valid_buf(window)
        #if this window doesn't have its own stack, pop from the default stack
        else:
            return self._pop_next_valid_buf(self.default_key)

buf_stacks = BufferStackDict()

#returns whichever buffer in a list of size 2 isn't the (passed) current buffer
def get_other_buf(curr_buf, buf_list):
    if len(buf_list) != 2:
        raise "get_other_buf should only be called on a list of 2 buffers!"

    curr_buf_num = curr_buf.number
    for i in buf_list:
        if i.number != curr_buf_num:
            return curr_buf_num

    raise "Current buffer does not exist in passed buffer list!"

def get_buf_numbers(buf_list):
    nums = list()
    for i in buf_list:
        nums.append(i.number)
    return nums

#"op_most" and "op_least" mean functions that return the most extreme value for that type of comparison
#for less than, op_least=min because the min is the most extremely less value
#vice-versa for greater than
#
#single_step is -1 for min and 1 for max
def _cmp_buf_num(curr_buf, buf_list, op_most, op_least, single_step):
    buf_numbers = get_buf_numbers(buf_list)
    this_buf_num = curr_buf.number

    most_num = op_most(buf_numbers)
    #if this is the highest-numbered buffer, wrap around to the lowest
    if this_buf_num == most_num:
        return op_least(buf_numbers)
    else:
        #(for max): if we're not the highest numbered buffer, there's at least one with a higher index
        #(for min): if we're not the lowest numbered buffer, there's at least one with a lower idnex
        sorted_buf_nums = sorted(buf_list)
        #note that this works for a negative index
        return sorted_buf_nums[sorted_buf_nums.index(this_buf_num) + single_step]

def gt_buf_num(curr_buf, buf_list):
    _cmp_buf_num(curr_buf, buf_list, max, min, 1)

def lt_buf_num(curr_buf, buf_list):
    _cmp_buf_num(curr_buf, buf_list, min, max -1)

def get_buf_with_number(buf_number, buf_list):
    get_buf_numbers(buf_list)[buf_number]

#get all valid buffers from vim.buffers
def get_valid_bufs():
    valid_bufs = list()
    for i in vim.buffers:
        if i.valid:
            valid_bufs.append(i)

def next_buf():
    b = buf_stacks.pop(vim.current.window)
    if b is None:
        valid_bufs = get_valid_bufs()
        #this window has no valid buffers
        if len(valid_bufs) > 1:
