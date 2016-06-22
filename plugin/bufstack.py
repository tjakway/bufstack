from __future__ import print_function
import vim

buf_stacks = None

def initialize_bufstack():
    global buf_stacks
    buf_stacks = BufferStackDict()


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

    #delete all stacks except the default and 
    #clear the default stack
    def remove_all_stacks(self):
        self.bufdict = dict()
        self.remake_default()

    #returns None if the stack for the passed window has no valid buffers
    #***WARNING:*** assumes the window is a valid key in the dictionary
    #explicitly pass self.default_key if this is not the case
    def _pop_next_valid_buf(self, window):
        win_stack = self.bufdict[window.number]
        while len(win_stack) > 0:
            #python's pop() with no args returns the _last_ item in the list...
            return_buf = win_stack.pop(0)
            if return_buf.valid:
                return return_buf

        #no valid buffers
        return None
             

    def new_window_stack(self, window):
        if window.number not in self.bufdict:
            self.bufdict[window.number] = list()

    #deletes the windows stack from the dictionary if it has one
    def del_window_stack(self, window):
        if window.number in self.bufdict:
            del self.bufdict[window.number]

    #checks if the window is invalid AND has its own stack and if so deletes that stack
    def remove_if_invalid(self, window):
        if not window.valid:
            self.del_window_stack(window)
        
    def push_buf(self, window, buf):
        if window.number in self.bufdict:
            self.bufdict[window.number].insert(0, buf)
        else:
            self.bufdict[self.default_key].insert(0, buf)

    def pop_buf(self, window):
        #if this window has its own non-empty stack, pop that buffer
        if window.number in self.bufdict and len(self.bufdict[window.number]) > 0:
            return self._pop_next_valid_buf(window)
        #if this window doesn't have its own stack, pop from the default stack
        else:
            return self._pop_next_valid_buf(self.default_key)

    def pop(self, window):
        self.pop_buf(self, window)

    def get_stack_for_window(self, window):
        if window.number in self.bufdict:
            return bufdict[window]

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

def get_gt_buf_num(curr_buf, buf_list):
    _cmp_buf_num(curr_buf, buf_list, max, min, 1)

def get_lt_buf_num(curr_buf, buf_list):
    _cmp_buf_num(curr_buf, buf_list, min, max -1)

def get_gt_buf(curr_buf, buf_list):
    get_buf_with_number(get_gt_buf_num(curr_buf, buf_list))

def get_lt_buf(curr_buf, buf_list):
    get_buf_with_number(get_lt_buf_num(curr_buf, buf_list))

def get_buf_with_number(buf_number, buf_list):
    get_buf_numbers(buf_list)[buf_number]

#get all valid buffers from vim.buffers
def get_valid_bufs():
    valid_bufs = list()
    for i in vim.buffers:
        if i.valid:
            valid_bufs.append(i)
    return valid_bufs

#state modifying functions
#******************************************************


def prev_buf():
    b = buf_stacks.pop(vim.current.window)
    if b is None:
        valid_bufs = get_valid_bufs()
        #if this window has no valid buffers
        #do nothing
        #otherwise, switch to the next one
        if len(valid_bufs) > 1:
            vim.current.buffer = get_buf_with_number(get_gt_buf_num(vim.current.buffer, valid_bufs))
    else:
        #switch to this buffer if we're not already viewing it
        if vim.current.buffer.number != b.number:
            vim.current.buffer = b.number

#checks if there's more than 1 valid buffer and then performs a comparison and changes the current buffer, optionally pushing
#it on the stack first
#cmp_func should be either get_gt_buf_num or get_lt_buf_num
#should_push_buf is a flag to indicate whether the current buffer should be pushed on the stack before changing
def _move_buf_common(cmp_func, should_push_buf):
    valid_bufs = get_valid_bufs()
    if len(valid_bufs) > 1:
        #the buffer we're changing to
        dest_buf = get_buf_with_number(cmp_func(vim.current.buffer, valid_bufs))
        #check whether we should push this buffer on the stack
        if should_push_buf:
            buf_stacks.push_buf(vim.current.window, vim.current.buffer)
        #change the current buffer
        vim.current.buffer = dest_buf

#like gt_buf but pushes the old buffer on the stack
def next_buf():
    _move_buf_common(get_gt_buf_num, True)

#move to the next buffer and dont push the current one on the stack
def gt_buf_no_push():
    _move_buf_common(get_gt_buf_num, False)

#push the current buffer on the stack and move to the next lowest buffer
def lt_buf():
    _move_buf_common(get_lt_buf_num, True)

#like lt_buf but don't push the current buffer
def lt_buf_no_push():
    _move_buf_common(get_lt_buf_num, False)

def remove_all_stacks():
    buf_stacks.remove_all_stacks()

#print the buffer stack for the current window
def show_buffer_stack():
    def buf_str(buf):
        return str(buf.name + "\t #" + b.number)

    output_str = ""
    current_buf_stack = get_valid_bufs(buf_stacks.get_stack_for_window(vim.current.window))
    if len(current_buf_stack) <= 0:
        print("Empty buffer stack.")
    else:
        copied_buf_stack = current_buf_stack[:]
        #format the first line specially
        #the pop is safe because we checked that the list has at least 1 item
        output_str += buf_str(copied_buf_stack.pop()) + "\t<------- TOP" + "\n"

        #add the rest of the buffers to the string
        for i in copied_buf_stack:
            output_str += buf_str(i) + "\n"

        print(output_str)

#useful in vimscript as a primitive combinator
def push_current_buffer():
    buf_stacks.push_buf(vim.current.window, vim.current.buffer)
