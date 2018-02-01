from __future__ import print_function
import sys

#TODO: remove after replacing tests with nosetests framework
#tests shouldn't fail if vim can't be imported
try:
    import vim
except:
    pass


#print to stderr
#see http://stackoverflow.com/questions/5574702/how-to-print-to-stderr-in-python
def errprint(*args, **kwargs):
    print(*args, file=sys.stderr, **kwargs)
#*********************************************

buf_stacks = None

def initialize_bufstack():
    global buf_stacks
    buf_stacks = BufferStackDict()

class BufstackException(BaseException):
    pass

class DupException(BufstackException):
    pass

def make_entity_key_function(identify_windows, identify_tab_pages):
    default_key = "default"

    def entity_key_fn(window_object=None, tab_page_object=None):
        identifiers = []
        if identify_windows:
            identifiers.append("win_{}".format(window_object.number))
        if identify_tab_pages:
            identifiers.append("tab_{}".format(tab_page_object.number))

        if len(identifiers) == 0:
            identifiers = [default_key]

        return "_".join(identifiers)

    return entity_key_fn



class BufferStackDict(object):
    """Note that for our purposes "top" means index 0 and "bottom" means the last index
    max_stack_depth = -1 means the stack has no maximum size
    """


    def __init__(self, 
            entity_key_fn,
            logger
            max_stack_depth=-1):

        self.bufdict = dict()
        self.set_max_stack_depth(max_stack_depth)
        self.entity_key_fn = get_entity_key_fn
        self.logger = logger


    def set_max_stack_depth(self, depth):
        self.max_stack_depth = depth
        self.truncate_all_if_too_large()

    def check_entity_stack_depth(self, window=None, tab_page=None):
        self.truncate_if_too_large(self.entity_key_fn(window, tab_page))

    #truncate the stack if it's greater than max_stack_depth
    def truncate_if_too_large(self, key):
        if self.max_stack_depth > 1:
            #TODO: print a warning that we're truncating
            if len(self.bufdict[key]) > self.max_stack_depth:
                #truncate buffer numbers over the limit
                truncated_buffer_list = self.bufdict[key][0:self.max_stack_depth]
                self.bufdict[key] = truncated_buffer_list

    def truncate_all_if_too_large(self):
        for key,_ in self.bufdict.items():
            self.truncate_if_too_large(key)

    #delete all stacks except the default and 
    #clear the default stack
    def remove_all_stacks(self):
        self.bufdict = dict()

    #returns None if the stack for the passed window has no valid buffers
    def _pop_next_valid_buf_for_entity(self, window=None, tab_page=None):
        this_stack = self.get_entity_stack(window, tab_page)

        while len(this_stack) > 0:
            #python's pop() with no args returns the _last_ item in the list...
            return_buf = this_stack.pop()

            if return_buf.valid:
                #found a valid buf--need to modify the underlying stack before we return
                self.set_entity_stack(this_stack)
                return return_buf

        #no valid buffers
        return None
             
    #get the stack associated with this entity, creating a stack if it doesn't exist
    def get_entity_stack(self, window=None, tab_page=None):
        key = self.entity_key_fn(window, tab_page)

        if key not in self.bufdict:
            self.bufdict[key] = list()

        return self.bufdict[key]

    def set_entity_stack(self, new_stack, window=None, tab_page=None):
        key = self.entity_key_fn(window, tab_page)
        self.bufdict[key] = new_stack


    def delete_entity_stack(self, window=None, tab_page=None):
        key = self.entity_key_fn(window, tab_page)
        if key in self.bufdict:
            del self.bufdict[key]


    #checks if the entity is invalid AND has its own stack and if so deletes that stack
    #no-op if we're we're using the default key (since it can't be invalid)
    def remove_if_invalid(self, window=None, tab_page=None):
        if window is not None or tab_page is not None: #if both are none we're using the default key
            self.delete_entity_stack(window, tab_page)
        
    #push the buffer if it's valid
    def push_buf(self, buf, window=None, tab_page=None):
        if buf.valid:
            #modify and replace the entity's buf stack
            buf_stack = self.get_entity_stack(window, tab_page)
            buf_stack.insert(0, buf)
            self.set_entity_stack(bufstack, window, tab_page)

            #check if this stack is too big
            self.check_entity_stack_depth(window, tab_page)

        else:
            logger.warn("Tried to push an invalid buf: {}".format(buf))

    def pop_buf(self, window=None, tab_page=None):
        return self._pop_next_valid_buf_for_entity(window, tab_page)

    def peek_top(self, window=None, tab_page=None):
        stack = self.get_entity_stack(window, tab_page)
        if len(stack) > 0:
            return stack[0]
        else:
            return None

    def remove_invalid_buffers(self, window=None, tab_page=None):
        stack = self.get_entity_stack(window, tab_page)

        #filter the stack for valid buffers
        valid_bufs = [x for x in stack if x.valid]

        #replace the old stack
        self.set_entity_stack(valid_bufs, window, tab_page)

        #return as a convenience
        return valid_bufs

    def dup(self, num_to_copy, window=None, tab_page=None):
        stack = self.get_entity_stack(window, tab_page)

        if num_to_copy <= len(stack):
            #copy the first n items off the top of the stack
            stack = self.get_entity_stack(window, tab_page)
            duplicated_items = stack[0:num_to_copy]

            #mutate the old stack
            new_stack = duplicated_items ++ valid_bufs
            self.set_entity_stack(new_stack, window, tab_page)
        else:
            entity = self.entity_key_fn(window, tab_page)
            raise DupException("Cannot copy more items than exist in the bufstack for entity {}".format(entity))

    def _get_window_key(self, window):
        if not self.separate_window_stacks:
            return self.default_key
        elif window.number in self.bufdict:
            return window.number
        else:
            self.new_window_stack(window.number)
            return window.number

    def public_get_stack_for_window(self, window):
        return self.bufdict[self._get_window_key(window)]

#returns whichever buffer in a list of size 2 isn't the (passed) current buffer
def get_other_buf(curr_buf, buf_list):
    if len(buf_list) != 2:
        raise BufstackException("get_other_buf should only be called on a list of 2 buffers!")

    curr_buf_num = curr_buf.number
    for i in buf_list:
        if i.number != curr_buf_num:
            return curr_buf_num

    raise BufstackException("Current buffer does not exist in passed buffer list!")

def get_buf_numbers(buf_list):
    nums = list()
    for i in buf_list:
        nums.append(i.number)
    return sorted(nums)

#"op_most" and "op_least" mean functions that return the most extreme value for that type of comparison
#for less than, op_least=min because the min is the most extremely less value
#vice-versa for greater than
#
#single_step is -1 for min and 1 for max
def _next_buf_num_cmp(curr_buf, buf_list, op_most, op_least, single_step):
    buf_numbers = get_buf_numbers(buf_list)
    this_buf_num = curr_buf.number

    #if this is the highest-numbered buffer, wrap around to the lowest
    if this_buf_num == op_most(buf_numbers):
        return op_least(buf_numbers)
    else:
        #(for max): if we're not the highest numbered buffer, there's at least one with a higher index
        #(for min): if we're not the lowest numbered buffer, there's at least one with a lower index 
        #sort the buf NUMBERS, not the vim buffer objects
        sorted_buf_nums = sorted(buf_numbers)
        #note that this works for a negative index
        return sorted_buf_nums[sorted_buf_nums.index(this_buf_num) + single_step]

#TODO: double check these aren't called on windows
def get_gt_buf_num(curr_buf, buf_list):
    return _next_buf_num_cmp(curr_buf, buf_list, max, min, 1)

def get_lt_buf_num(curr_buf, buf_list):
    return _next_buf_num_cmp(curr_buf, buf_list, min, max, -1)

def get_gt_buf(curr_buf, buf_list):
    return get_buf_with_number(get_gt_buf_num(curr_buf, buf_list))

def get_lt_buf(curr_buf, buf_list):
    return get_buf_with_number(get_lt_buf_num(curr_buf, buf_list))

def get_buf_with_number(buf_number, buf_list):
    return buf_list[get_buf_numbers(buf_list).index(buf_number)]

#get all valid buffers from the passed list
def get_valid_bufs(buf_list):
    valid_bufs = list()
    for i in buf_list:
        if i.valid:
            valid_bufs.append(i)
    return valid_bufs

#get all valid buffers from vim.buffers
def get_current_valid_bufs():
    return get_valid_bufs(vim.buffers)


#state modifying functions
#******************************************************


def prev_buf():
    b = buf_stacks.pop(vim.current.window)
    if b is None:
        valid_bufs = get_current_valid_bufs()
        #if this window has no other valid buffers, do nothing
        #otherwise, switch to the next one
        if len(valid_bufs) > 1:
            vim.current.buffer = get_buf_with_number(get_lt_buf_num(vim.current.buffer, valid_bufs))
    else:
        #switch to this buffer if we're not already viewing it
        if vim.current.buffer.number != b.number:
            vim.current.buffer = b

#checks if there's more than 1 valid buffer and then performs a comparison and changes the current buffer, optionally pushing
#it on the stack first
#cmp_func should be either get_gt_buf_num or get_lt_buf_num
#should_push_buf is a flag to indicate whether the current buffer should be pushed on the stack before changing
def _move_buf_common(cmp_func, should_push_buf):
    valid_bufs = get_current_valid_bufs()
    if len(valid_bufs) > 1:
        #the buffer we're changing to
        dest_buf = get_buf_with_number(cmp_func(vim.current.buffer, valid_bufs), valid_bufs)
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
        return str(buf.name + "\t #" + str(buf.number))

    output_str = ""
    current_buf_stack = get_valid_bufs(buf_stacks.public_get_stack_for_window(vim.current.window))
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

#VimL binding for bufstack class method
def set_max_stack_depth(depth):
    buf_stacks.set_max_stack_depth(depth)

#push the current buffer if it isn't on top of the current stack
def push_current_buffer_if_not_top():
    curr_buf = vim.current.buffer
    top = buf_stacks.peek_top(vim.current.window)

    #push if it's a valid buffer and not already on top of the stack
    if (curr_buf.valid and top is not None and top.number != curr_buf.number) \
            or (curr_buf.valid and top is None):
        push_current_buffer()

#ARG: which 
def switch_buffer():
    #argument passed through viml
    which_buf = int(vim.eval("a:which"))

    #check if the buffer exists
    #buffwinnr returns -1 if the buffer DNE
    if int(vim.eval("buffwinnr({})".format(str(which_buf)))) == -1:
        errprint("Buffer #{} does not exist!".format(str(which_buf)))
    else:
        #push the current buffer and switch to the new one
        push_current_buffer_if_not_top()
        vim.current.buffer = get_buf_with_number(which_buf)
