from __future__ import print_function
import sys

#TODO: remove after replacing tests with nosetests framework
#tests shouldn't fail if vim can't be imported
try:
    import vim
except:
    pass


def errprint(*args, **kwargs):
    """print to stderr
    see http://stackoverflow.com/questions/5574702/how-to-print-to-stderr-in-python
    """
    print(*args, file=sys.stderr, **kwargs)

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
            logger,
            max_stack_depth=-1):

        self.bufdict = dict()
        self.set_max_stack_depth(max_stack_depth)
        self.entity_key_fn = entity_key_fn
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
        else:
            self.remove_invalid_buffers(window, tab_page)
            self.truncate_if_too_large(window, tab_page)

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

