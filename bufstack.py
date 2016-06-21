from __future__ import print_function
import vim

class BufferStackDict(object):
    def __init__(self):
        self.bufdict = dict()
        self.default_key = "default"

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

