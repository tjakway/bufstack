if exists('g:bufstack_pluginfile_loaded')
    finish
endif
let g:bufstack_pluginfile_loaded = 1


" define commands
command! BufstackPrevBuf call Bufstack#prev_buf()
command! BufstackNextBuf call Bufstack#next_buf()
command! BufstackGtBuf call Bufstack#next_buf() " same as BufstackNextBuf
command! BufstackGtBufNoPush call Bufstack#gt_buf_no_push()
command! BufstackLtBuf call Bufstack#lt_buf()
command! BufstackLtBufNoPush call Bufstack#lt_buf_no_push()
command! BufstackRemoveAllStacks call Bufstack#remove_all_stacks()
command! BufstackShowBufferStack call Bufstack#show_buffer_stack()
command! BufstackPushCurrentBuffer call Bufstack#push_current_buffer_if_not_top()
