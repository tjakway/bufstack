if !has('python')
	finish
endif

if exists('g:bufstack_autoloadfile_loaded')
    finish
endif
let g:bufstack_autoloadfile_loaded = 1

let s:bufstack_plugindir = expand('<sfile>:p:h:h')
let s:bufstack_python_file = s:bufstack_plugindir . "/python/Bufstack.py"

" TODO: add python function prefixes
function! Bufstack#init()
    "calling multiple times has no effect
    if exists("g:bufstack_init_called")
        return
    endif
    let g:bufstack_init_called = 1

   execute 'pyfile' s:bufstack_python_file
   python initialize_bufstack()
endfunc

function! Bufstack#prev_buf()
    call Bufstack#init()
    python prev_buf()
endfunc

function! Bufstack#next_buf()
    call Bufstack#init()
    python next_buf()
endfunc

function! Bufstack#gt_buf_no_push()
    call Bufstack#init()
    python gt_buf_no_push()
endfunc

function! Bufstack#lt_buf()
    call Bufstack#init()
    python lt_buf()
endfunc

function! Bufstack#lt_buf_no_push()
    call Bufstack#init()
    python lt_buf_no_push()
endfunc

function! Bufstack#remove_all_stacks()
    call Bufstack#init()
    python remove_all_stacks()
endfunc

function! Bufstack#show_buffer_stack()
    call Bufstack#init()
    python show_buffer_stack()
endfunc

function! Bufstack#push_current_buffer()
    call Bufstack#init()
    python push_current_buffer()
endfunc
