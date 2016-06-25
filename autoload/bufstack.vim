if !has('python')
	finish
endif

if exists('g:bufstack_autoloadfile_loaded')
    finish
endif
let g:bufstack_autoloadfile_loaded = 1

let Bufstack#plugindir = expand('<sfile>:p:h:h')
let Bufstack#python_file = s:bufstack_plugindir . "/python/Bufstack.py"

function! Bufstack#init()
   execute 'pyfile' Bufstack#python_file
   python initialize_bufstack()
endfunc

function! Bufstack#prev_buf()
    python prev_buf()
endfunc

function! Bufstack#next_buf()
    python next_buf()
endfunc

function! Bufstack#gt_buf_no_push()
    python gt_buf_no_push()
endfunc

function! Bufstack#lt_buf()
    python lt_buf()
endfunc

function! Bufstack#lt_buf_no_push()
    python lt_buf_no_push()
endfunc

function! Bufstack#remove_all_stacks()
    python remove_all_stacks()
endfunc

function! Bufstack#show_buffer_stack()
    python show_buffer_stack()
endfunc

function! Bufstack#push_current_buffer()
    python push_current_buffer()
endfunc

" initialize the plugin
call Bufstack#init()

