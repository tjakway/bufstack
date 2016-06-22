if !has('python')
	finish
endif

if exists('g:bufstack_loaded')
    finish
endif
let g:bufstack_loaded = 1

function! s:bufstack_init()
   pyfile bufstack.py 
   python initialize_bufstack()
endfunc

function! s:bufstack_prev_buf()
    python prev_buf()
endfunc

function! s:bufstack_next_buf()
    python next_buf()
endfunc

function! s:bufstack_gt_buf_no_push()
    python gt_buf_no_push()
endfunc

function! s:bufstack_lt_buf()
    python lt_buf()
endfunc

function! s:bufstack_remove_all_stacks()
    python remove_all_stacks()
endfunc

function! s:bufstack_show_buffer_stack()
    python show_buffer_stack()
endfunc

function! s:bufstack_push_current_buffer()
    python push_current_buffer()
endfunc

command! -bar  BufstackInit call s:bufstack_init
