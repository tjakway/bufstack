if exists('g:bufstack_autoloadfile_loaded')
    finish
endif
let g:bufstack_autoloadfile_loaded = 1

let s:bufstack_plugindir = expand('<sfile>:p:h:h')
let s:bufstack_python_file = s:bufstack_plugindir . "/python/Bufstack.py"

function! Bufstack#set_global_defaults()
    let g:bufstack_max_depth = get(g:, "bufstack_max_depth", 20)
    let g:bufstack_max_b_remappings = get(g:, "bufstack_max_b_remappings", 100)
endfunction


" TODO: add python function prefixes
function! Bufstack#init()
    "calling multiple times has no effect
    if exists("g:bufstack_init_called")
        return
    endif
    let g:bufstack_init_called = 1

    " we check for python in init so we don't bother the user if they're not
    " using this plugin
    call Bufstack#warn_python()
    call Bufstack#set_global_defaults()

    execute 'pyfile' s:bufstack_python_file
    python initialize_bufstack()
endfunc

function! Bufstack#warn_python()
    " warn the user about python if the warning hasn't been suppressed
    if !exists("g:bufstack_python_no_warning")
        if !has('python')
            echohl WarningMsg
            echom  "Bufstack requires python"
            echohl None
        endif
    endif
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

function! Bufstack#push_current_buffer_if_not_top()
    call Bufstack#init()
    " set this variable to allow repeats on top of the stack
    if !exists("g:bufstack_allow_top_repeats")
        python push_current_if_not_top()
    endif
endfunction

function! Bufstack#switch_buffer(which)
    " argument is retrieved in python via vim.eval
    call Bufstack#init()
    python switch_buffer()
endfunction

function! Bufstack#remap_b_number()
    let l:i = 1
    while l:i <= g:bufstack_max_b_remappings
    execute "nnoremap " . l:i . "gb :" . l:i . "b\<CR>"
    let l:i += 1
    endwhile
endfunction

" see http://stackoverflow.com/questions/5559029/quickly-switching-buffers-in-vim-normal-mode
function! Bufstack#switch_to_next_buffer(incr)
  let help_buffer = (&filetype == 'help')
  let current = bufnr("%")
  let last = bufnr("$")
  let new = current + a:incr
  while 1
    if new != 0 && bufexists(new) && ((getbufvar(new, "&filetype") == 'help') == help_buffer)
      execute ":buffer ".new
      break
    else
      let new = new + a:incr
      if new < 1
        let new = last
      elseif new > last
        let new = 1
      endif
      if new == current
        break
      endif
    endif
  endwhile
endfunction
"nnoremap <silent> <C-n> :call SwitchToNextBuffer(1)<CR>
"nnoremap <silent> <C-p> :call SwitchToNextBuffer(-1)<CR>

function! Bufstack#map_default_keybindings()

endfunction

