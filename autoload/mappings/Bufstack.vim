if exists('g:bufstack_mappingsfile_loaded')
    finish
endif
let g:bufstack_mappingsfile_loaded = 1

" put mappings in functions so they can be used piecemeal without forcing
" all the mappings on the user

function! mappings#Bufstack#remap_bn()
    nnoremap <C-n> :bn<CR>
    nnoremap :bn BufstackNextBuf<CR>
endfunction

function! mappings#Bufstack#remap_bp()
    nnoremap <C-p> :bp<CR>
    nnoremap :bp BufstackPrevBuf<CR>
endfunction

function! mappings#Bufstack#map_default_keybindings()
    call Bufstack#init()
    "remap bn and bp

    call mappings#Bufstack#remap_bn()
    call mappings#Bufstack#remap_bp()
endfunction
