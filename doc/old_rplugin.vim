" python3 plugins
call remote#host#RegisterPlugin('python3', '/dpoolfs/git/bufstack/rplugin/python3/BufstackPlugin.py', [
      \ {'sync': v:true, 'name': 'GetBufstackPerTabPageStacks', 'type': 'function', 'opts': {}},
      \ {'sync': v:true, 'name': 'GetBufstackPerWindowStacks', 'type': 'function', 'opts': {}},
      \ {'sync': v:true, 'name': 'BufstackPerTabPageStacks', 'type': 'function', 'opts': {}},
      \ {'sync': v:true, 'name': 'BufstackPerWindowStacks', 'type': 'function', 'opts': {}},
     \ ])


" ruby plugins


" python plugins


