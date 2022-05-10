let s:accent_fg = "#ea4298"
call wilder#setup({'modes': [':', '/', '?']})

call wilder#set_option('renderer', wilder#popupmenu_renderer(wilder#popupmenu_border_theme({
      \ 'max_height': 12,
      \ 'border': 'rounded',
      \ 'highlighter': wilder#basic_highlighter(),
      \ 'highlights': {
      \   'default': 'Pmenu',
      \   'accent': wilder#make_hl('WilderAccent', 'Pmenu', [{}, {}, {'foreground': s:accent_fg}]),
      \ },
      \ 'left': [' ', wilder#popupmenu_devicons(), wilder#popupmenu_buffer_flags()],
      \ 'right': [' ', wilder#popupmenu_scrollbar()],
      \ })))


call wilder#set_option('pipeline', [
      \   wilder#branch(
      \     wilder#cmdline_pipeline({
      \       'fuzzy': 1,
      \     }),
      \     wilder#python_search_pipeline(),
      \   ),
      \ ])
