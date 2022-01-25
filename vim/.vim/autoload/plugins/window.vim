" Window appearance
" ----------------------------------------------------------------------------

function plugins#window#setup()
  let l:callbacks = []
  if has('nvim-0.5')
    Plug 'nvim-lualine/lualine.nvim'
    " Status line for neovim

    Plug 'akinsho/bufferline.nvim'
    " Buffer line for neovim

    Plug 'kyazdani42/nvim-web-devicons'
    " Dependency for bufferline

    " Plug 'arkav/lualine-lsp-progress'
    " " Show lsp progress
    " Too verbose

    function s:SetupLualine()
lua <<EOF
      local gps_component = nil
      if vim.g.treesitter_langs then
        local gps = require'nvim-gps'
        gps_component = { gps.get_location, cond = gps.is_available }
      end
      require'lualine'.setup {
        options = {
          section_separators = '',
          component_separators = '',
        },
        sections = {
          lualine_b = {'branch', 'diff', {'diagnostics', colored = false}},
          lualine_c = {
            'hostname',
            'filename',
            gps_component,
          },
          lualine_x = {
            function ()
              if vim.bo.filetype ~= "md" and
                 vim.bo.filetype ~= "txt" and
                 vim.bo.filetype ~= "text" and
                 vim.bo.filetype ~= "markdown"
              then
                return ""
              end
              local wc = vim.fn.wordcount()
              local count = wc.visual_words and wc.visual_words or wc.words
              if count == 1 then
                return tostring(count) .. " word"
              else
                return tostring(count) .. " words"
              end
            end,
            'filetype',
            'encoding',
            'fileformat',
          },
        },
        extensions = {
          'fugitive',
          'fzf',
          'fern',
        }
      }
      require'bufferline'.setup {
        options = {
          diagnostics = 'nvim_lsp',
          diagnostics_indicator = function(count, level, diagnostics_dict, context)
            if level:match('error') then
              return '  ' .. count
            end
            return ''
          end,
        },
      }
EOF
    nnoremap <silent>]b :BufferLineCycleNext<CR>
    nnoremap <silent>[b :BufferLineCyclePrev<CR>
    endfunction
    let l:callbacks += [function('s:SetupLualine')]
  else
    Plug 'ourigen/skyline.vim'
    " Lightweight status line that takes the guesswork out of configuration
      let g:skyline_fugitive = 1
      let g:skyline_wordcount = 1
      let g:skyline_linecount = 1
      let g:skyline_bufnum = 0

    Plug 'ap/vim-buftabline'
    " Tab bar at top
      let g:buftabline_indicators = 1 " Show whether modified
      let g:buftabline_numbers  = 1   " Show buffer numbers
  endif

  Plug 'moll/vim-bbye'
  " Delete buffers without messing up buffer layout

  Plug 'AndrewRadev/bufferize.vim'
  " Command contents in buffer

  Plug 'AndrewRadev/linediff.vim'
  " Vimdiff line ranges

  Plug 'Konfekt/FastFold'
  " Lazy folding
    let g:fastfold_fold_movement_commands = []

  if has('nvim-0.5.1')
    Plug 'luukvbaal/stabilize.nvim'
    " Prevent stabilize buffer focus during splits
    function s:SetupStabilize()
      lua require'stabilize'.setup()
    endfunction
    let l:callbacks += [function('s:SetupStabilize')]

    Plug 'petertriho/nvim-scrollbar'
    " Side scrollbar
      augroup scrollbar-highlights
        autocmd!
        autocmd Colorscheme * highlight! link ScrollbarHandle StatusLine
      augroup END
    function s:SetupScrollbar()
lua <<EOF
      require'scrollbar'.setup{}
EOF
    endfunction
    let l:callbacks += [function('s:SetupScrollbar')]
  endif

  return l:callbacks
endfunction
