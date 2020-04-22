colorscheme delek

call plug#begin('~/.local/share/plugged')
Plug 'neovim/nvim-lsp'
Plug 'easymotion/vim-easymotion'
call plug#end()

lua <<EOF
require'nvim_lsp'.rust_analyzer.setup{
  settings = {
    ["rust-analyzer"] = {
      checkOnSave = {
        enable = true;
      },
      diagnostics = {
        enable = false;
      }
    }
  }
}
EOF
set signcolumn=yes " https://github.com/neovim/nvim-lsp/issues/195

nnoremap <silent> gd <cmd>lua vim.lsp.buf.definition()<CR>
