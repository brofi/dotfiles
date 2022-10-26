"Options

"Copy indent from current line when starting a new line.
set autoindent

"Maximum width of text that is being inserted. A longer line will be broken
"after white space to get this width.
"Reformat with: gq{motion} (e.g. gqap) or vap, then gq.
set textwidth=80

"Number of spaces that a <Tab> in the file counts for.
set tabstop=4
"Number of spaces to use for each step of (auto)indent.
set shiftwidth=4
"In Insert mode: Use the appropriate number of spaces to insert a <Tab>.
set expandtab

"While typing a search command, show where the pattern, as it was typed so far,
"matches.
set incsearch
"When there is a previous search pattern, highlight all its matches.
set hlsearch

"Allow buffer to become hidden when abandoned (e.g. when switchwing to next
"buffer). Allows abandoning buffer, even when changes weren't saved. So "think
"twice when using ":q!" or ":qa!".
set hidden

"Complete longest common string and list alternatives.
set wildmode=longest:list
"Ignore files matching these patterns when expanding wildcards and completing
"file or directory names.
set wildignore+=*/.git,*/.svn
set wildignore+=*.aux,*.bbl,*.lof,*.lol,*.lot,*.out,*.toc
set wildignore+=*.class,*.dll,*.exe,*.o,*.obj
set wildignore+=*.bmp,*.gif,*.jpe,*.jpeg,*.jpg,*.png,*.tga,*.tif,*.tiff,*.webp
set wildignore+=*/.@__thumb
set wildignore+=*.iso
set wildignore+=*.swp

"The title of the window will be set to the value of 'titlestring' (if it is not
"empty), or to: filename [+=-] (path) - VIM
set title

"Sets the character encoding.
set encoding=utf8

"Setting dark mode.
set background=dark

"Reduce the timeout for key codes. This is normally controlled with 'timeoutlen'
"if 'ttimeoutlen' < 0 (default: -1). Now 'timeoutlen' can control the mapping
"timeout only, which is left unchanged at 1000 (1 sec / default). This also
"reduces the delay when leaving insert mode with <Esc>. Escaped function and
"arrow keys still work this way (which is not the case with 'noesckeys').
set ttimeoutlen=10

"Airline (no need to modify 'statusline' for e.g. syntastic)
"
"Show status bar, even if there is only one window.
set laststatus=2
"Don't show mode, since airline already shows it.
set noshowmode

"GUI Options {{{

if has('gui_running')
    if has('gui_win32')
        "Windows specific options.
        set guifont=Consolas:h10:cANSI:qDRAFT
    elseif has ('gui_gtk')
        "Linux GUI specific options.
        set guifont=Inconsolata\ 11
    endif
    "GUI window size.
    set lines=40
    set columns=140
    "Disable menu bar.
    set guioptions-=m
    "Disable toolbar.
    set guioptions-=T
    "Disable right-hand scrollbar.
    set guioptions-=r
    "Disable right-hand scrollbar when vertical split (no default).
    set guioptions-=R
    "Disable left-hand scrollbar (no default).
    set guioptions-=l
    "Disable left-hand scrollbar when vertical split.
    set guioptions-=L
    "Disable bottom (horizontal) scrollbar (no default)
    set guioptions-=b
    "Console dialogs instead of popup dialogs for simple choices.
    set guioptions+=c
    "Flash screen instead of sounding a beep, but unset the code that will cause
    "the screen to flash (disables bells completely). Has to be done after the
    "GUI has started.
    autocmd GUIEnter * set visualbell t_vb=
endif

"}}}

"Commands {{{

"We add a '!' to overwrite command. Necessary e.g. when reloading vimrc with
":so ~/.vimrc or :so % (when editing it).

"Custom command to unset number and show fold column to provide a left margin.
command! Nonu set nonu fdc=1
"Custom command to set number and hide fold column to remove the left margin.
command! Nu set nu fdc=0

"Don't show line numbers on start.
exec 'Nonu'

"}}}

"Plugins {{{

" Automatically install plugins.
if has('win32')
    if !empty(glob('~\vimfiles\autoload\plug.vim')) && !isdirectory(glob('~\vimfiles\plugged'))
        autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
    endif
elseif empty(glob('~/.vim/autoload/plug.vim'))
    silent !curl -fLo ~/.vim/autoload/plug.vim --create-dirs
        \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
    autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif

"Automatically calls:
"
" * syntax enable:
"    Enable syntax highlighting.
"
" * filetype plugin indent on:
"    When a file is edited its plugin and indent file is loaded (if there is one
"    for the detected filetype).
"    If filetype detection was not switched on yet, it will be as well.
"
call plug#begin()

"Syntastic: Automatic syntax checking.
Plug 'scrooloose/syntastic'

"Fugitive: A Git wrapper.
Plug 'tpope/vim-fugitive'

"Airline: Lean & meas status/tabline.
Plug 'vim-airline/vim-airline'

"NERDTree: Tree explorer plugin for navigating the filesystem.
Plug 'scrooloose/nerdtree', { 'on': 'NERDTreeToggle' }

"Tagbar: Plugin to browse the tags of the current file and get an overview of
"its structure. Remember to install ctags as well.
Plug 'majutsushi/tagbar',
    \ { 'on': ['TagbarOpen', 'TagbarToggle', 'TagbarOpenAutoClose'] }

"CtrlP: Full path fuzzy file, buffer, mru, tag, ... finder.
Plug 'kien/ctrlp.vim'

"Vimtex: A modern vim plugin for editing LaTeX files.
Plug 'lervag/vimtex'

"Gruvbox: A color scheme.
Plug 'morhetz/gruvbox'

"Initialize plugin system
call plug#end()

"}}}

"Key mappings {{{
"';' repeats latest f, t, F or T [count] times. ',' is the same just in opposite
"direction. We might not use the latter, so we map ',' to execute ';', since we
"want to use ';' as <Leader>.
nnoremap , ;

"Use a convenient home row key as <Leader> and <LocalLeader> (default:
"backslash).
let mapleader = ';'
let maplocalleader = ';'

"Don't treat wrapped lines as single lines when moving the cursor down (j) and
"up (k).
nnoremap j gj
nnoremap k gk

"See ':help index' to find bindings which are synonyms for other commands. More
"information on ':help map-which-keys'. Stay away from mapping ALT keys (see
"':help :map-alt-keys') if they should work on different terminals without extra
"configuration.

"Toggle line numbers with custom command.
nmap <silent> <C-@> :exe &nu ? 'Nonu' : 'Nu'<CR>

"Plugin mappings
nmap <silent> <C-n> :NERDTreeToggle<CR>
nmap <silent> <F8>  :TagbarToggle<CR>
nmap <leader>1 <Plug>AirlineSelectTab1
nmap <leader>2 <Plug>AirlineSelectTab2
nmap <leader>3 <Plug>AirlineSelectTab3
nmap <leader>4 <Plug>AirlineSelectTab4
nmap <leader>5 <Plug>AirlineSelectTab5
nmap <leader>6 <Plug>AirlineSelectTab6
nmap <leader>7 <Plug>AirlineSelectTab7
nmap <leader>8 <Plug>AirlineSelectTab8
nmap <leader>9 <Plug>AirlineSelectTab9
nmap <leader>- <Plug>AirlineSelectPrevTab
nmap <leader>+ <Plug>AirlineSelectNextTab

"Go to next buffer if no count specified, go to buffer {count} otherwise.
"This is similar to [count]gt (next tab or tab {count}) and gT (previous tab).
nmap <silent> <Leader>j :<C-U>exec v:count ? 'b' . v:count : 'bn'<CR>
"Go to previous buffer.
nmap <silent> <Leader>k :bp<CR>
"Close current buffer.
nmap <silent> <Leader>c :bd<CR>

"TODO easier window mappings.

"}}}

"Syntax {{{

" Set to be able to use fdm=syntax on xml files.
" See: /usr/share/vim/vim80/syntax/xml.vim
let g:xml_syntax_folding = 1

"}}}

"Plugin Customization / Variables

"Syntastic {{{

"TODO use hdevtools + hlint
"TODO hdevtools needs a PKGBUILD

"Always write to errors to location list. If it conflicts with other plugins
"disable this and don't open with :Errors instead of :lop.
let g:syntastic_always_populate_loc_list = 1

"Run syntax checks when buffers are first loaded, as well as on saving.
let g:syntastic_check_on_open = 1

"Skip checks when issuing :wq, :x or :ZZ.
let g:syntastic_check_on_wq = 0

"Jump to first error detected (don't jump when all issues are warnings).
"When using jumps, the location list is overwritten, regardless of the value
"g:syntastic_always_populate_loc_list (Keep in mind if there are location list
"conflicts with other plugins).
let g:syntastic_auto_jump = 3

"Custom status line format. Default: "[Syntax: line:%F (%t)]"
let g:syntastic_stl_format = 'Syn: ln:%F (e:%e|w:%w)'

"Allow shellcheck to 'source' outside of FILES.
let g:syntastic_sh_shellcheck_args = '-x'

"Prefer chktex over lacheck, since we can't ignore warnings in lacheck.
let g:syntastic_tex_checkers = ['chktex', 'lacheck']

"}}}

"Airline {{{

"We don't use fancy powerline fonts as seperators right now, because this vimrc
"should be be compatible with different machines. A font patched for powerline
"might not be available.
"let g:airline_powerline_fonts = 1

"Disable rather modest looking default seperators.
let g:airline_left_sep = ''
let g:airline_right_sep = ''

"Don't show word count, not even in the default filetypes markdown, rst, org,
"help, text.
let g:airline#extensions#wordcount#enabled = 0

"Show airline's tabline.
let g:airline#extensions#tabline#enabled = 1
"Show number of splits _and_ tab number. Format: tab_nr.nr_splits.
let g:airline#extensions#tabline#tab_nr_type = 2
"Dont't show close button on tabline.
let g:airline#extensions#tabline#show_close_button = 0
"Minimum number of buffers needed to show the tabline.
"Either we have a second buffer showing in a window in another tab, a buffer in
"the same tab in another window or just another buffer (not in a new window in
"an extra tab).
let g:airline#extensions#tabline#buffer_min_count = 2
"Display buffer index in tabline and expose tab mappings.
let g:airline#extensions#tabline#buffer_idx_mode = 1
"Fix: maxlinenr symbol not displayed correctly in urxvt with Inconsolata font
"when set to bold (not even with letterspace 0). Maxlinenr only relevant if
"winwidth(0) > 80 (see: airline init.vim).
if !has('gui_running') && winwidth(0) > 80
    " Strip maxlinenr symbol from maxlinenr part
    call airline#parts#define('maxlinenr', {'raw': '/%L', 'accent': 'bold'})
    " Define new part with symbol only (not bold)
    call airline#parts#define_raw('maxlinenrsym', '%{g:airline_symbols.maxlinenr}')
    " Add symbol only part to default z section
    let g:airline_section_z = airline#section#create(['windowswap', 'obsession', '%3p%% ', 'linenr', 'maxlinenr', 'maxlinenrsym', ' :%3v'])
endif

"}}}

"NERDTree {{{

"Change the current working directory for vim when starting NERDTree with a
"directory path and whenever the tree root is changed.
let NERDTreeChDirMode = 2

"Don't replace netrw with NERDTree when :edit <directory> in window.
let NERDTreeHijackNetrw = 0

"Respect the 'wildignore' setting so we don't have to repeat ourselves in
"NERDTreeIngore. NERDTreeIngore would provide toggling with NERDTree-f though.
let NERDTreeRespectWildIgnore = 1

"Close NERDTree on o, i, s, t and T. If we don't want to close it we want to use
"go, gi, gs anyways and we don't heavily rely on tabs. Also re-opening it
"provides the focus automatically.
let NERDTreeQuitOnOpen = 1

"Show bookmarks on startup.
let NERDTreeShowBookmarks = 1

"Display hidden files by default.
let NERDTreeShowHidden = 1

"Change NERDTree window size when loaded.
let NERDTreeWinSize=35

"Disable 'Bookmarks' label and help key info.
let NERDTreeMinimalUI = 1

"}}}

"Tagbar {{{

"Width of the Tagbar window in characters (default: 40).
let g:tagbar_width = 35

"Move cursor to the Tagbar when it is opened.
let g:tagbar_autofocus = 1

"Sort tags according to their order in the source file.
let g:tagbar_sort = 0

"Omit short help at the top and blank lines between top-level scopes.
let g:tagbar_compact = 1

"Automatically show current tag in the preview window when moving the cursor.
let g:tagbar_autopreview = 1

"}}}

"CtrlP {{{

"Set the default opening command.
let g:ctrlp_cmd = 'CtrlPMixed'

"Set searching by filename (as opposed to full path) as the default (Toggle with
"<c-d>).
let g:ctrlp_by_filename = 1

"Increase the maximum height of the match window and keep results = max. height.
let g:ctrlp_match_window = 'max:15'

"Put a new tab page after the last instead of after the current one.
let g:ctrlp_tabpage_position = 'al'

"Enable per-session caching when more than 250 entries.
let g:ctrlp_use_caching = 251

"Scan for hidden files and directories.
let g:ctrlp_show_hidden = 1

"Open newly created files in the current window when pressing <c-y>.
let g:ctrlp_open_new_file = 'r'
"Open all files as hidden buffers when opening multiple files (<c-z> and <c-o>).
let g:ctrlp_open_multiple_files = 'i'
"Prompt for an additional keypress for <c-o> and <c-y> to override the above
"default behavior.
let g:ctrlp_arg_map = 1

"Follow symlinks but ignore looped internal symlinks to avoid duplicates.
let g:ctrlp_follow_symlinks = 1

"Number of MRU files to remember.
let g:ctrlp_mruf_max = 25
"Don't remember files matching the following pattern.
let g:ctrlp_mruf_exclude = '/testdir/.*\|/.vim/.*/doc/.*'
"Only show MRU files in the current working directory.
let g:ctrlp_mruf_relative = 1
"Toggle the above behavior with a key binding.
let g:ctrlp_prompt_mappings = { 'ToggleMRURelative()': ['<F2>'] }
"Don't sort when searching in MRU mode.
let g:ctrlp_mruf_default_order = 1

"Enable extensions to show up in status bar.
let g:ctrlp_extensions = ['mixed', 'bookmarkdir']

"}}}

"Vimtex {{{
"
"Requires xdotool for auto refresh and forward and backward search. Latexmk
"comes with texlive-core on Arch Linux. On Windows, if using TeX Live, make sure
"to check component 'helper programs'.

"Don't use vimtex if necessary programs are not available.
if !executable('latex') || !executable('latexmk')
    let g:vimtex_enabled = 0
endif

"Ensure Vim starts with a server if feature 'clientserver' is available.
if has('clientserver') && empty(v:servername)
    call remote_startserver('VIM')
endif

if has('win32') || has('win32unix')
    "Try to use SumatraPDF for Windows and Cygwin.
    if executable('SumatraPDF')
        let g:vimtex_view_general_viewer = 'SumatraPDF'
        let g:vimtex_view_general_options
                    \ = '-reuse-instance'
                    \ . ' -forward-search @tex @line @pdf'

        "Setup inverse search (not for Cygwin, because clientserver is not
        "available).
        if has('win32unix')
            let g:vimtex_view_general_options .= ' -inverse-search ""'
        elseif executable('gvim')
            let g:vimtex_view_general_options .=
                        \ ' -inverse-search "gvim --servername ' . v:servername . ' --remote-send \"'
                            \ . ':e \%f^<CR^>'
                            \ . ':\%l^<CR^>'
                            \ . ':norm\!zzzv^<CR^>'
                            \ . ':call remote_foreground('''.v:servername.''')^<CR^>'
                        \ . '\""'
        endif
    else
        "So we don't get warnings about unexecutable general viewer.
        let g:vimtex_view_enabled = 0
    endif
else
    "Try to use MuPDF for Linux.
    if executable('mupdf')
        "Auto refresh doesn't work with default 'general'
        "(vimtex_view_general_viewer = xdg-open).
        let g:vimtex_view_method = 'mupdf'
    endif
endif

"Don't fall back to plain TeX if there is not enough information to detect file
"as LaTeX (e.g. for included tex files).
let g:tex_flavor = 'latex'

"Set a separate build directory for latexmk.
let g:vimtex_compiler_latexmk = {
    \ 'build_dir': './build',
\}

"Disable overfull/underfull \hbox and all package warnings.
let g:vimtex_quickfix_ignore_filters = {
    \ 'overfull' : 0,
    \ 'underfull' : 0,
    \ 'packages' : {
    \   'default' : 0,
    \ },
\}

"}}}

"Gruvbox {{{

"Don't show sth in the fold column, since we're only using it as left margin.
"We use autocmd, otherwise it won't work when scheme changed.
autocmd ColorScheme * hi FoldColumn ctermbg=NONE ctermfg=0 guibg=NONE

"Changes dark mode contrast. Possible values: soft, medium, hard.
"If changed remember to set the xresources bg color to the appropriate value.
let g:gruvbox_contrast_dark = 'hard'

"}}}


"Load gruvbox color scheme, if already installed.
try
    colorscheme gruvbox
catch /E185/
    " Don't show "E185: Cannot find color scheme 'gruvbox'" on fresh
    " installations. Vim-Plug and plugin installation is done above.
endtry

" vim: set fdm=marker:
