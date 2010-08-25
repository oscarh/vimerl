" Vim ftplugin file
" Language:   Erlang
" Maintainer: Oscar Hellström <oscar@oscarh.net>
" URL:        http://personal.oscarh.net
" Version:    2010-08-09
" ------------------------------------------------------------------------------
" Usage: {{{1
"
" To enable folding put in your vimrc
" let g:erlangFold=1
"
" Folding will make only one fold for a complete function, even though it has
" more than one function head and body
" To change this behaviour put
" let g:erlangFoldSplitFunction=1
" in your vimrc file
"
" }}}
" ------------------------------------------------------------------------------
" Plugin init {{{1
if exists("b:did_ftplugin")
	finish
endif

" Don't load any other
let b:did_ftplugin=1

if exists('s:doneFunctionDefinitions')
	call s:SetErlangOptions()
	finish
endif

let s:doneFunctionDefinitions=1
" }}}

" Local settings {{{1
" Run Erlang make instead of GNU Make
function s:SetErlangOptions()
	if version >= 700
		setlocal omnifunc=erlangcomplete#Complete
	endif

	" {{{2 Settings for folding
	if (exists("g:erlangFold")) && g:erlangFold
		setlocal foldmethod=expr
		setlocal foldexpr=GetErlangFold(v:lnum)
		setlocal foldtext=ErlangFoldText()
		"setlocal fml=2
	endif
endfunction


" Define folding functions {{{1
if !exists("*GetErlangFold")
	" Folding params {{{2
	" FIXME: Could these be shared between scripts?
	let s:ErlangFunEnd      = '^[^%]*\.\s*\(%.*\)\?$'
	let s:ErlangFunHead     = '^\a\w*(.*)\(\s+when\s+.*\)\?\s\+->\s*$'
	let s:ErlangBeginHead   = '^\a\w*(.*$'
	let s:ErlangEndHead     = '^\s\+[a-zA-Z-_{}\[\], ]\+)\(\s+when\s+.*\)\?\s\+->\s\(%.*\)\?*$'
	let s:ErlangBlankLine   = '^\s*\(%.*\)\?$'

	" Auxiliary fold functions {{{2 
	function s:GetNextNonBlank(lnum)
		let lnum = nextnonblank(a:lnum + 1)
		let line = getline(lnum)
		while line =~ s:ErlangBlankLine && 0 != lnum
			let lnum = nextnonblank(lnum + 1)
			let line = getline(lnum)
	   endwhile
	   return lnum
	endfunction

	function s:GetFunName(str)
		return matchstr(a:str, '^\a\w*(\@=')
	endfunction

	function s:GetFunArgs(str, lnum)
		let str = a:str
		let lnum = a:lnum
		while str !~ '->\s*\(%.*\)\?$'
			let lnum = s:GetNextNonBlank(lnum)
			if 0 == lnum " EOF
				return ""
			endif
			let str .= getline(lnum)
		endwhile
		return matchstr(str, 
			\ '\(^(\s*\)\@<=.*\(\s*)\(\s\+when\s\+.*\)\?\s\+->\s*\(%.*\)\?$\)\@=')
	endfunction

	function s:CountFunArgs(arguments)
		let pos = 0
		let ac = 0 " arg count
		let arguments = a:arguments
		
		" Change list / tuples into just one A(rgument)
		let erlangTuple = '{\([A-Za-z_,|=\-\[\]]\|\s\)*}'
		let erlangList  = '\[\([A-Za-z_,|=\-{}]\|\s\)*\]'

		" FIXME: Use searchpair?
		while arguments =~ erlangTuple
			let arguments = substitute(arguments, erlangTuple, "A", "g")
		endwhile
		" FIXME: Use searchpair?
		while arguments =~ erlangList
			let arguments = substitute(arguments, erlangList, "A", "g")
		endwhile
		
		let len = strlen(arguments)
		while pos < len && pos > -1
			let ac += 1
			let pos = matchend(arguments, ',\s*', pos)
		endwhile
		return ac
	endfunction

	" Main fold function {{{2
	function GetErlangFold(lnum)
		let lnum = a:lnum
		let line = getline(lnum)

		" Function head gives fold level 1 {{{3
		if line=~ s:ErlangBeginHead
			while line !~ s:ErlangEndHead
				if 0 == lnum " EOF / BOF
					return '='
				endif
				if line =~ s:ErlangFunEnd
					return '='
				endif
				endif
				let lnum = s:GetNextNonBlank(lnum)
				let line = getline(lnum)
			endwhile 
			" check if prev line was really end of function
			let lnum = s:GetPrevNonBlank(a:lnum)
			if exists("g:erlangFoldSplitFunction") && g:erlangFoldSplitFunction
				if getline(lnum) !~ s:ErlangFunEnd
					return '='
				endif
			endif
			return '1>'
		endif

		" End of function (only on . not ;) gives fold level 0 {{{3
		if line =~ s:ErlangFunEnd
			return '<1'
		endif

		" Check if line below is a new function head {{{3
		" Only used if we want to split folds for different function heads
		" Ignores blank lines
		if exists("g:erlangFoldSplitFunction") && g:erlangFoldSplitFunction
			let lnum = s:GetNextNonBlank(lnum)

			if 0 == lnum " EOF
				return '<1'
			endif

			let line = getline(lnum)

			" End of prev function head (new function here), ending fold level 1
			if line =~ s:ErlangFunHead || line =~ s:ErlangBeginHead
				return '<1'
			endif
		endif
		
		" Otherwise use fold from previous line
		return '='
	endfunction

	" Erlang fold description (foldtext function) {{{2
	function ErlangFoldText()
		let foldlen = v:foldend - v:foldstart
		if 1 < foldlen
			let lines = "lines"
		else
			let lines = "line"
		endif
		let line = getline(v:foldstart)
		let name = s:GetFunName(line)
		let arguments = s:GetFunArgs(strpart(line, strlen(name)), v:foldstart)
		let argcount = s:CountFunArgs(arguments)
		let retval = v:folddashes . " " . name . "/" . argcount
		let retval .= " (" . foldlen . " " . lines . ")"
		return retval
	endfunction " }}}
endif " }}}

call s:SetErlangOptions()

" Skeletons {{{1
function GenServer()
	echo foo 
endfunction
" }}}

let g:erlangServerName = "vimerl"
let g:pathToWranglerBin = "/tmp/wrangler-0.8.9/pkg/share/wrangler/ebin/"

function! StartServer()
    let command = "erl -detached -pa " . g:pathToWranglerBin . " -sname " . g:erlangServerName
    call system(command)
    call SendStartSignal()
endfunction

function! SendStartSignal()
    echo s:send_rpc('application', 'start', '[wrangler_app]')
endfunction

function! SendStopSignal()
    echo s:send_rpc('erlang', 'halt', '')
endfunction

function! s:send_rpc(module, fun, args)
    let command = "erl_call -sname " . g:erlangServerName . " -a '" . a:module . " " . a:fun . " " . a:args . "'"
    return system(command)
endfunction

function! s:check_for_error(result)
    let error_start =  match(a:result, '{error,"')
    if error_start != -1
        echo matchstr(a:result, '[^"]*', error_start + strlen('{error,"'))
        return 1
    endif
    return 0
endfunction

function! s:send_confirm()
    let module = 'wrangler_preview_server'
    let fun = 'commit'
    let args = '[]'
    let result = s:send_rpc(module, fun, args)
endfunction

function! SendConfirm()
    call s:send_confirm()
endfunction

function! s:call_extract(start_line, start_col, end_line, end_col, name)
    let file = expand("%:p")
    let module = 'refac_new_fun'
    let fun = 'fun_extraction'
    let args = '["' . file . '", {' . a:start_line . ', ' . a:start_col . '}, {' . a:end_line . ', ' . a:end_col . '}, "' . a:name . '", ' . &sw . ']'
    let result = s:send_rpc(module, fun, args)
    if s:check_for_error(result)
        return 0
    endif
    call s:send_confirm()
    return 1
endfunction

function! ErlangExtractFunction(mode) range
    silent w!
    let name = inputdialog("New function name: ")
    if name != ""
        if a:mode == "v"
            let start_pos = getpos("'<")
            let start_line = start_pos[1]
            let start_col = start_pos[2]

            let end_pos = getpos("'>")
            let end_line = end_pos[1]
            let end_col = end_pos[2]
            if s:call_extract(start_line, start_col, end_line, end_col, name)
                :e
            endif
        elseif a:mode == "n"
            let pos = getpos(".")
            let line = pos[1]
            let col = pos[2]
            if s:call_extract(line, col, line, col, name)
                :e
            endif
        else
            echo "Mode not supported."
        endif
    else
        echo "Empty function name. Ignoring."
    endif
endfunction
nmap <A-r>e :call ErlangExtractFunction("n")<ENTER>
vmap <A-r>e :call ErlangExtractFunction("v")<ENTER>

function! s:call_rename(mode, line, col, name, search_path)
    let file = expand("%:p")
    let module = 'refac_rename_' . a:mode
    let fun = 'rename_' . a:mode
    let args = '["' . file .'", '
    if a:mode != "mod"
         let args = args . a:line . ', ' . a:col . ', '
    endif
    let args = args . '"' . a:name . '", ["' . a:search_path . '"], ' . &sw . ']'
    let result = s:send_rpc(module, fun, args)
    echo result
    if s:check_for_error(result)
        return 0
    endif
    call s:send_confirm()
    return 1
endfunction

function! ErlangRename(mode)
    silent w!
    if a:mode == "mod"
        let name = inputdialog('Rename module to: ')
    else
        let name = inputdialog('Rename "' . expand("<cword>") . '" to: ')
    endif
    if name != ""
        let search_path = inputdialog('Search path: ', expand("%:p:h"))
        if search_path != ""
            let pos = getpos(".")
            let line = pos[1]
            let col = pos[2]
            let current_filename = expand("%")
            let current_filepath = expand("%:p")
            let new_filename = name . '.erl'
            if s:call_rename(a:mode, line, col, name, search_path)
                if a:mode == "mod"
                    execute ':bd ' . current_filename
                    execute ':e ' . new_filename
                    silent execute '!mv ' . current_filepath . ' ' . current_filepath . '.bak'
                    redraw!
                else
                    :e
                endif
            endif
        else
            echo "You must specify search dir"
        endif
    else
        echo "Empty name. Ignoring."
    endif
endfunction

function! ErlangRenameFunction()
    call ErlangRename("fun")
endfunction
map <A-r>f :call ErlangRenameFunction()<ENTER>

function! ErlangRenameVariable()
    call ErlangRename("var")
endfunction
map <A-r>v :call ErlangRenameVariable()<ENTER>

function! ErlangRenameModule()
    call ErlangRename("mod")
endfunction
map <A-r>m :call ErlangRenameModule()<ENTER>

function! ErlangRenameProcess()
    call ErlangRename("process")
endfunction
map <A-r>p :call ErlangRenameProcess()<ENTER>

function! s:call_tuple_fun_args(start_line, start_col, end_line, end_col, search_path)
    let file = expand("%:p")
    let module = 'refac_tuple'
    let fun = 'tuple_funpar'
    let args = '["' . file . '", {' . a:start_line . ', ' . a:start_col . '}, {' . a:end_line . ', ' . a:end_col . '}, ["' . a:search_path . '"], ' . &sw . ']'
    let result = s:send_rpc(module, fun, args)
    if s:check_for_error(result)
        return 0
    endif
    call s:send_confirm()
    return 1
endfunction

function! ErlangTupleFunArgs(mode)
    silent w!
    let search_path = inputdialog('Search path: ', expand("%:p:h"))
    if a:mode == "v"
        let start_pos = getpos("'<")
        let start_line = start_pos[1]
        let start_col = start_pos[2]

        let end_pos = getpos("'>")
        let end_line = end_pos[1]
        let end_col = end_pos[2]
        if s:call_tuple_fun_args(start_line, start_col, end_line, end_col, search_path)
            :e
        endif
    elseif a:mode == "n"
        let pos = getpos(".")
        let line = pos[1]
        let col = pos[2]
        if s:call_tuple_fun_args(line, col, line, col, search_path)
            :e
        endif
    else
        echo "Mode not supported."
    endif
endfunction
nmap <A-r>t :call ErlangTupleFunArgs("n")<ENTER>
vmap <A-r>t :call ErlangTupleFunArgs("v")<ENTER>

" vim: set foldmethod=marker:
