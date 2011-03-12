" Vim ftplugin file
" Language:     Erlang
" Author:       Oscar Hellström <oscar@oscarh.net>
" Contributors: Ricardo Catalinas Jiménez <jimenezrick@gmail.com>
"               Eduardo Lopez (http://github.com/tapichu)
" Version:      2011/03/10

" Plugin init
if exists("b:did_ftplugin")
	finish
endif

" Don't load any other
let b:did_ftplugin=1

if !exists('g:erlangKCommand')
	let g:erlangKCommand='erl -man'
endif

if exists('s:doneFunctionDefinitions')
	call s:SetErlangOptions()
	finish
endif

let s:doneFunctionDefinitions=1

" Local settings
function s:SetErlangOptions()
	compiler erlang
	if version >= 700
		setlocal omnifunc=erlangcomplete#Complete
	endif

	setlocal foldmethod=expr
	setlocal foldexpr=GetErlangFold(v:lnum)
	setlocal foldtext=ErlangFoldText()

	setlocal comments=:%%%,:%%,:%
	setlocal commentstring=%%s
	setlocal formatoptions+=ro

	if g:erlangKCommand != ''
		let &l:keywordprg=g:erlangKCommand
	endif
endfunction

" Define folding functions
if !exists("*GetErlangFold")
	" Folding params
	let s:ErlangFunBegin    = '^\a\w*(.*$'
	let s:ErlangFunEnd      = '^[^%]*\.\s*\(%.*\)\?$'
	let s:ErlangBlankLine   = '^\s*\(%.*\)\?$'

	" Auxiliary fold functions
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

	" Main fold function
	function GetErlangFold(lnum)
		let lnum = a:lnum
		let line = getline(lnum)

		if line =~ s:ErlangFunEnd
			return '<1'
		endif

		if line =~ s:ErlangFunBegin && foldlevel(lnum - 1) == 1
			if exists("g:erlangFoldSplitFunction") && g:erlangFoldSplitFunction
				return '>1'
			else
				return '1'
			endif
		endif

		if line =~ s:ErlangFunBegin
			return '>1'
		endif

		return '='
	endfunction

	" Erlang fold description (foldtext function)
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
		let retval = "+" . v:folddashes . " " . name . "/" . argcount
		let retval .= " (" . foldlen . " " . lines . ")"
		return retval
	endfunction
endif

call s:SetErlangOptions()
