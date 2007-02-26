" ------------------------------------------------------------------------------
" Vim omni-completion script
" Author: Oscar Hellström
" Email: oscar@oscarh.net
" Version: 2006-06-23
" ------------------------------------------------------------------------------

" Patterns for completions {{{1
let s:erlangLocalFuncBeg    = '\(\<[0-9A-Za-z_-]*\|\s*\)$'
let s:erlangExternalFuncBeg = '\<[0-9A-Za-z_-]\+:[0-9A-Za-z_-]*$'
let s:ErlangBlankLine       = '^\s*\(%.*\)\?$'

" Main function for completion {{{1
function! erlangcomplete#Complete(findstart, base)
	" 0) Init {{{2
	let lnum = line('.')
	let column = col('.') 
	let line = strpart(getline('.'), 0, column - 1)

	" 1) First, check if completion is impossible {{{2
	if line =~ '[^~\\]%'
		return -1
	endif

	"echo "line[col - 1]:" . line[column - 1] . " line[col - 2]:" . line[column - 2] .  "\n" . line . "\n"

	" 2) Check if the char to the left of us are part of a function call {{{2
	"
	" Nothing interesting is written at the char just before the cursor
	" This means _anything_ could be started here
	" In this case, keyword completion should probably be used,
	" for now we'll only try and complete local functions.
	" TODO: Examine if we can stare Identifiers end complete on them
	" Is this worth it? Is /completion/ of a "blank" wanted? Can we consider (
	" interesting and check if we are in a function call etc.?
	if line[column - 2] !~ '[0-9A-Za-z:_-]'
		if a:findstart
			return column
		else
			return s:erlangFindLocalFunc(a:base)
		endif
	endif
	

	" 3) Function in external module {{{2
	if line =~ s:erlangExternalFuncBeg
		let delimiter = match(line, ':[0-9A-Za-z_-]*$') + 1
		if a:findstart
			return delimiter
		else
			let module = matchstr(line, '\(\<\)\@<=[0-9A-Za-z_-]\+:\@=')
			return s:erlangFindExternalFunc(module, a:base)
		endif
	endif

	" 4) Local function {{{2
	if line =~ s:erlangLocalFuncBeg
		let funcstart = match(line, ':\@<![0-9A-Za-z_-]*$')
		if a:findstart
			return funcstart
		else
			return s:erlangFindLocalFunc(a:base)
		endif
	endif

	" 5) Unhandled situation {{{2
	if a:findstart
		return -1
	else
		return []
	endif
endfunction

" Auxiliary functions for completion {{{1 
" Find the next non-blank line {{{2
function s:erlangFindNextNonBlank(lnum)
	let lnum = nextnonblank(a:lnum + 1)
	let line = getline(lnum)
	while line =~ s:ErlangBlankLine && 0 != lnum
		let lnum = nextnonblank(lnum + 1)
		let line = getline(lnum)
   endwhile
   return lnum
endfunction
			
" vim: foldmethod=marker:
" Find external function names {{{2
function s:erlangFindExternalFunc(module, base)
	let functions = []
	return functions
endfunction

" Find local function names {{{2
function s:erlangFindLocalFunc(base)
	" begin at line 1
	let lnum = s:erlangFindNextNonBlank(1)
	if "" == a:base
		let base = '\w' " used to match against word symbol
	else
		let base = a:base
	endif
	while 0 != lnum && !complete_check()
		let line = getline(lnum)
		let function_name = matchstr(line, '^' . base . '[0-9A-Za-z_-]\+(\@=')
		if function_name != ""
			call complete_add(function_name)
		endif
		let lnum = s:erlangFindNextNonBlank(lnum)
	endwhile
	return []
endfunction

