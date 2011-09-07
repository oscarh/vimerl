" Vim omni completion file
" Language:     Erlang
" Author:       Oscar Hellström <oscar@oscarh.net>
" Contributors: kTT (http://github.com/kTT)
"               Ricardo Catalinas Jiménez <jimenezrick@gmail.com>
"               Eduardo Lopez (http://github.com/tapichu)
" Version:      2011/08/06

" Completion options
if !exists('g:erlangCompletionGrep')
	let g:erlangCompletionGrep = 'grep'
endif

if !exists('g:erlangManSuffix')
	let g:erlangManSuffix = ''
endif

if !exists('g:erlangManPath')
	let g:erlangManPath = '/usr/lib/erlang/man'
endif

if !exists('g:erlangCompletionDisplayDoc')
	let g:erlangCompletionDisplayDoc = 1
endif

" Completion program path
let s:erlangCompleteFile = expand('<sfile>:p:h') . '/erlang_complete.erl'

" Patterns for completions
let s:erlangLocalFuncBeg    = '\(\<[0-9A-Za-z_-]*\|\s*\)$'
let s:erlangExternalFuncBeg = '\<[0-9A-Za-z_-]\+:[0-9A-Za-z_-]*$'
let s:ErlangBlankLine       = '^\s*\(%.*\)\?$'

" Main function for completion
function! erlangcomplete#Complete(findstart, base)
	let lnum = line('.')
	let column = col('.')
	let line = strpart(getline('.'), 0, column - 1)

	" 1) Check if the char to the left of us are part of a function call
	"
	" Nothing interesting is written at the char just before the cursor
	" This means _anything_ could be started here
	" In this case, keyword completion should probably be used,
	" for now we'll only try and complete local functions.
	"
	" TODO: Examine if we can stare Identifiers end complete on them
	" Is this worth it? Is /completion/ of a "blank" wanted? Can we consider
	" `(' interesting and check if we are in a function call etc.?
	if line[column - 2] !~ '[0-9A-Za-z:_-]'
		if a:findstart
			return column
		else
			return s:erlangFindLocalFunc(a:base)
		endif
	endif
	
	" 2) Function in external module
	if line =~ s:erlangExternalFuncBeg
		let delimiter = match(line, ':[0-9A-Za-z_-]*$') + 1
		if a:findstart
			return delimiter
		else
			let module = matchstr(line[:-2], '\<\k*\>$')
			return s:erlangFindExternalFunc(module, a:base)
		endif
	endif

	" 3) Local function
	if line =~ s:erlangLocalFuncBeg
		let funcstart = match(line, ':\@<![0-9A-Za-z_-]*$')
		if a:findstart
			return funcstart
		else
			return s:erlangFindLocalFunc(a:base)
		endif
	endif

	" 4) Unhandled situation
	if a:findstart
		return -1
	else
		return []
	endif
endfunction

" Find the next non-blank line
function s:erlangFindNextNonBlank(lnum)
	let lnum = nextnonblank(a:lnum + 1)
	let line = getline(lnum)

	while line =~ s:ErlangBlankLine && 0 != lnum
		let lnum = nextnonblank(lnum + 1)
		let line = getline(lnum)
	endwhile

	return lnum
endfunction

" Find external function names
function s:erlangFindExternalFunc(module, base)
	" If it is a local module, try to compile it
	if filereadable(a:module . '.erl') && !filereadable(a:module . '.beam')
		silent execute '!erlc' a:module . '.erl' '>/dev/null' '2>/dev/null'
		redraw!
	endif

	let functions = system(s:erlangCompleteFile . ' ' . a:module)
	for element in sort(split(functions, '\n'))
		if match(element, a:base) == 0
			let function_name = matchstr(element, a:base . '\w*')
			let number_of_args = matchstr(element, '\d\+', len(function_name))
			let number_of_comma = max([number_of_args - 1, 0])
			let file_path = g:erlangManPath . '/man?/' . a:module . '\.?' . g:erlangManSuffix
			let description = ''

			" Don't look man pages if the module is present in the current directory
			if g:erlangCompletionDisplayDoc != 0 && !filereadable(a:module . '.erl')
				let system_command = g:erlangCompletionGrep . ' -A 1 "\.B" ' . file_path .
							\' | grep -EZo "\<' . function_name . '\>\((\[?\w+,\]? ){' .
							\number_of_comma . '}[^),]*\) -> .*"'
				let description = system(system_command)

				" Cutting some weird characters at the end with `[:-2]'
				" because grep doesn't support multilines, so we have to
				" filter first by `.B' and next by looking via function
				" name, if someone have a better idea, please change it
				let description = description[:-2]
			endif

			if description == ''
				" If function doesn't have a description e.g.
				" lists:rmerge, put rmerge/2 instead
				let description = element
			endif

			let field = {'word': function_name . '(', 'abbr': description,
						\'kind': 'f', 'dup': 1} " Allow to duplicate functions
			call complete_add(field)
		endif
	endfor

	return []
endfunction

" Find local function names
function s:erlangFindLocalFunc(base)
	" Begin at line 1
	let lnum = s:erlangFindNextNonBlank(1)

	if "" == a:base
		let base = '\w' " Used to match against word symbol
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
