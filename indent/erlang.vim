" ------------------------------------------------------------------------------
" Vim indent file
" Language:    Erlang
" Maintainer:  Oscar Hellström <oscar@oscarh.net>
" URL:         http://personal.oscarh.net
" Version:     2006-05-24
" ------------------------------------------------------------------------------

" Only load this indent file when no other was loaded {{{1
"if exists("b:did_indent")
"  finish
"endif
"let b:did_indent = 1

" Set up our indention expression and the triggers {{{1
setlocal indentexpr=GetErlangIndent()
setlocal indentkeys=o,O,e
setlocal indentkeys+==end,=after

if exists("*GetErlangIndent")
	finish
endif

" 1) Patterns to be used for determining the indentation {{{1
"
" Commends are ignored at end of lines
let s:erlangIgnorePattern       = '\(%.*\)\?'

" The following patterns will always get the ignore pattern and
" the EOL char appended
" Pattern that add to the indentation
let s:erlangAddIndentPattern    = '\(if\|of\|receive\|after\|catch\|->\)'
" Pattern that resets indentation
let s:erlangZeroIndentPattern   = '\.'
" Pattern that will keep the current indentation
let s:erlangKeepIndentPattern   = '\(,\|.\+\s\+->\s\+.\+;\|\<try\>\s\+\w.*\)'
" Pattern that deletes from the current indentation
let s:erlangRemoveIndentPattern = ';'
" end of pattern which get EOL appended


" Pattern that should align indentation with block opening line
let s:erlangAlingIdentPattern   = '\(\<after\>\|\<catch\>\|\<after\s\+?\?\w\s\+->\)'

" Pattern that opens blocks
let s:erlangBlockbegPattern     = '.*\%(fun(.*)\s\+\%(when\s\+.\+\s\+\)\?->.*'
let s:erlangBlockbegPattern    .= '\|receive\|if\|query'
let s:erlangBlockbegPattern    .= '\|case\s.\+\sof'
let s:erlangBlockbegPattern    .= '\|try\s.\+\sof'
let s:erlangBlockbegPattern    .= '\|catch\)'

" Pattern that closes block
let s:erlangBlockendPattern     = 'end'

" Empty line
let s:erlangEmptyLine           = '^\s*$'

" 2)  Auxiliary functions: {{{1

" Matches the whole string
function s:Match(line, exp)
	return a:line =~ a:exp
endfunction

" Matches the whole string, but only if it is the only thing on that line
function s:MatchAll(line, exp)
	return s:MatchEnd(a:line, '^\s*' . a:exp)
endfunction

" Matches the end of a string
" Any pattern that should be ignored before a line end is ignored
function s:MatchEnd(line, exp)
	return a:line =~ a:exp . '\s*' . s:erlangIgnorePattern . '$'
endfunction

" Find the previous nonblank line
" ignoring lines only containing comments
function s:FindPrevNonBlankNonComment(lnum)
	let lnum = prevnonblank(a:lnum)
	let line = getline(lnum)
	" continue to search above if the current line begins with a '%'
	while line =~ '^\s*%.*$'
		let lnum = prevnonblank(lnum - 1)
		if 0 == lnum
			return 0
		endif
		let line = getline(lnum)
	endwhile
	return lnum
endfunction

" 3) Define the indent function {{{1
function GetErlangIndent()

	" 1) Check the current line {{{2

	let lnum = v:lnum
	let line = getline(lnum)

	" At the beginning of the file, zero indentation
	if 0 == lnum
		return 0
	endif

	" If the current line is the end of a block,
	" find the indentation of the block opener
	if s:MatchEnd(line, s:erlangBlockendPattern) 
		" FIXME write func find_block_open
		let begline = searchpair(s:erlangBlockbegPattern, '',
			\ s:erlangBlockendPattern, 'bW',
			\ 'synIDattr(synID(line("."), col("."), 0), "name") =~? "string"')
		if begline != lnum
			return indent(begline)
		endif
	endif

	if s:MatchAll(line, s:erlangAlingIdentPattern)
		" FIXME write func find_block_open
	endif

	" 2) check the previous line(s) {{{2

	" Find a non-blank, non-multi-line string line above the current line.
	let lnum = s:FindPrevNonBlankNonComment(v:lnum - 1)

	" Read line 
	let line = getline(lnum)

	" Beginning of file
	if 0 == lnum
		return 0
	endif

	" Check for pattern that will add to indentation
	if s:MatchEnd(line, s:erlangAddIndentPattern)
		return indent(lnum) + &sw
	endif

	" Check for patterns that will keep indent
	if s:MatchEnd(line, s:erlangKeepIndentPattern)
		return indent(lnum)
	endif

	" Check for patterns that will decrease indent
	if s:MatchEnd(line, s:erlangRemoveIndentPattern)
		return indent(lnum) - &sw
	endif

	" Check for patterns that will reset indentation
	if s:MatchEnd(line, s:erlangZeroIndentPattern)
		return 0
	endif

	" 3) can't find a pattern, get indent from line before
	return indent(lnum) + &sw
endfunction
" vim: set foldmethod=marker:
