" Vim indent file
" Language:     Erlang
" Maintainer:   Csaba Hoch <csaba.hoch@gmail.com>
" Contributor:  Edwin Fine <efine145_nospam01 at usa dot net>
" Last Change:  2008 Mar 12

" Only load this indent file when no other was loaded.
if exists("b:did_indent")
  finish
endif
let b:did_indent = 1

setlocal indentexpr=ErlangIndent()
setlocal indentkeys+==after,=end,=catch,=),=],=}

" Only define the functions once.
if exists("*ErlangIndent")
   finish
endif

" The function go through the whole line, analyses it and sets the indentation
" (ind variable).
" l: the number of the line to be examined.
function s:ErlangIndentAfterLine(l)
    let i = 0 " the index of the current character in the line
    let length = strlen(a:l) " the length of the line
    let ind = 0 " how much should be the difference between the indentation of
                " the current line and the indentation of the next line?
                " e.g. +1: the indentation of the next line should be equal to
                " the indentation of the current line plus one shiftwidth
    let lastFun = 0 " the last token was a 'fun'
    let lastReceive = 0 " the last token was a 'receive'; needed for 'after'
    let lastHashMark = 0 " the last token was a 'hashmark'

    while 0<= i && i < length

        " m: the next value of the i
        if a:l[i] == '%'
            break
        elseif a:l[i] == '"'
            let m = matchend(a:l,'"\%([^"\\]\|\\.\)*"',i)
            let lastReceive = 0
        elseif a:l[i] == "'"
            let m = matchend(a:l,"'[^']*'",i)
            let lastReceive = 0
        elseif a:l[i] =~# "[a-z]"
            let m = matchend(a:l,".[[:alnum:]_]*",i)
            if lastFun
                let ind = ind - 1
                let lastFun = 0
                let lastReceive = 0
            elseif a:l[(i):(m-1)] =~# '^\%(case\|if\|try\)$'
                let ind = ind + 1
            elseif a:l[(i):(m-1)] =~# '^receive$'
                let ind = ind + 1
                let lastReceive = 1
            elseif a:l[(i):(m-1)] =~# '^begin$'
                let ind = ind + 2
                let lastReceive = 0
            elseif a:l[(i):(m-1)] =~# '^end$'
                let ind = ind - 2
                let lastReceive = 0
            elseif a:l[(i):(m-1)] =~# '^after$'
                if lastReceive == 0
                    let ind = ind - 1
                else
                    let ind = ind + 0
                endif
                let lastReceive = 0
            elseif a:l[(i):(m-1)] =~# '^fun$'
                let ind = ind + 1
                let lastFun = 1
                let lastReceive = 0
            endif
        elseif a:l[i] =~# "[A-Z_]"
            let m = matchend(a:l,".[[:alnum:]_]*",i)
            let lastReceive = 0
        elseif a:l[i] == '$'
            let m = i+2
            let lastReceive = 0
        elseif a:l[i] == "." && (i+1>=length || a:l[i+1]!~ "[0-9]")
            if matchend(a:l, '[^({<[]*[)}>\]]', i) == -1
                if lastHashMark
                    let lastHashMark = 0
                else
                    let ind = ind - 1
                endif
            endif
            let m = i+1
            let lastReceive = 0
        elseif a:l[i] == '-' && (i+1<length && a:l[i+1]=='>')
            let m = i+2
            let ind = ind + 1
            let lastReceive = 0
        elseif a:l[i] == ';'
            let m = i+1
            let ind = ind - 1
            let lastReceive = 0
        elseif a:l[i] == '#'
            let m = i+1
            let lastHashMark = 1
        else
            let m = i+1
        endif

        let i = m

    endwhile

    return ind

endfunction

function s:FindPrevNonBlankNonComment(lnum)
    let lnum = prevnonblank(a:lnum)
    let line = getline(lnum)
    " continue to search above if the current line begins with a '%'
    while line =~# '^\s*%.*$'
        let lnum = prevnonblank(lnum - 1)
        if 0 == lnum
            return 0
        endif
        let line = getline(lnum)
    endwhile
    return lnum
endfunction

" Galculate the lowest distance between two parent ranges
function s:ErlangInnermostParenSorter(p1, p2)
    let diff = a:p2[0] - a:p1[0]
    return diff == 0 ? a:p2[1] - a:p1[1] : diff
endfunction

" Find the next parens to curline and curcol and get the best math by sorter
function s:ErlangSearchParens(curline, curcol, sorter)
    let parens = [['(', ')'],
                \ ['\[', '\]'],
                \ ['{', '}'],
                \ ['<<', '>>']]

    let skip = "synIDattr(synID(line('.'), col('.'), 1), 'name')"
                \ ." =~? '\\(modifier\\|comment\\|string\\)$'"


    let plist = []

    " search all paren types
    for [lpar, rpar] in parens
        call cursor(a:curline, a:curcol)
        let result = searchpair(lpar, '', rpar, 'bW', skip)
        let plist += [[result, col('.')]]
    endfor

    " get and return best match
    call sort(plist, a:sorter)
    return plist[0]
endfunction

" Find the innermost starting parens/braces/brackets/funnels and calculate the
" indentation level according to their position.
function s:ErlangHandleParens(curline, curcol)
    let [line, column] = s:ErlangSearchParens(a:curline, a:curcol, "s:ErlangInnermostParenSorter")

    if line > 0
        let matchdata = getline(line)
        let linedata = getline(a:curline)

        " first closing par in current line
        let clpar = match(linedata, '^\s*\%([)}\]]\|>>\)') != -1

        if match(matchdata, '\%([({[]\|<<\)\s*$', column - 1) != -1
            " block data
            let ind = indent(line) + (clpar ? 0 : &sw)
        else
            " inline data
            let ind = column - (clpar ? 1 : 0)
        endif

        return ind >= 0 ? ind : -1
    endif

    return -1
endfunction

function ErlangIndent()

    " Find a non-blank line above the current line.
    let lnum = prevnonblank(v:lnum - 1)

    " Hit the start of the file, use zero indent.
    if lnum == 0
        return 0
    endif

    let prevline = getline(lnum)
    let currline = getline(v:lnum)

    let newind = s:ErlangHandleParens(v:lnum, 1)
    if newind >= 0
        return newind
    endif

    let soup = prevline

    let ind = indent(lnum)

    if prevline =~# '\([)}\]]\|>>\)'
        " Last line contains an end paren, so indent at the same level as
        " the statement introducing the paren.
        let pllastparen = match(prevline, '\([)}\]]\|>>\)[^)}>\]]*$')
        let [pl, pc] = s:ErlangSearchParens(lnum, pllastparen, "s:ErlangInnermostParenSorter")
        if pl > 0 && pl != lnum
            let ind = indent(pl)
            " Let's put all that soup together so we can later find
            " the right keywords and indent according to them.
            let soup = join(getline(pl, lnum), ' ')
        endif
    endif

    let ind = ind + &sw * s:ErlangIndentAfterLine(soup)

    " special cases:
    if prevline =~# '^\s*\%(after\|end\)\>'
        let ind = ind + 2*&sw
    endif
    if currline =~# '^\s*end\>'
        let ind = ind - 2*&sw
    endif
    if currline =~# '^\s*after\>'
        let plnum = s:FindPrevNonBlankNonComment(v:lnum-1)
        if getline(plnum) =~# '^[^%]*\<receive\>\s*\%(%.*\)\=$'
            let ind = ind - 1*&sw
            " If the 'receive' is not in the same line as the 'after'
        else
            let ind = ind - 2*&sw
        endif
    endif
    if prevline =~# '^\s*\%(catch\)\s*\%(%\|$\)'
        let ind = ind + 1*&sw
    endif
    if currline =~# '^\s*\%(catch\)\s*\%(%\|$\)'
        let ind = ind - 1*&sw
    endif

    if ind<0
        let ind = 0
    endif
    return ind

endfunction

" TODO:
" 
" f() ->
"     x("foo
"         bar")
"         ,
"         bad_indent.
"
" fun
"     init/0,
"     bad_indent
"
"     #rec
"     .field,
" bad_indent
"
" case X of
"     1 when A; B ->
"     bad_indent

