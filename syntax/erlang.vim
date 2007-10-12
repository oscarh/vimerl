" Vim syntax file
" Language:   Erlang
" Maintainer: Oscar Hellström <oscar@oscarh.net>
" URL:        http://oscar.hellstrom.st
" Version:    2007-10-12
" ------------------------------------------------------------------------------
" {{{1
" Options:
" }}}
" -----------------------------------------------------------------------------


" For version 5.x: Clear all syntax items
" For version 6.x: Quit when a syntax file was already loaded
if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif

setlocal iskeyword+=-

" Erlang is case sensitive
syn case match

syn match erlangOperator         /=\|==\|\/=\|=:=\|=\/=\|<\|>\|=<\|>=\|/ contained
syn match erlangOperator         /++\|--/ contained
syn match erlangOperator         /|/ contained
syn match erlangOperator         /<-\|->/ contained

syn keyword erlangOperator       band bor bnot bsl bsr bxor div rem xor contained

syn match erlangInteger          /[0-9]\+/ contained

syn match erlangDelimiter        /,\||\|;\|/ contained

syn keyword erlangTodo TODO FIXME XXX contained
syn match erlangComment          /%.*$/ contains=erlangTodo,@Spell

syn match erlangMacro            /?\?\h/ contained

syn region erlangAttribute       start=/^-\(module\|compile\|vsn\|copyright\)(/ end=/)\.\@=/ contains=erlangList,erlangAtom,erlangString
syn region erlangAttribute       start=/^-export(/ end=/)\.\@=/ contains=erlangList

syn region erlangInclude         start=/-include\(_lib\)\?(/ end=/)\.\@=/ contains=erlangNoSpellString
syn region erlangDefine          start=/-define(/ end=/)\.\@=/ contains=erlangDelimiter,erlangMacro,erlangAtom,erlangList, erlangTuple,erlangRecord
syn region erlangPreCondit       start=/-\(ifdif\|ifndef\|else\|endif\)(/ end=/)\.\@=/ contains=erlangMacro
syn region erlangPreProc        start=/-record(/ end=/)\.\@=/ contains=erlangTuple


syn region erlangString          start="\\\@<!\"" end="\\\@<!\"" contains=@Spell
syn region erlangNoSpellString          start="\\\@<!\"" end="\\\@<!\""

syn match erlangAtom             /\<-\@<!\l\w*\>/ contained
syn match erlangAtom             /'.*'/ contained

syn match erlangVariable          /\<?\@<![A-Z_]\w*\>/ contained

syn region erlangList            start=/\[/ end=/\]/ contained contains=erlangAtom,erlangVariable,erlangDelimiter,erlangString
syn region erlangTuple           start=/{/ end=/}/ contained contains=erlangAtom,erlangVariable,erlangDelimiter
syn match erlangRecord           /#\l\w\+/ 

syn region erlangFunction        start=/\(^\a\+(.*).*->\)\@<=/ end=/[0-9]\@<!\.[0-9]\@!/ contains=erlangComment,erlangAtom,erlangVariable,erlangMacro,erlangDelimiter,erlangRecord,erlangOperator,erlangInteger,erlangList

" Link Erlang stuff to Vim groups {{{1
hi link erlangTodo          Todo
hi link erlangString        String
hi link erlangNoSpellString String 
hi link erlangModifier      SpecialChar
hi link erlangComment       Comment
hi link erlangVariable      Identifier
hi link erlangInclude       Include
hi link erlangAttribute     Keyword
hi link erlangGBIF          Keyword
hi link erlangKeyword       Keyword
hi link erlangMacro         Macro
hi link erlangDefine        Define
hi link erlangPreCondit     PreCondit
hi link erlangPreProc       PreProc
"hi link erlangDelimiter     Delimiter
hi link erlangDelimiter     Normal
hi link erlangOperator      Operator
hi link erlangContitional   Conditional
hi link erlangGuard         Conditional
hi link erlangAtom          Normal
hi link erlangRecord        Structure
hi link erlangInteger       Number
hi link erlangFloat         Number
hi link erlangFloat         Number
hi link erlangFloat         Number
hi link erlangFloat         Number
hi link erlangHex           Number
hi link erlangBIF           Function
hi link erlangFun           Keyword
hi link erlangList          Structure
hi link erlangTuple         Structure

" }}}

" hi def link erlangFunctionHead  ErlangFunHead

let b:current_syntax = "erlang"

" vim: set foldmethod=marker:
