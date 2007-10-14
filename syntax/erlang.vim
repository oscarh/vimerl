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

" Erlang is case sensitive
syn case match

syn match erlangDelimiter                    /\|,\|\.\|;/
syn match erlangArityDelimiter               /\/\|,/ contained

syn match erlangStringModifier               /\\./ contained
syn match erlangModifier                     /\$\\\?./

syn match erlangInteger                      /[0-9]\+#[0-9a-f]\+\|[0-9]\+/
syn match erlangFloat                        /[0-9]\+\.[0-9]\+\%(e-\?[0-9]\+\)\?/

syn keyword erlangTodo                       TODO FIXME XXX contained
syn match erlangComment                      /%.*$/ contains=@Spell,erlangTodo

syn keyword erlangKeyword                    band bor bnot bsl bsr bxor div rem xor
syn keyword erlangKeyword                    try catch begin receive after cond fun let query

syn keyword erlangContitional                case if of end
syn keyword erlangContitional                not and or andalso orelse

syn keyword erlangBoolean                    true false

syn keyword erlangGuard                      is_list is_atom is_binary is_tuple is_number is_integer is_float is_function is_constant is_pid is_port is_reference is_record

syn match erlangOperator                     /\/\|*\|+\|-\|++\|--/
syn match erlangOperator                     /->\|<-\|||\||\|!\|=/
syn match erlangOperator                     /=:=\|==\|\/=\|=\/=\|<\|>\|=<\|>=/
syn keyword erlangOperator                   div rem

syn region erlangString                      start=/\\\@<!"/ end=/\\\@<!"/ contains=@Spell, erlangStringModifier

syn match erlangVariable                     /\<[A-Z_]\w*\>/
syn match erlangAtom                         /\%(\%(^-\)\|#\)\@<!\<[a-z]\w*\>(\@!/
syn match erlangAtom                         /\\\@<!'.*\\\@<!'/

syn match erlangRecord                       /#\w\+/
syn match erlangTuple                        /{\|}/
syn match erlangList                        /\[\|\]/
syn region erlangArityList                   start="\[" end="\]" contains=erlangAtom,erlangInteger,erlangArityDelimiter contained

syn keyword erlangKeyword                    when
syn region erlangAttribute                   start=/^-\%(vsn\|author\|copyright\|compile\|module\|export\)(/ end=/)\.\@=/ contains=erlangArityList,erlangAtom,erlangString
syn region erlangDefine                      start=/^-define(/ end=/)\.\@=/
syn region erlangPreCondit                   start=/^-\%(ifdef\|ifndef\|endif\)(/ end=/)\.\@=/

syn match erlangMacro                        /?\w\+/

syn match erlangBitType                      /\/\@<=\%(binary\|integer\|float\)/ contained
syn match erlangBitVariable                  /\<\%([0-9]\+\|[A-Z]\w\+\)/ contained
syn match erlangBitSize                      /:\@<=[0-9]\+/ contained
syn match erlangBinary                       /<<.*>>/ contains=erlangBitVariable,erlangDelimiter,erlangBitType,erlangBitSize,erlangBitError

" Link Erlang stuff to Vim groups {{{1
hi link erlangTodo           Todo
hi link erlangString         String
hi link erlangNoSpellString  String 
hi link erlangModifier       SpecialChar
hi link erlangStringModifier SpecialChar
hi link erlangComment        Comment
hi link erlangVariable       Identifier
hi link erlangInclude        Include
hi link erlangAttribute      Keyword
hi link erlangGBIF           Keyword
hi link erlangKeyword        Keyword
hi link erlangMacro          Macro
hi link erlangDefine         Define
hi link erlangPreCondit      PreCondit
hi link erlangPreProc        PreProc
hi link erlangDelimiter      Delimiter
"hi link erlangDelimiter      Normal
hi link erlangOperator       Operator
hi link erlangContitional    Conditional
hi link erlangGuard          Conditional
hi link erlangBoolean        Boolean
hi link erlangAtom           Normal
hi link erlangRecord         Structure
hi link erlangInteger        Number
hi link erlangFloat          Number
hi link erlangFloat          Number
hi link erlangFloat          Number
hi link erlangFloat          Number
hi link erlangHex            Number
hi link erlangBIF            Function
hi link erlangFun            Keyword
hi link erlangList           Structure
hi link erlangArityList      Structure
hi link erlangTuple          Structure
hi link erlangBinary         Structure
hi link erlangBitVariable    Identifier
hi link erlangBitType        Type
hi link erlangBitSize        Number

" }}}

" hi def link erlangFunctionHead  ErlangFunHead

let b:current_syntax = "erlang"

" vim: set foldmethod=marker:
