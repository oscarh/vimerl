" Vim syntax file
" Language:   Erlang
" Maintainer: Oscar Hellström <oscar@oscarh.net>
" URL:        http://personal.oscarh.net
" Version:    2006-06-28
" ------------------------------------------------------------------------------
" Thanks to Kre¹imir Mar¾iæ (Kresimir Marzic) <kmarzic@fly.srk.fer.hr>,
" stole some stuff from his syntax for erlang
" ------------------------------------------------------------------------------
" {{{1
" Options:
"
" To enable BIF highlighting put 
" let g:erlangHighlightBif=1
" in your vimrc file
"
" To get function head highlighting working, add a definition for 
" "ErlangFunHead" to your color scheme file and also put
" g:erlangHighlightFunHead=1
" in your vimrc file.
"
" To disable highlighting words (Identifiers / key-/reserved words put 
" g:erlangHighLightWords=0
" in your vimrc file.
" 
" To disable highlighting operators put
" g:erlangHighLightOperators=0
" in your vimrc file.
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

" Need to use - in keywords
setlocal iskeyword+=-

" Comments {{{1
" TODO / FIXME etc.
syn keyword erlangTodo          TODO FIXME contained
syn match erlangComment          /%.*/ contains=erlangTodo,@Spell

" Preprocessor stuff {{{1
syn keyword erlangInclude        -include -include_lib -import
syn keyword erlengDefine         -define -undef
syn keyword erlangPreProc        -record
syn keyword erlangPreCondit      -ifdef -ifndef -else -endif
syn match erlangMacro            /?[0-9A-Za-z_-]\+/

" Module attributes {{{1
syn keyword erlangAttribute      -compile -module -export -behaviour -author -vsn
syn keyword erlangAttribute      -doc -copyright -file

" String modifiers {{{1
syn match erlangModifier         /\~\a\|\\\a/ contained

" Sparator {{{1
syn match erlangSeparator        /\.\|#/

" Numbers {{{1
syn match erlangInteger    "[a-zA-Z]\@<![+-]\=\d\+[a-zA-Z]\@!"           contains=erlangSeparator
syn match erlangFloat      "[a-zA-Z]\@<![+-]\=\d\+.\d\+[a-zA-Z]\@!"      contains=erlangSeparator
syn match erlangFloat      "[a-zA-Z]\@<![+-]\=\d\+\(.\d\+\)\=[eE][+-]\=\d\+\(.\d\+\)\=[a-zA-Z]\@!" contains=erlangSeparator
syn match erlangFloat      "[a-zA-Z]\@<![+-]\=\d\+[#]\x\+[a-zA-Z]\@!"    contains=erlangSeparator
syn match erlangFloat      "[a-zA-Z]\@<![+-]\=[eE][+-]\=\d\+[a-zA-Z]\@!" contains=erlangSeparator
syn match erlangHex        "[a-zA-Z]\@<!$\x\+[a-zA-Z]\@!"                contains=erlangSeparator

" Variables {{{1
syn match erlangIdentifier       /\<\(\u\|_\)\w*\>/

" Atoms {{{1
syn region erlangAtom            start=/'/ end=/'/ contained

" Records {{{1
syn match erlangRecord          /#\a\w*\>/

" String {{{1
syn region erlangString    start=/"/ skip=/\\"/ end=/"/ contains=erlangModifier

" Delimiters {{{1
syn match erlangDelimiter        /:/

" Data structures {{{1
syn region erlangList  matchgroup=Normal start=/\[/ end=/\]/ contained contains=erlangIdentifier,erlangAtom,elangList,erlangTuple
syn region erlangTuple matchgroup=Normal start=/{/ end=/}/ contained contains=erlangIdentifier,erlangAtom,erlangList,erlangTuple

" Keywords {{{1
if (!exists("g:erlangHighLightWords")) || g:erlangHighLightWords
	" Conditions
	syn keyword erlangContitional    if else case of end not andalso orelse and or 
	syn keyword erlangContitional    receive after

	" Exceptions
	syn keyword erlangException      try catch begin

	" Funs 
	syn match erlangFun            ':\@<!fun(\@='

	" Reserved words
	syn keyword erlangKeyword        cond query let 
endif

" Operators {{{1
if (!exists("g:erlangHighLightOperators")) || g:erlangHighLightOperators
	syn match   erlangOperator       "+\|-\|*\|/"
	syn match   erlangOperator       "==\|/=\|=:=\|=/=\|<\|=<\|>\|>="
	syn match   erlangOperator       /++\|--\|=\|!\|<-/
	syn keyword erlangOperator       band bnot bor bsl bsr bxor div rem xor
endif

" All erlang BIF:s (as of R11B) {{{1
" Not highlighted by default
if exists("g:erlangHighlightBif") && g:erlangHighlightBif
" BIF:s
    syn match erlangBIF ':\@<!\<\(abs\|append_element\|apply\|atom_to_list\)(\@='
	syn match erlangBIF ':\@<!\<\(binary_to_list\|binary_to_term\|bump_reductions\)(\@='
	syn match erlangBIF ':\@<!\<\(cancel_timer\|check_process_code\|date\)\>(\@='
	syn match erlangBIF ':\@<!\<\(delete_module\|demonitor\|disconnect_node\|display\)(\@='
	syn match erlangBIF ':\@<!\<\(erase\|exit\|fault\|float\|float_to_binary\)(\@='
	syn match erlangBIF ':\@<!\<\(fun_info\|fun_to_list\|garbage_collect\|get\)(\@='
	syn match erlangBIF ':\@<!\<\(get_keys\|group_leader\|halt\|hash\|hd\|hibernate\)(\@='
	syn match erlangBIF ':\@<!\<\(integer_to_list\|is_alive\|is_builtin\)(\@='
	syn match erlangBIF ':\@<!\<\(length\|link\|list_to_atom\|list_to_binary\|list_to_float\)(\@='
	syn match erlangBIF ':\@<!\<\(list_to_integer\|list_to_pid\|list_to_tuple\|load_module\)(\@='
	syn match erlangBIF ':\@<!\<\(loaded\|localtime\|localtime_to_universaltime\|make_ref\)(\@='
	syn match erlangBIF ':\@<!\<\(make_tuple\|md5\|md5_final\|md5_init\|md5_update\|memory\)(\@='
	syn match erlangBIF ':\@<!\<\(module_loaded\|monitor\|monitor_node\|node\|nodes\|now\)(\@='
	syn match erlangBIF ':\@<!\<\(open_port\|phash\|phash2\|pid_to_list\|port_close\)(\@='
	syn match erlangBIF ':\@<!\<\(port_command\|port_connect\|port_control\|port_call\)(\@='
	syn match erlangBIF ':\@<!\<\(port_info\|port_to_list\|ports\|pre_loaded\|element\)(\@='
	syn match erlangBIF ':\@<!\<\(process_flag\|process_info\|processes\|purge_module\)(\@='
	syn match erlangBIF ':\@<!\<\(put\|read_timer\|ref_to_list\|register\|registered\)(\@='
	syn match erlangBIF ':\@<!\<\(resume_process\|round\|self\|send\|send_after\|spawn\)(\@='
	syn match erlangBIF ':\@<!\<\(send_nosuspend\|set_cookie\|setelement\|size\|spawn_link\)(\@='
	syn match erlangBIF ':\@<!\<\(spawn_opt\|split_binary\|start_timer\|statistics\)(\@='
	syn match erlangBIF ':\@<!\<\(suspend_process\|system_flag\|system_info\|system_monitor\)(\@='
	syn match erlangBIF ':\@<!\<\(term_to_binary\|time\|tl\|trace\|trace_info\|trace_pattern\)(\@='
	syn match erlangBIF ':\@<!\<\(trace_pattern\|trunc\|tuple_to_list\|universaltime\)(\@='
	syn match erlangBIF ':\@<!\<\(universaltime_to_localtime\|unlink\|unregister\|whereis\)(\@='
	syn match erlangBIF ':\@<!\<\(yield\|process_display\|send_nosuspend\|binary_to_float\)(\@='
	syn match erlangBIF ':\@<!\<\(ncat_binary\|loat_to_list\|get_cookie\|info\)(\@='
	syn match erlangBIF ':\@<!\<\(is_process_alive\)(\@='

endif

" BIF:s used in guards, all contained in guard expressions {{{1
syn keyword erlangGBIF is_atom is_binary is_constant             contained
syn keyword erlangGBIF is_float is_function is_integer is_list   contained
syn keyword erlangGBIF is_number is_pid is_port is_reference     contained
syn keyword erlangGBIF is_tuple is_record                        contained

syn keyword erlangGBIF atom binary constant float function       contained
syn keyword erlangGBIF integer list number pid port reference    contained
syn keyword erlangGBIF tuple record                              contained
syn keyword erlangGBIF abs element float hd length node          contained
syn keyword erlangGBIF round self size tl trunc                  contained


" Function heads {{{1
if  exists("g:erlangHighlightFunHead") && g:erlangHighlightFunHead
	"syn region erlangGuardArgs start=/(/ end=/)/ contains=erlangIdentifier contained
	syn region erlangGuard     start=/when/ end=/->/ contains=erlangGBIF,erlangInteger,erlangFloat,erlangIdentifier
	syn region erlangFunArgs   start=/(/ end=/)/ contains=erlangIdentifier,erlangList,erlangTuple,erlangString contained 
	syn region erlangFunHead   start='^\a[0-9A-Za-z_-]*(' end=')' contains=erlangFunArgs keepend
endif

" Link Erlang stuff to Vim groups {{{1
hi link erlangTodo         Todo
hi link erlangString       String
hi link erlangModifier     SpecialChar
hi link erlangComment      Comment
hi link erlangIdentifier   Identifier
hi link erlangInclude      Include
hi link erlangAttribute    Keyword
hi link erlangGBIF         Keyword
hi link erlangKeyword      Keyword
hi link erlangMacro        Macro
hi link erlengDefine       Define
hi link erlangPreCondit    PreCondit
hi link erlangPreProc      PreProc
hi link erlangDelimiter    Delimiter
hi link erlangOperator     Operator
hi link erlangContitional  Conditional
hi link erlangGuard        Conditional
hi link erlangAtom         Structure
hi link erlangRecord       Structure
hi link erlangInteger      Number
hi link erlangFloat        Number
hi link erlangFloat        Number
hi link erlangFloat        Number
hi link erlangFloat        Number
hi link erlangHex          Number
hi link erlangBIF          Function
hi link erlangFun          Keyword
hi link erlangList         Normal
hi link erlangTuple        Normal

" Define new highlight groups {{{1
hi def link erlangFunHead  ErlangFunHead
" }}}

let b:current_syntax = "erlang"

" vim: set foldmethod=marker:
