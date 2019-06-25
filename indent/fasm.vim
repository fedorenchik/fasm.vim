" Vim indent file
" Language:         Fasm
" Maintainer:       Splus <Splus at 56 dot com>
" Latest Revision:  2005-06-29

if exists("b:did_indent")
  finish
endif
let b:did_indent = 1


setlocal indentexpr=GetFasmIndent()
" default indentkeys value: 0{,0},:,0#,!^F,o,O,e
setlocal indentkeys+==if,=repeat,=virtual,=else,=ends
setlocal indentkeys-=0#


if exists("*GetFasmIndent")
  finish
endif

let s:cpo_save = &cpo
set cpo&vim


function GetFasmIndent()
  let lnum = prevnonblank(v:lnum - 1)
  if lnum == 0
    return 0
  endif


  let ind = indent(lnum)

  " Add a 'shiftwidth' after if, repeat, virtual ...
  let line = getline(lnum)
  if line =~ '^\s*\(if\|else\|else\s\+if\|repeat\|virtual\|struct\|union\)' 
			  \|| line =~ '^.*{' 
    if line !~ '}\s*$'
      let ind = ind + &sw
    endif
  endif

  " Subtract a 'shiftwidth' on an 'end' and if, repeat, virtual ...
  let line = getline(v:lnum)
  if line =~ '^\s*end\s\+\(if\|repeat\|virtual\)' 
			  \|| line =~ '^\s*else'
			  \|| line =~ '^\s*ends'
			  \|| line =~ '^\s*}'
    let ind = ind - &sw
  endif

  return ind
endfunction

let &cpo = s:cpo_save
unlet s:cpo_save
