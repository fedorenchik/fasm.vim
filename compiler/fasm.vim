" Vim compiler file
" Compiler:	Fasm - Flat Assembler
" Maintainer:	6+ <sixplus@users.sf.net>
" Last Change:	August 27 2004

if exists("current_compiler")
  finish
endif
let current_compiler = "fasm"

if exists(":CompilerSet") != 2		" older Vim always used :setlocal
  command -nargs=* CompilerSet setlocal <args>
endif

" A workable errorformat for Flat Assembler
CompilerSet errorformat=%E%f\ \[%l\]%.%#:,%Z%m,%Ierror:\ %m,%Z

" default make
CompilerSet makeprg=fasm
