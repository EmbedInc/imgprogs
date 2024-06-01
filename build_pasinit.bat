@echo off
rem
rem   Set up for building a Pascal module.
rem
call build_vars

call src_getbase
call src_getfrom math math.ins.pas
call src_getfrom vect vect.ins.pas
call src_getfrom img img.ins.pas
call src_getfrom rend core rend.ins.pas

make_debug debug_switches.ins.pas
call src_builddate "%srcdir%"
