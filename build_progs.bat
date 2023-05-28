@echo off
rem
rem   BUILD_PROGS
rem
rem   Build the executable programs from this source directory.
rem
setlocal
call build_pasinit

call src_prog %srcdir% image_blend
call src_prog %srcdir% image_copy
call src_prog %srcdir% image_crop
call src_prog %srcdir% image_dither
call src_prog %srcdir% image_filter
call src_prog %srcdir% image_flip
call src_prog %srcdir% image_hist
call src_prog %srcdir% image_info
call src_prog %srcdir% image_mask
call src_prog %srcdir% image_resize
