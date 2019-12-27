@echo off
rem
rem   Define the variables for running builds from this source library.
rem
set srcdir=imgprogs
set buildname=
call treename_var "(cog)source/imgprogs" sourcedir
set libname=
set fwname=
call treename_var "(cog)src/%srcdir%/debug_%fwname%.bat" tnam
make_debug "%tnam%"
call "%tnam%"
