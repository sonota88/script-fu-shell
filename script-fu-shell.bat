@echo off

set RUBY_HOME=C:\ruby
set SCRIPT_FU_SHELL=C:\foo\script-fu-shell.rb

path=%RUBY_HOME%\bin;%path%

%RUBY_HOME%\bin\ruby.exe %SCRIPT_FU_SHELL% %1 %2 %3 %4 %5 %6 %7 %8 %9
