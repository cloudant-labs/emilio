@echo off
setlocal
set emilioscript=%~f0
escript.exe "%emilioscript:.cmd=%" %*
