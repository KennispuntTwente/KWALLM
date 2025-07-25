@echo off

REM Set paths
set Rscript="C:\Program Files\R\R-4.5.0\bin\Rscript.exe"
set Script="C:\Users\Public\tekstanalyse_met_llm\app-llm-server.R"
set LogDir="C:\Users\Public\tekstanalyse_met_llm\logs"

REM Create log directory if it doesn't exist
if not exist %LogDir% mkdir %LogDir%

REM Format timestamp
for /f %%i in ('wmic os get localdatetime ^| find "."') do set datetime=%%i
set timestamp=%datetime:~0,4%-%datetime:~4,2%-%datetime:~6,2%_%datetime:~8,2%-%datetime:~10,2%-%datetime:~12,2%

REM Log file path
set LogFile=%LogDir%\llm_log_%timestamp%.log

REM Run R script and capture all output and errors
%Rscript% %Script% > %LogFile% 2>&1
