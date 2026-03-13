@echo off
setlocal EnableExtensions

rem ---------------- Config (override via env) ----------------
if not defined BUILD_DIR set "BUILD_DIR=build"

if not defined JOBS (
    if defined NUMBER_OF_PROCESSORS (
        set "JOBS=%NUMBER_OF_PROCESSORS%"
    ) else (
        set "JOBS=8"
    )
)

if not defined CABAL_UPDATE set "CABAL_UPDATE=0"

set "ROOT_DIR=%~dp0"
if "%ROOT_DIR:~-1%"=="\" set "ROOT_DIR=%ROOT_DIR:~0,-1%"

set "BUILD_DIR_ABS=%ROOT_DIR%\%BUILD_DIR%"
set "COMPILER_DIR=%ROOT_DIR%\compiler"
set "EXE=xlang"

set "BYTECODEGEN_DIR=%ROOT_DIR%\tools\BytecodeToolkit"
set "TOOLS_OUT_DIR=%BUILD_DIR_ABS%\tools"

set "JAVA_LIB_DIR=%ROOT_DIR%\libs\java"
set "JAVA_LIB_OUT_DIR=%BUILD_DIR_ABS%\libs\java"

set "GRADLE_USER_HOME=%BUILD_DIR_ABS%\.gradle-home"
set "GRADLE_ARGS=--console=plain --no-daemon"

set "CMD="
set "SHOW_HELP=0"
set "ARG_ERROR="

:parse_args
if "%~1"=="" goto parse_done
set "ARG=%~1"

if /I "%ARG%"=="-h" (
    set "SHOW_HELP=1"
    shift
    goto parse_args
)
if /I "%ARG%"=="--help" (
    set "SHOW_HELP=1"
    shift
    goto parse_args
)
if /I "%ARG%"=="help" (
    set "SHOW_HELP=1"
    shift
    goto parse_args
)

if /I "%ARG%"=="-u" (
    set "CABAL_UPDATE=1"
    shift
    goto parse_args
)
if /I "%ARG%"=="--update" (
    set "CABAL_UPDATE=1"
    shift
    goto parse_args
)

if /I "%ARG%"=="all"       call :set_cmd all       || goto parse_fail
if /I "%ARG%"=="build"     call :set_cmd build     || goto parse_fail
if /I "%ARG%"=="compile"   call :set_cmd compile   || goto parse_fail
if /I "%ARG%"=="update"    call :set_cmd update    || goto parse_fail
if /I "%ARG%"=="tools"     call :set_cmd tools     || goto parse_fail
if /I "%ARG%"=="java_lib"  call :set_cmd java_lib  || goto parse_fail
if /I "%ARG%"=="clean"     call :set_cmd clean     || goto parse_fail
if /I "%ARG%"=="clean_ide" call :set_cmd clean_ide || goto parse_fail
if /I "%ARG%"=="rebuild"   call :set_cmd rebuild   || goto parse_fail
if /I "%ARG%"=="all"       (shift & goto parse_args)
if /I "%ARG%"=="build"     (shift & goto parse_args)
if /I "%ARG%"=="compile"   (shift & goto parse_args)
if /I "%ARG%"=="update"    (shift & goto parse_args)
if /I "%ARG%"=="tools"     (shift & goto parse_args)
if /I "%ARG%"=="java_lib"  (shift & goto parse_args)
if /I "%ARG%"=="clean"     (shift & goto parse_args)
if /I "%ARG%"=="clean_ide" (shift & goto parse_args)
if /I "%ARG%"=="rebuild"   (shift & goto parse_args)

if /I "%ARG%"=="-j" (
    if "%~2"=="" (
        set "ARG_ERROR=missing value for -j"
        goto parse_fail
    )
    set "JOBS=%~2"
    call :validate_jobs "%JOBS%" || (
        set "ARG_ERROR=invalid jobs value: %JOBS%"
        goto parse_fail
    )
    shift
    shift
    goto parse_args
)

if /I "%ARG:~0,2%"=="-j" (
    set "JOBS=%ARG:~2%"
    call :validate_jobs "%JOBS%" || (
        set "ARG_ERROR=invalid jobs value: %JOBS%"
        goto parse_fail
    )
    shift
    goto parse_args
)

if /I "%ARG%"=="--jobs" (
    if "%~2"=="" (
        set "ARG_ERROR=missing value for --jobs"
        goto parse_fail
    )
    set "JOBS=%~2"
    call :validate_jobs "%JOBS%" || (
        set "ARG_ERROR=invalid jobs value: %JOBS%"
        goto parse_fail
    )
    shift
    shift
    goto parse_args
)

if /I "%ARG:~0,7%"=="--jobs=" (
    set "JOBS=%ARG:~7%"
    call :validate_jobs "%JOBS%" || (
        set "ARG_ERROR=invalid jobs value: %JOBS%"
        goto parse_fail
    )
    shift
    goto parse_args
)

set "ARG_ERROR=Unknown argument: %ARG%"
goto parse_fail

:parse_done
if "%SHOW_HELP%"=="1" goto usage
if not defined CMD set "CMD=all"
if /I "%CMD%"=="build" set "CMD=all"

if /I "%CMD%"=="all" goto all
if /I "%CMD%"=="compile" goto compile
if /I "%CMD%"=="update" goto update
if /I "%CMD%"=="tools" goto tools
if /I "%CMD%"=="java_lib" goto java_lib
if /I "%CMD%"=="clean" goto clean
if /I "%CMD%"=="clean_ide" goto clean_ide
if /I "%CMD%"=="rebuild" goto rebuild

set "ARG_ERROR=Unknown command: %CMD%"
goto parse_fail

:parse_fail
if defined ARG_ERROR echo %ARG_ERROR%
call :usage
exit /b 2

:maybe_update
if "%CABAL_UPDATE%"=="1" (
    call :update
    exit /b %ERRORLEVEL%
)
exit /b 0

:all
call :compile || exit /b 1
call :tools || exit /b 1
call :java_lib || exit /b 1
exit /b 0

:update
pushd "%COMPILER_DIR%" >nul || exit /b 1
cabal update
set "ERR=%ERRORLEVEL%"
popd >nul
exit /b %ERR%

:compile
call :maybe_update || exit /b 1
if not exist "%BUILD_DIR_ABS%" mkdir "%BUILD_DIR_ABS%" || exit /b 1

pushd "%COMPILER_DIR%" >nul || exit /b 1
cabal build -j%JOBS% "exe:%EXE%"
if errorlevel 1 (
    popd >nul
    exit /b 1
)

set "EXE_PATH="
for /f "usebackq delims=" %%I in (`cabal list-bin "exe:%EXE%"`) do set "EXE_PATH=%%I"
if not defined EXE_PATH (
    echo Failed to locate built executable for exe:%EXE%.
    popd >nul
    exit /b 1
)
popd >nul

copy /Y "%EXE_PATH%" "%BUILD_DIR_ABS%\" >nul || exit /b 1
echo [OK] copied: %EXE_PATH% to %BUILD_DIR_ABS%\
exit /b 0

:tools
if not exist "%TOOLS_OUT_DIR%" mkdir "%TOOLS_OUT_DIR%" || exit /b 1

pushd "%BYTECODEGEN_DIR%" >nul || exit /b 1
set "GRADLE_USER_HOME=%GRADLE_USER_HOME%"
call gradlew.bat build -PxlangJobs=%JOBS% %GRADLE_ARGS%
set "ERR=%ERRORLEVEL%"
popd >nul
if not "%ERR%"=="0" exit /b %ERR%

copy /Y "%BYTECODEGEN_DIR%\build\libs\*.jar" "%TOOLS_OUT_DIR%\" >nul 2>nul
echo [OK] tools jars copied to: %TOOLS_OUT_DIR%\
exit /b 0

:java_lib
if not exist "%JAVA_LIB_OUT_DIR%" mkdir "%JAVA_LIB_OUT_DIR%" || exit /b 1

pushd "%JAVA_LIB_DIR%" >nul || exit /b 1
set "GRADLE_USER_HOME=%GRADLE_USER_HOME%"
call gradlew.bat build -PxlangJobs=%JOBS% %GRADLE_ARGS%
set "ERR=%ERRORLEVEL%"
popd >nul
if not "%ERR%"=="0" exit /b %ERR%

copy /Y "%JAVA_LIB_DIR%\build\libs\*" "%JAVA_LIB_OUT_DIR%\" >nul 2>nul
copy /Y "%JAVA_LIB_DIR%\build\runtime-libs\*.jar" "%JAVA_LIB_OUT_DIR%\" >nul 2>nul
echo [OK] java lib outputs copied to: %JAVA_LIB_OUT_DIR%\
exit /b 0

:clean
echo [CLEAN] cabal clean in "%COMPILER_DIR%"
pushd "%COMPILER_DIR%" >nul 2>nul
if not errorlevel 1 (
    cabal clean
    popd >nul
)

echo [CLEAN] gradle clean in "%BYTECODEGEN_DIR%"
call :gradle_clean "%BYTECODEGEN_DIR%"
echo [CLEAN] gradle clean in "%JAVA_LIB_DIR%"
call :gradle_clean "%JAVA_LIB_DIR%"

if exist "%BUILD_DIR_ABS%\tools" rmdir /s /q "%BUILD_DIR_ABS%\tools"
if exist "%BUILD_DIR_ABS%\libs" rmdir /s /q "%BUILD_DIR_ABS%\libs"
if exist "%BUILD_DIR_ABS%\%EXE%" del /f /q "%BUILD_DIR_ABS%\%EXE%" >nul 2>nul
del /f /q "%BUILD_DIR_ABS%\*.exe" >nul 2>nul

call :clean_ide

echo clean done
exit /b 0

:clean_ide
if exist "%ROOT_DIR%\.vscode" rmdir /s /q "%ROOT_DIR%\.vscode"
if exist "%ROOT_DIR%\.idea" rmdir /s /q "%ROOT_DIR%\.idea"

if exist "%COMPILER_DIR%\.vscode" rmdir /s /q "%COMPILER_DIR%\.vscode"
if exist "%COMPILER_DIR%\.idea" rmdir /s /q "%COMPILER_DIR%\.idea"

if exist "%BYTECODEGEN_DIR%\.vscode" rmdir /s /q "%BYTECODEGEN_DIR%\.vscode"
if exist "%BYTECODEGEN_DIR%\.idea" rmdir /s /q "%BYTECODEGEN_DIR%\.idea"

if exist "%JAVA_LIB_DIR%\.vscode" rmdir /s /q "%JAVA_LIB_DIR%\.vscode"
if exist "%JAVA_LIB_DIR%\.idea" rmdir /s /q "%JAVA_LIB_DIR%\.idea"

echo [OK] removed .vscode/.idea under root/compiler/tools/libs
exit /b 0

:rebuild
call :clean || exit /b 1
call :all
exit /b %ERRORLEVEL%

:gradle_clean
set "GDIR=%~1"
if not exist "%GDIR%\gradlew.bat" exit /b 0

pushd "%GDIR%" >nul 2>nul
if errorlevel 1 exit /b 0

set "GRADLE_USER_HOME=%GRADLE_USER_HOME%"
call gradlew.bat --stop %GRADLE_ARGS% >nul 2>nul
call gradlew.bat clean %GRADLE_ARGS% >nul 2>nul
popd >nul
exit /b 0

:usage
echo Usage: build.bat [all^|build^|compile^|update^|tools^|java_lib^|clean^|clean_ide^|rebuild] [-jN ^| -j N ^| --jobs=N ^| --jobs N] [--update]
echo.
echo Env overrides:
echo   BUILD_DIR=out      (default: build)
echo   JOBS=24            (default: NUMBER_OF_PROCESSORS or 8)
echo   CABAL_UPDATE=1     (run cabal update before compile/all/rebuild)
exit /b 0

:set_cmd
if not defined CMD (
    set "CMD=%~1"
    exit /b 0
)
if /I "%CMD%"=="%~1" exit /b 0
set "ARG_ERROR=multiple commands provided: %CMD% and %~1"
exit /b 1

:validate_jobs
set "JOBS_CAND=%~1"
echo %JOBS_CAND%| findstr /r "^[1-9][0-9]*$" >nul
if errorlevel 1 exit /b 1
exit /b 0