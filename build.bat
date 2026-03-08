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

set "ROOT_DIR=%~dp0"
if "%ROOT_DIR:~-1%"=="\" set "ROOT_DIR=%ROOT_DIR:~0,-1%"

set "BUILD_DIR_ABS=%ROOT_DIR%\%BUILD_DIR%"
set "COMPILER_DIR=%ROOT_DIR%\compiler"
set "EXE=xlang"

set "BYTECODEGEN_DIR=%ROOT_DIR%\tools\BytecodeGenerator"
set "TOOLS_OUT_DIR=%BUILD_DIR_ABS%\tools"

set "JAVA_LIB_DIR=%ROOT_DIR%\libs\java"
set "JAVA_LIB_OUT_DIR=%BUILD_DIR_ABS%\libs\java"

set "GRADLE_USER_HOME=%BUILD_DIR_ABS%\.gradle-home"
set "GRADLE_ARGS=--console=plain --no-daemon"

if not defined TERM set "TERM=dumb"

set "CMD=%~1"
if "%CMD%"=="" set "CMD=all"

if /I "%CMD%"=="all" goto all
if /I "%CMD%"=="compile" goto compile
if /I "%CMD%"=="tools" goto tools
if /I "%CMD%"=="java_lib" goto java_lib
if /I "%CMD%"=="clean" goto clean
if /I "%CMD%"=="clean_ide" goto clean_ide
if /I "%CMD%"=="rebuild" goto rebuild
if /I "%CMD%"=="-h" goto usage
if /I "%CMD%"=="--help" goto usage
if /I "%CMD%"=="help" goto usage

echo Unknown command: %CMD%
call :usage
exit /b 2

:all
call :compile || exit /b 1
call :tools || exit /b 1
call :java_lib || exit /b 1
exit /b 0

:compile
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
call gradlew.bat build %GRADLE_ARGS%
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
call gradlew.bat build %GRADLE_ARGS%
set "ERR=%ERRORLEVEL%"
popd >nul
if not "%ERR%"=="0" exit /b %ERR%

copy /Y "%JAVA_LIB_DIR%\build\libs\*" "%JAVA_LIB_OUT_DIR%\" >nul 2>nul
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
if "%CLEAN_VERBOSE%"=="1" (
    call gradlew.bat --stop %GRADLE_ARGS%
) else (
    call gradlew.bat --stop %GRADLE_ARGS% >nul 2>nul
)
set "ERR1=%ERRORLEVEL%"
if "%CLEAN_VERBOSE%"=="1" (
    call gradlew.bat clean %GRADLE_ARGS%
) else (
    call gradlew.bat clean %GRADLE_ARGS% >nul 2>nul
)
set "ERR2=%ERRORLEVEL%"

if "%ERR1%%ERR2%"=="00" (
    popd >nul
    exit /b 0
)

echo [INFO] retry gradle clean with fallback GRADLE_USER_HOME in "%GDIR%"
set "OLD_GRADLE_USER_HOME=%GRADLE_USER_HOME%"
set "GRADLE_USER_HOME=%TEMP%\\xlang-gradle-home"
if not exist "%GRADLE_USER_HOME%" mkdir "%GRADLE_USER_HOME%" >nul 2>nul
if "%CLEAN_VERBOSE%"=="1" (
    call gradlew.bat --stop %GRADLE_ARGS%
) else (
    call gradlew.bat --stop %GRADLE_ARGS% >nul 2>nul
)
set "ERR1=%ERRORLEVEL%"
if "%CLEAN_VERBOSE%"=="1" (
    call gradlew.bat clean %GRADLE_ARGS%
) else (
    call gradlew.bat clean %GRADLE_ARGS% >nul 2>nul
)
set "ERR2=%ERRORLEVEL%"
set "GRADLE_USER_HOME=%OLD_GRADLE_USER_HOME%"
set "OLD_GRADLE_USER_HOME="

if not "%ERR1%%ERR2%"=="00" (
    if not "%ERR1%"=="0" echo [WARN] gradle --stop failed in "%GDIR%"
    if not "%ERR2%"=="0" echo [WARN] gradle clean failed in "%GDIR%"
)

popd >nul
exit /b 0

:usage
echo Usage: build.bat [all^|compile^|tools^|java_lib^|clean^|clean_ide^|rebuild]
echo.
echo Env overrides:
echo   BUILD_DIR=out   (default: build)
echo   JOBS=24         (default: NUMBER_OF_PROCESSORS or 8)
echo   CLEAN_VERBOSE=1 (show gradle clean output)
exit /b 0
