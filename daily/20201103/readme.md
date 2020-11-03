## symlink in windows

(but need admin permission?)

```
New-Item -Type SymbolicLink <link name> -Value <target file>
```


```
New-Item -Type SymbolicLink v2src_win -Value ..\..\shapes\v2
New-Item -Type SymbolicLink v3src_win -Value ..\..\shapes\v3
New-Item -Type SymbolicLink v2dst_win -Value ..\..\shapes\expected
New-Item -Type SymbolicLink v3dst_win -Value ..\..\shapes\expected
New-Item -Type SymbolicLink legacy_src_win -Value legacy_src
New-Item -Type SymbolicLink legacy_dst_win -Value legacy_dst
```

### hmm

using `New-Item` generates symlik as absolute path, if you want to need relative path's version, then

```
cmd /C mklink /D <link name> .\mytarget
```

```
cmd /C mklink /D v2src_win ..\..\shapes\v2
cmd /C mklink /D v3src_win ..\..\shapes\v3
cmd /C mklink /D v2dst_win ..\..\shapes\expected
cmd /C mklink /D v3dst_win ..\..\shapes\expected
cmd /C mklink /D legacy_src_win legacy_src
cmd /C mklink /D legacy_dst_win legacy_dst
```

### hmm

this option is needed?

```
$ git config --global core.symlinks true
```


- https://took.jp/post-447/
- https://itojisan.xyz/trouble/17155/

To modify local security policy is needed.

activate-gpedit.bat

```
@echo off
pushd "%~dp0"
  dir /b %SystemRoot%\servicing\Packages\Microsoft-Windows-GroupPolicy-ClientExtensions-Package~3*.mum >List.txt
dir /b %SystemRoot%\servicing\Packages\Microsoft-Windows-GroupPolicy-ClientTools-Package~3*.mum >>List.txt
for /f %%i in ('findstr /i . List.txt 2^>nul') do dism /online /norestart /add-package:"%SystemRoot%\servicing\Packages\%%i"
 pause
```

## githere

```
alias githere='git branch  | grep '^\*' | cut -d " " -f 2'
```
