;^j::
; from https://www.autohotkey.com/docs/Tutorial.htm#s2
MsgBox, Wow!
MsgBox, There are
Run, notepad.exe
WinActivate, Untitled - Notepad
WinWaitActive, Untitled - Notepad
Send, 7 lines{!}{Enter}
SendInput, inside the CTRL{+}J hotkey.
return