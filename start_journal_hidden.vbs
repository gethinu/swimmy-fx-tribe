' Auto-Journal Hidden Launcher
' This VBScript runs the batch file hidden (no window)
' Double-click this to start Auto-Journal in background

Set WshShell = CreateObject("WScript.Shell")
Set fso = CreateObject("Scripting.FileSystemObject")
' Dynamically resolve path to batch file (distro-independent)
currentDir = fso.GetParentFolderName(WScript.ScriptFullName)
WshShell.Run """" & currentDir & "\scripts\start_journal_windows.bat""", 0, False
