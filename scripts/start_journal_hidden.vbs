' Auto-Journal Hidden Launcher
' This VBScript runs the batch file hidden (no window)
' Double-click this to start Auto-Journal in background

Set WshShell = CreateObject("WScript.Shell")
WshShell.Run """\\wsl$\Ubuntu\home\swimmy\swimmy\start_journal_windows.bat""", 0, False
