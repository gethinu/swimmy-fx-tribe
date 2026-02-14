Option Explicit

' Run a PowerShell .ps1 script with no visible window.
' Usage:
'   wscript.exe run_ps1_hidden.vbs <script.ps1> [args...]
'
' Notes:
' - Runs PowerShell hidden (window style 0).
' - Waits for completion and propagates the exit code.

If WScript.Arguments.Count < 1 Then
  WScript.Echo "Usage: run_ps1_hidden.vbs <script.ps1> [args...]"
  WScript.Quit 2
End If

Dim scriptPath
scriptPath = WScript.Arguments(0)

Dim shell
Set shell = CreateObject("WScript.Shell")

Dim fso
Set fso = CreateObject("Scripting.FileSystemObject")

On Error Resume Next
If fso.FileExists(scriptPath) Then
  shell.CurrentDirectory = fso.GetParentFolderName(fso.GetAbsolutePathName(scriptPath))
End If
On Error GoTo 0

Dim cmd
cmd = "powershell -NoProfile -ExecutionPolicy Bypass -File " & QuoteArg(scriptPath)

Dim i
For i = 1 To WScript.Arguments.Count - 1
  cmd = cmd & " " & QuoteArg(WScript.Arguments(i))
Next

Dim exitCode
exitCode = shell.Run(cmd, 0, True)
WScript.Quit exitCode

Private Function QuoteArg(ByVal s)
  QuoteArg = """" & Replace(s, """", """""") & """"
End Function

