---
metaTitle: "PowerShell - PowerShell.exe Command-Line"
description: "Executing a command, Executing a script file"
---

# PowerShell.exe Command-Line



## Executing a command


The `-Command` parameter is used to specify commands to be executed on launch. It supports multiple data inputs.

### -Command <string>

You can specify commands to executed on launch as a string. Multiple semicolon `;`-separated statements may be executed.

```powershell
>PowerShell.exe -Command "(Get-Date).ToShortDateString()"
10.09.2016

>PowerShell.exe -Command "(Get-Date).ToShortDateString(); 'PowerShell is fun!'"
10.09.2016
PowerShell is fun!

```

### -Command { scriptblock }

The `-Command` parameter also supports a scriptblock input (one or multiple statements wrapped in braces `{ #code }`. This only works when calling `PowerShell.exe` from another Windows PowerShell-session.

```powershell
PS > powershell.exe -Command {
"This can be useful, sometimes..."
(Get-Date).ToShortDateString()
}
This can be useful, sometimes...
10.09.2016

```

### -Command -  (standard input)

You can pass in commands from the standard input by using `-Command -`. The standard input can come from `echo`, reading a file, a legacy console application etc.

```powershell
>echo "Hello World";"Greetings from PowerShell" | PowerShell.exe -NoProfile -Command -
Hello World
Greetings from PowerShell

```



## Executing a script file


You can specify a file to a `ps1`-script to execute it's content on launch using the `-File` parameter.

### Basic script

MyScript.ps1

```powershell
(Get-Date).ToShortDateString()
"Hello World"

```

Output:

```powershell
>PowerShell.exe -File Desktop\MyScript.ps1
10.09.2016
Hello World

```

### Using parameters and arguments

You can add parameters and/or arguments after filepath to use them in the script. Arguments will be used as values for undefined/available script-parameters, the rest will be available in the `$args`-array

MyScript.ps1

```powershell
param($Name)

"Hello $Name! Today's date it $((Get-Date).ToShortDateString())"
"First arg: $($args[0])"

```

Output:

```powershell
>PowerShell.exe -File Desktop\MyScript.ps1 -Name StackOverflow foo
Hello StackOverflow! Today's date it 10.09.2016
First arg: foo

```



#### Parameters


|Parameter|Description
|---|---|---|---|---|---|---|---|---|---
|-Help | -? | /?|Shows the help
|-File <FilePath> [<Args>]|Path to script-file that should be executed and arguments (optional)
|-Command { - | <script-block> [-args <arg-array>] | <string> [<CommandParameters>] }|Commands to be executed followed by arguments
|-EncodedCommand <Base64EncodedCommand>|Base64 encoded commands
|-ExecutionPolicy <ExecutionPolicy>|Sets the execution policy for this process only
|-InputFormat { Text | XML}|Sets input format for data sent to process. Text (strings) or XML (serialized CLIXML)
|-Mta|PowerShell 3.0+: Runs PowerShell in multi-threaded apartment (STA is default)
|-Sta|PowerShell 2.0: Runs PowerShell in a single-threaded apartment (MTA is default)
|-NoExit|Leaves PowerShell console running after executing the script/command
|-NoLogo|Hides copyright-banner at launch
|-NonInteractive|Hides console from user
|-NoProfile|Avoid loading of PowerShell profiles for machine or user
|-OutputFormat { Text | XML }|Sets output format for data returned from PowerShell. Text (strings) or XML (serialized CLIXML)
|-PSConsoleFile <FilePath>|Loads a pre-created console file that configures the environment (created using `Export-Console`)
|-Version <Windows PowerShell version>|Specify a version of PowerShell to run. Mostly used with `2.0`
|-WindowStyle <style>|Specifies whether to start the PowerShell process as a `normal`, `hidden`, `minimized` or `maximized` window.

