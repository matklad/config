# - Grab iso from https://www.microsoft.com/software-download/windows11
# - install https://winfsp.dev/rel, win+services, enable VIrtIO-FS
# - https://aka.ms/getwinget
# - powershell -ExecutionPolicy Bypass -File Z:\init.ps1

winget install -e --id workman-layout.workman
winget install -e --id Microsoft.WindowsTerminal
winget install -e --id Git.Git
winget install -e --id BurntSushi.ripgrep.MSVC
winget install -e --id Microsoft.VisualStudio.2022.BuildTools
winget install -e --id Microsoft.VisualStudioCode
winget install -e --id GoLang.Go.1.20
winget install -e --id Microsoft.DotNet.SDK.8
winget install -e --id Microsoft.OpenJDK.21
