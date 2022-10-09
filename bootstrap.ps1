# 
# Bootstrap my Windows machine.
#
if (! Get-Command choco.exe -errorAction SilentlyContinue)
{
    Set-ExecutionPolicy Bypass -Scope Process -Force; [System.Net.ServicePointManager]::SecurityProtocol = [System.Net.ServicePointManager]::SecurityProtocol -bor 3072; iex ((New-Object System.Net.WebClient).DownloadString('https://community.chocolatey.org/install.ps1'))
}

RefreshEnv 

choco install -y visualstudio2019community
choco install -y windowsdriverkit11
choco install -y cmake
choco install -y rustup.install
choco install -y chezmoi
choco install -y git
choco install -y neovim
choco install -y processhacker
