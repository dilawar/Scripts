#!/bin/bash
set -x 
set -e

sudo dnf install -y epel-release dnf-plugins-core
sudo dnf config-manager --set-enabled powertools
sudo dnf upgrade
sudo dnf install -y curl wget git cmake nmap htop neovim mutt

# docker
sudo dnf config-manager --add-repo=https://download.docker.com/linux/centos/docker-ce.repo
sudo dnf install -y docker-ce --nobest
sudo curl -L "https://github.com/docker/compose/releases/download/1.29.2/docker-compose-$(uname -s)-$(uname -m)" -o /usr/local/bin/docker-compose
sudo chmod +x /usr/local/bin/docker-compose
sudo systemctl enable docker
sudo systemctl start docker

# install neovim
./install_neovim_appimage.sh
rm -rf ~/.config/nvim 
git clone https://github.com/dilawar/nvim ~/.config/nvim

sudo dnf install -y python39 python39-devel 
./bootstrap.sh
