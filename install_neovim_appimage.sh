#!/usr/bin/env bash
(
	mkdir -p ~/.cache
	cd ~/.cache
	if [ ! -f ./nvim.appimage ]; then
		curl -LO https://github.com/neovim/neovim/releases/latest/download/nvim.appimage 
		chmod u+x nvim.appimage
	fi

	# mkdir -p $HOME/.local/bin
	# cp nvim.appimage $HOME/.local/bin/
	# $HOME/.local/bin/nvim.appimage
	chmod a+x nvim.appimage
	./nvim.appimage --appimage-extract 
	echo "Make sure ~/squashfs-root/usr/bin is in path"
)

