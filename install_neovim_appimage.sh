#!/usr/bin/env bash
(
    CHANNEL=nightly
	mkdir -p ~/.cache
	cd ~/.cache
	if [ -f ./nvim.appimage ]; then
        rm -f ./nvim.appimage
    fi

    curl -LO https://github.com/neovim/neovim/releases/download/$CHANNEL/nvim.appimage
    chmod u+x nvim.appimage

	# mkdir -p $HOME/.local/bin
	# cp nvim.appimage $HOME/.local/bin/
	# $HOME/.local/bin/nvim.appimage
	chmod a+x nvim.appimage
	./nvim.appimage --appimage-extract
	mv squashfs-root ~

	if [ ! -d ~/.config/nvim ]; then
		cd ~/.config && git clone https://github.com/dilawar/nvim --recursive
	fi
	echo "Make sure ~/squashfs-root/usr/bin is in path"

	# From https://stackoverflow.com/a/28021305/1805129
	LINE='alias vim=$HOME/squashfs-root/usr/bin/nvim'
	FILE=$HOME/.bashrc
	grep -qF -- "$LINE" "$FILE" || echo "$LINE" >> "$FILE"

	source ~/.bashrc
)

echo "Delete ~/.cache/*.appimage if you like now"
