if status is-interactive
    # Commands to run in interactive sessions can go here
end

# Thanks https://fishshell.com/docs/current/interactive.html
function fish_hybrid_key_bindings --description \
"Vi-style bindings that inherit emacs-style bindings in all modes"
    for mode in default insert visual
        fish_default_key_bindings -M $mode
    end
    fish_vi_key_bindings --no-erase
end
set -g fish_key_bindings fish_hybrid_key_bindings

set ANDROID_HOME ~/Android/Sdk
set ANDROID_NDK_HOME ~/Android/Sdk/ndk/29.0.13113456/

set ONEAPI_TOKEN woof-flattop-rascal-flakily
set COMPOSER_MEMORY_LIMIT -1

set PATH /usr/local/bin $PATH
set PATH ~/.mutt/ $PATH
set PATH /home/dilawar/Scripts $PATH
set COMPOSER_MEMORY_LIMIT -1

# Created by `pipx` on 2025-05-30 07:32:31
set PATH $PATH /home/dilawar/.local/bin

alias python=python3

# bun
set --export BUN_INSTALL "$HOME/.bun"
set --export PATH $BUN_INSTALL/bin $PATH
