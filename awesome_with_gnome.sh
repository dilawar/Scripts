#!/bin/bash
gconftool-2 --type bool --set /apps/nautilus/preferences/show_desktop False 
 # Disable buggy Nautilus desktop thing
gconftool-2 --type string --set /desktop/gnome/applications/window_manager/current /usr/bin/awesome
