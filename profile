# ~/.profile: executed by the command interpreter for login shells.
# This file is not read by bash(1), if ~/.bash_profile or ~/.bash_login
# exists.
# see /usr/share/doc/bash/examples/startup-files for examples.
# the files are located in the bash-doc package.

# the default umask is set in /etc/profile; for setting the umask
# for ssh logins, install and configure the libpam-umask package.

source ~/Scripts/git_prompt.sh
export PS1='\u@\h\[\033[01;30m\] \w\[\033[01;34m\]$(__git_ps1 " (%s)")\[\033[01;31m\]\[\033[00m\] '
source ~/Scripts/svn_prompt.sh
export PS1="$PS1\$(svn_prompt)$\n"
