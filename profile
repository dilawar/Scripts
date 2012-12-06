# ~/.profile: executed by the command interpreter for login shells.
# This file is not read by bash(1), if ~/.bash_profile or ~/.bash_login
# exists.
# see /usr/share/doc/bash/examples/startup-files for examples.
# the files are located in the bash-doc package.

# the default umask is set in /etc/profile; for setting the umask
# for ssh logins, install and configure the libpam-umask package.
export GITAWAREPROMPT=~/Scripts/bash/git-aware-prompt
source $GITAWAREPROMPT/main.sh
green=$'\e[1;32m'
magenta=$'\e[1;34m'
normal_colours=$'\e[m'
export PS1="\[$green\]\u@\h:\[$magenta\]\w\[$txtcyn\]\$git_branch\[$txtylw\]\$git_dirty\[$txtrst\]\$ \n"
