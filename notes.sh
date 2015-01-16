NOTEDIR=notes
EXT=txt
n() {
    $EDITOR $NOTEDIR/"$*".$EXT
}

nls() {
    ls -c $NOTEDIR | grep "$*"
}

nsync() {
    cd $NOTEDIR && git pull && git diff && git add . && git commit -m "updating" && git push 
}
