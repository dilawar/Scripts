NOTEDIR=$HOME/Work/notes
EXT=pandoc
[ -z "$EDITOR" ] && echo "You need to export EDITOR";

n() {
    # Remove the extension from the input.
    ARG="$*"
    ARG=${ARG%$EXT}
    $EDITOR $NOTEDIR/"$ARG".$EXT
}

# Searches for notes with given pattern. Using agrep is recommened.
nls() {
    GREP=`which agrep`
    if [ ! -f  $GREP ]; then
        echo "I use $GREP. Please install it. Continuing with grep"
        GREP=`which grep`
    fi
    if [[ "$GREP" == *"agrep"* ]]; then
        GREP="$GREP -4"
    fi
    ( cd $NOTEDIR && $GREP "$*" *.$EXT )
}

nsync() {
    cd $NOTEDIR && git pull && git diff && git add . && git commit -m "updating" && git push 
}
