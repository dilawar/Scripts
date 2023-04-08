#!/bin/bash - 

# Like git -fxd
# The version is from here https://stackoverflow.com/a/9144984/1805129
# Changelog: Do not delete any .git folder or any .git* file.


set -o nounset                              # Treat unset variables as an error
set -x

# make sure this script exits with a non-zero return value if the
# current directory is not in a svn working directory
svn info >/dev/null || exit 1

svn status --no-ignore | grep '^[I?]' | cut -c 9- |
# setting IFS to the empty string ensures that any leading or
# trailing whitespace is not trimmed from the filename
while IFS= read -r f; do
    if [[ "${f}" = *".git"* ]]; then
        continue
    fi
    # tell the user which file is being deleted.  use printf
    # instead of echo because different implementations of echo do
    # different things if the arguments begin with hyphens or
    # contain backslashes; the behavior of printf is consistent
    printf '%s\n' "Deleting ${f} ..."
    # if rm -rf can't delete the file, something is wrong so bail
    rm -rf "${f}" || exit 1
done
