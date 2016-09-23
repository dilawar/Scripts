# unregister broken GHC packages. Run this a few times to resolve dependency rot in installed packages.                                                                                                                                     
function ghc-pkg-clean() {
    for p in `ghc-pkg check $* 2>&1  | grep problems | awk '{print $6}' | sed -e 's/:$//'`
    do
        echo unregistering $p; ghc-pkg $* unregister --force $p
    done
}
ghc-pkg-clean
