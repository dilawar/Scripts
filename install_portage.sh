#!/bin/bash
#

if [ "`id -u`" != "0" ]; then
   echo ""
   echo "Su as root and try again."
   echo ""
   exit 1
fi
                                                                               
pythonV=`python -V 2>&1`
if [ "$pythonV" \< "Python 2.2.0" ];then
   echo ""
   echo "Install python 2.2 and then try again"
   echo ""
   exit 1
fi

mkdir -p /usr/lib/portage/bin
mkdir -p /usr/lib/portage/pym
mkdir -p /usr/portage/profiles
mkdir -p /etc/env.d
mkdir -p /etc/portage/profile
mkdir -p /var/log/portage
mkdir -p /var/tmp/portage
mkdir -p /etc/config-archive
mkdir -p /var/lib/init.d
mkdir -p /var/lib/portage
 
cd /tmp
rm -rf portage-2.*                                                                     
wget ftp://gentoo.mirrors.pair.com/distfiles/portage-2.0.*
portar=`/bin/ls portage-2.0.* 2>/dev/null |sort|tail -n 1`
if [ ! -f "${portar}" ];then
  echo ""
  echo "Mirror error. Try this script again after a while without any args".
  echo ""
  exit 1
fi
tar jxf ${portar}
                                                                           
cd portage-2.*/bin
cp * /usr/lib/portage/bin
export PATH=/usr/lib/portage/bin:$PATH
cd ../pym ; cp * /usr/lib/portage/pym/
cd ../man ;cp *.1 /usr/share/man/man1 ;cp *.5 /usr/share/man/man5
cd ../src/python-missingos
./setup.py install
cd ../sandbox-1.1
make && make install
cd ../../cnf; cp * /etc
                                                                           
if [ "`id portage`" == "" ]; then
  /usr/sbin/groupadd -g 250 portage
  /usr/sbin/useradd -d /var/tmp/portage -g portage -u 250 portage
fi
                                                                           
cd /usr/sbin
ln -sf ../lib/portage/bin/regenworld .
ln -sf ../lib/portage/bin/pkgmerge .
ln -sf ../lib/portage/bin/fixpackages .
ln -sf ../lib/portage/bin/etc-update .
ln -sf ../lib/portage/bin/env-update .
ln -sf ../lib/portage/bin/emerge-webrsync .
ln -sf ../lib/portage/bin/ebuild.sh .
ln -sf ../lib/portage/bin/ebuild .
ln -sf ../lib/portage/bin/dispatch-conf .
ln -sf ../lib/portage/bin/archive-conf .
                                                                           
cd /usr/bin
ln -sf ../lib/portage/bin/xpak .
ln -sf ../lib/portage/bin/repoman .
ln -sf ../lib/portage/bin/quickpkg .
ln -sf ../lib/portage/bin/portageq .
ln -sf ../lib/portage/bin/g-cpan.pl .
ln -sf ../lib/portage/bin/emerge .
               
export PATH=/usr/lib/portage/bin:$PATH                                                         
echo "CC=gcc;CXX=g++" > /etc/env.d/compilers.sh
myldpath="/lib:/usr/lib"
# before we call portage the first time, we should save our ld.so.conf
for i in $(cat /etc/ld.so.conf);do myldpath=$myldpath:$i;done
echo "LDPATH=\"${myldpath}\"" > /etc/env.d/10ldpath
. /etc/env.d/compilers.sh
#emerge sync
emerge-webrsync
#
# emerge sync should get /usr/portage/profiles
#
if [ -d /usr/portage/profiles/default-linux ];then
rm -f /etc/make.profile
ln -s /usr/portage/profiles/default-linux/x86/2004.2/gcc34 /etc/make.profile
else
echo ""
echo "Something wrong. Get /usr/portage/profiles dir from a working"
echo "Gentoo system and try again."
echo ""
exit 1
fi
                                                                           
# inject the "dangerous" packages first.
glibcP=`emerge -p -O glibc|grep ebuild|cut -d" " -f8`
emerge inject $glibcP
gccP=`emerge -p -O gcc|grep ebuild|cut -d" " -f8`
emerge inject $gccP
binP=`emerge -p -O binutils|grep ebuild|cut -d" " -f8`
emerge inject $binP
                                                                           
# fetch the baselayout.
emerge -f -O baselayout
                                                                           
# now inject it too.
baseP=`emerge -p -O baselayout|grep ebuild|cut -d" " -f8`
emerge inject $baseP

#was inject success
ret=`emerge -p baselayout|grep "ebuild   R"`

if [ "$ret" == "" ] ;then
echo "Inject has failed. Please verify and continue with cut & paste of rest of the steps"
echo "ERROR."
exit 1
fi
                                                                           
# if it fails with some errors, its fine.
emerge -O portage
                                                                           
cd /tmp
fileN=`ls /usr/portage/distfiles/rc-scripts-*|sort|tail -n1`
tar xjpf $fileN
cd rc-scripts*/sbin
cp depscan.sh /sbin; cp functions.sh /sbin
mkdir -p /lib/rcscripts/awk
cd ../src/awk
cp *.awk /lib/rcscripts/awk/
cd /etc/init.d
ln -s /sbin/depscan.sh .
ln -s /sbin/functions.sh .
                                                                           
emerge -O bison gawk

# FOLLOWING SHOULD NOT GIVE ANY ERRORS HERE. You screwed up if it did.
emerge -O portage
                                                                           
if ! fgrep -q "/etc/profile.env" /etc/profile ; then
echo ". /etc/profile.env" >> /etc/profile
fi
                                                                           
echo ""
echo "#############################################################"
echo "You are now free to enjoy portage. Make sure you tune"
echo "/etc/make.conf and other portage configuration files to take"
echo "advantage of portage. If you screwed up, its OK. Pick yourself"
echo "up and give it another go."
echo "#############################################################"
echo "" 
