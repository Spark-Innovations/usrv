# Set up CCL

ipkg emacs gcc make m4 python subversion libc6-dev

cd
# svn co http://svn.clozure.com/publicsvn/openmcl/trunk/linuxx86/ccl
svn co http://svn.clozure.com/publicsvn/openmcl/release/1.8/linuxx86/ccl
cd ccl/lisp-kernel/linuxx8664
make
cd ../..
./lx86cl64 -n -e "(progn (ccl:rebuild-ccl :full t) (quit))"
sudo ln -s -f /home/ron/ccl/lx86cl64 /usr/local/bin/ccl

wget http://beta.quicklisp.org/quicklisp.lisp 
ccl -l quicklisp.lisp
rm quicklisp.lisp
