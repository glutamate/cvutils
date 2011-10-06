it:
	g++ -o record1 record1.cpp `pkg-config opencv --cflags --libs`
	g++ -o extract extract.cpp `pkg-config opencv --cflags --libs`
	g++ -o trackc trackc.cpp `pkg-config opencv --cflags --libs`

prof:
	rm -f frame*.png && sudo cabal install --global -p --enable-executable-profiling && track mixed.png ~/Dropbox/woodlice/wl0.avi 1027 653 '-3.34' +RTS -prof -sstderr