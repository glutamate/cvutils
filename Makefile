it:
	g++ -o record1 record1.cpp `pkg-config opencv --cflags --libs`
	g++ -o extract extract.cpp `pkg-config opencv --cflags --libs`