it:
	g++ -o record record.cpp `pkg-config opencv --cflags --libs`
	g++ -o record1 record1.cpp `pkg-config opencv --cflags --libs`