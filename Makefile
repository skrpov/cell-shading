main: main.c glad.o
	cc -o $@ $^ -Wall -Wextra -I./glad/include `pkg-config --cflags --libs glfw3 gl` -g

glad.o: ./glad/src/glad.c
	cc -c -o $@ $^ -I./glad/include
