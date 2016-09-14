
.PHONY: rust cpp all

all: rust cpp

cpp:
#	g++ -std=c++11 -O3 FasterGame.cpp
	clang++ -std=c++11 -O3 FasterGame.cpp -o FasterGame_cpp.exe

rust:
	(cd rust; cargo rustc --release  -- -C lto )

# Uh this is giving me illegal instruction:
#	cargo rustc --release -- -C lto -C target-cpu=native
