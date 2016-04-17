all: $(addprefix bin/, $(patsubst %.hs, %, $(wildcard *.hs)))

bin/%: %.hs
	mkdir -p bin
	ghc $< -o $@
	rm *.hi *.o

clean:
	rm -rf bin


