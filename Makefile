all: haskinator

haskinator :
	ghc -o haskinator --make -main-is Haskinator Haskinator.hs
	rm -f *.o *.hi *~

clean :
	rm -f haskinator *.o *.hi *~
