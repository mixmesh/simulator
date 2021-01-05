all: #priv/epfl_mobility/cabs.dat: priv/epfl_mobility/_cabs.txt
	(cd src && $(MAKE) all)
	(cd c_src && $(MAKE) all)

# Keep awhile. Has been put in priv/attic/.
priv/epfl_mobility/cabs.dat: priv/epfl_mobility/_cabs.txt
	perl -n -e '/<cab id=\"([^\"]*)\" updates=\"([^\"]*)\"/ && print $$1 . " " . $$2 . "\n"' priv/epfl_mobility/_cabs.txt > priv/epfl_mobility/cabs.dat

clean:
	rm -f priv/epfl_mobility/cabs.dat
	(cd src && $(MAKE) clean)
	(cd c_src && $(MAKE) clean)
