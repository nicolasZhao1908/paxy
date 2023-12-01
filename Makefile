clean: 
	rm -f **/*.beam **/*.dump

purge: 
	rm -f **/*.beam **/experiments/* **/*.dump

.PHONY: clean purge
