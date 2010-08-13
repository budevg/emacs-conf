
all: init.el

init.el:
	python PackageManagerNG.py init_file
profiling:
	python PackageManagerNG.py init_file profiling

clean:
	rm -f init.el
help:
	echo target: all, profiling, clean