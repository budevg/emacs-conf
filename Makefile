
all: init.el

init.el:
	python PackageManagerNG.py init_file
profiling:
	python PackageManager.py init_file profiling

clean:
	rm -f init.el