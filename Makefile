
all: init.el

init.el:
	python PackageManager.py init

clean:
	rm -f init.el