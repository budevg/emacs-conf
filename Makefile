
all: init.el

init.el:
	python PackageManager.py init
log:
	python PackageManager.py init true

clean:
	rm -f init.el