
all:
	echo Creating emacs configuration
	python autogen/builder.py --action create

skeleton:
	echo Creating emacs configuration skeleton
	python autogen/builder.py --action skeleton


clean:
	echo Cleaninig emacs configuration
	rm -f init.el early-init.el
