# Setup

## Emacs

    cd ~/
	git clone https://github.com/Bogdanp/.emacs.d.git
	cd .emacs.d
	git submodule init
	git submodule update


## Python support

Inside a shell:

	pip install flake8
	pip install virtualenv


Inside Emacs:

	M-x jedi:install-server RET


# On Windows

See the `windows` branch.
