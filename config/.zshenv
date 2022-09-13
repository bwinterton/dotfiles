export VISUAL=vim
export EDITOR=vim

# Initial Path Setup
# See .zshrc for notes
export NEW_PATH="\$PATH"

# Home bin
export NEW_PATH="$NEW_PATH:$HOME/bin"

# Exports for Go
export GOPATH=~/dev/go
export NEW_PATH="$NEW_PATH:$GOPATH/bin"

# Exports for Rust
export RUST_SRC_PATH=/usr/local/src/rust/src
export NEW_PATH="$NEW_PATH:$HOME/.cargo/bin"

# Activator/Scala Exports
export NEW_PATH="$NEW_PATH:/opt/activator/bin"

# Poetry bin
export NEW_PATH="$NEW_PATH:$HOME/.poetry/bin"

# Local bin
export NEW_PATH="$NEW_PATH:$HOME/.local/bin"

# MacOS Specifics
if [[ `uname` == 'Darwin' ]]; then
	# Homebrew prefix, cellar, and repository depends on architecture
	if [ `uname -p` == "arm" ]; then
		export HOMEBREW_PREFIX="/opt/homebrew";
		export HOMEBREW_CELLAR="/opt/homebrew/Cellar";
		export HOMEBREW_REPOSITORY="/opt/homebrew";
	else
		export HOMEBREW_PREFIX="/usr/local";
		export HOMEBREW_CELLAR="/usr/local/Cellar";
		export HOMEBREW_REPOSITORY="/usr/local/Homebrew";
	fi

	# Homebrew Python 3 default
	export NEW_PATH="$HOMEBREW_PREFIX/opt/python/libexec/bin:$NEW_PATH"

	# Homebrew Path, Man, and Info
	export NEW_PATH="$HOMEBREW_PREFIX/bin:$HOMEBREW_PREFIX/sbin${NEW_PATH+:$NEW_PATH}";
	export MANPATH="$HOMEBREW_PREFIX/share/man${MANPATH+:$MANPATH}:";
	export INFOPATH="$HOMEBREW_PREFIX/share/info:${INFOPATH:-}";
fi
