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

	# Homebrew Python 3 default
	export NEW_PATH="/opt/homebrew/opt/python/libexec/bin:$NEW_PATH"
	export HOMEBREW_PREFIX="/opt/homebrew";
	export HOMEBREW_CELLAR="/opt/homebrew/Cellar";
	export HOMEBREW_REPOSITORY="/opt/homebrew";
	export NEW_PATH="/opt/homebrew/bin:/opt/homebrew/sbin${NEW_PATH+:$NEW_PATH}";
	export MANPATH="/opt/homebrew/share/man${MANPATH+:$MANPATH}:";
	export INFOPATH="/opt/homebrew/share/info:${INFOPATH:-}";

fi
