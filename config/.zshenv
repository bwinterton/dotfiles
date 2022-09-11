export VISUAL=vim
export EDITOR=vim

# Home bin
export PATH="$PATH:$HOME/bin"

# Exports for Go
export GOPATH=~/dev/go
export PATH="$PATH:$GOPATH/bin"

# Exports for Rust
export RUST_SRC_PATH=/usr/local/src/rust/src
export PATH="$PATH:$HOME/.cargo/bin"

# Activator/Scala Exports
export PATH="$PATH:/opt/activator/bin"

# Poetry bin
export PATH="$PATH:$HOME/.poetry/bin"

# Local bin
export PATH="$PATH:$HOME/.local/bin"

# MacOS Specifics
if [[ `uname` == 'Darwin' ]]; then

	# Homebrew Python 3 default
	export PATH="/usr/local/opt/python/libexec/bin:$PATH"

fi
