# lkirk-xmonad
Package containing my personal xmonad config and the infrastructure to rebuild via stack. It's not perfect, but it's a lot better than it was.

## Building
`stack` must be installed
```
sudo apt install \
	libx11-dev \
	libxft-dev \
	libxinerama-dev \
	libxrandr-dev \
	libxss-dev

stack build
```

## TODO:
* Find a way to install the xmobar binary (or use a different bar package?)
* Remove the xmobar binary?
* Clean up directory handling with scripts
