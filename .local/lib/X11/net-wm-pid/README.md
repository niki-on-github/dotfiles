# Reliable \_NET_WM_PID for X windows

This is intended to make X applications respect `_NET_WM_PID`, through applying a post-hook on `XCreateWindow`, `XCreateSimpleWindow` and `XReparentWindow` via `LD_PRELOAD`. I use this fix for my Xmonad Swallow extension where I have to rely on the XProperty `_NET_WM_PID`. This code was published by deepfire on [github](https://github.com/deepfire/ld-preload-xcreatewindow-net-wm-pid).

## Compile

```bash
gcc ld-preload-xcreatewindow.c -shared -lX11 -fPIC -o ld-preload-xcreatewindow.so
```

## Usage

add to `~/.xprofile`:

```
export LD_PRELOAD=$HOME/.local/lib/X11/net-wm-pid/ld-preload-xcreatewindow.so
```
