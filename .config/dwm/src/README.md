# dwm

dwm is a dynamic window manager for X. 


## setup
```bash
git clone [URL]
git checkout stable
sudo make clean install
```


## update
```bash
git checkout master
git pull https://git.suckless.org/dwm master
git checkout stable
git merge [NEWEST_TAG]
# ...
git push --all origin
```


## push changes to local git repository
this transmits both the current master state and the modified stable branch.
```bash
git push --all origin
```


## patches
list of applied patches:
- [actualfullscreen](https://dwm.suckless.org/patches/actualfullscreen/)
- [autoresize](https://dwm.suckless.org/patches/autoresize/)
- [autostart](https://dwm.suckless.org/patches/autostart/)
- [center](https://dwm.suckless.org/patches/center/)
- [columns](https://dwm.suckless.org/patches/columns/)
- [ewmhtags](https://dwm.suckless.org/patches/ewmhtags/)
- [cyclelayouts](https://dwm.suckless.org/patches/cyclelayouts/)
- [focusonnetactive](https://dwm.suckless.org/patches/focusonnetactive/)
- [fullgaps](https://dwm.suckless.org/patches/fullgaps/)
- [horizgrid](https://dwm.suckless.org/patches/horizgrid/)
- [movestack](https://dwm.suckless.org/patches/movestack/)
- [namedscratchpads](https://dwm.suckless.org/patches/namedscratchpads/)
- [pertag](https://dwm.suckless.org/patches/pertag/)
- [retarting](https://dwm.suckless.org/patches/restartsig/)
- [rmaster](https://dwm.suckless.org/patches/rmaster/)
- [smartborders](https://dwm.suckless.org/patches/smartborders/)
- [sticky](https://dwm.suckless.org/patches/sticky/)
- [swallow](https://dwm.suckless.org/patches/swallow/)
- [systray](https://dwm.suckless.org/patches/systray/)
- [three-column](https://dwm.suckless.org/patches/three-column/)
- [xrdb](https://dwm.suckless.org/patches/xrdb/)


## about
dwm manages windows in tiled, monocle and floating layouts. All of the layouts can be applied dynamically, optimising the environment for the application in use and the task performed.

In tiled layout windows are managed in a master and stacking area. The master area contains the window which currently needs most attention, whereas the stacking area contains all other windows. In monocle layout all windows are maximised to the screen size. In floating layout windows can be resized and moved freely. Dialog windows are always managed floating, regardless of the layout applied.

Windows are grouped by tags. Each window can be tagged with one or multiple tags. Selecting certain tags displays all windows with these tags.

Each screen contains a small status bar which displays all available tags, the layout, the number of visible windows, the title of the focused window, and the text read from the root window name property, if the screen is focused. A floating window is indicated with an empty square and a maximised floating window is indicated with a filled square before the windows title. The selected tags are indicated with a different color. The tags of the focused window are indicated with a filled square in the top left corner. The tags which are applied to one or more windows are indicated with an empty square in the top left corner.

