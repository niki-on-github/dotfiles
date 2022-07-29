# -*- coding: utf-8 -*-
# Copyright (c) 2011-2012 Dustin Lacewell
# Copyright (c) 2011 Mounier Florian
# Copyright (c) 2012 Craig Barnes
# Copyright (c) 2012 Maximilian Köhl
# Copyright (c) 2012, 2014-2015 Tycho Andersen
# Copyright (c) 2013 jpic
# Copyright (c) 2013 babadoo
# Copyright (c) 2013 Jure Ham
# Copyright (c) 2013 Tao Sauvage
# Copyright (c) 2014 ramnes
# Copyright (c) 2014 Sean Vig
# Copyright (c) 2014 dmpayton
# Copyright (c) 2014 dequis
# Copyright (c) 2014 Florian Scherf
# Copyright (c) 2017 Dirk Hartmann
# Copyright (c) 2021 Jakob Helmecke
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.

from libqtile.layout.base import _SimpleLayoutBase


class MonadTall(_SimpleLayoutBase):
    """Emulate the behavior of XMonad's default tiling scheme.

    Master-Pane:

    A master pane that contains a set number of windows takes up a vertical portion of
    the screen_rect based on the ratio setting. This ratio can be adjusted with
    the ``cmd_grow_master`` and ``cmd_shrink_master``.

    ::

        ---------------------
        |            |      |
        |            |      |
        |            |      |
        |            |      |
        |            |      |
        |            |      |
        ---------------------

    Using the ``cmd_flip`` method will switch which horizontal side the master
    pane will occupy. The master pane is considered the "top" of the stack.

    ::

        ---------------------
        |      |            |
        |      |            |
        |      |            |
        |      |            |
        |      |            |
        |      |            |
        ---------------------

    Slave-panes:

    Occupying the rest of the screen_rect are one or more slave panes. The
    slave panes will share the vertical space of the screen_rect.

    ::

        ---------------------
        |            |      |
        |            |______|
        |            |      |
        |            |______|
        |            |      |
        |            |      |
        ---------------------

    Panes can be moved with the ``cmd_shuffle_up`` and ``cmd_shuffle_down``
    methods. As mentioned the master pane is considered the top of the stack;
    moving up is counter-clockwise and moving down is clockwise.

    The opposite is true if the layout is "flipped".

    ::

        ---------------------          ---------------------
        |            |  2   |          |   2   |           |
        |            |______|          |_______|           |
        |            |  3   |          |   3   |           |
        |     1      |______|          |_______|     1     |
        |            |  4   |          |   4   |           |
        |            |      |          |       |           |
        ---------------------          ---------------------


    Normalizing/Resetting:

    To restore master slave ratio use the ``cmd_normalize`` method.

    To reset the layout to its default state, including the master
    windows, master slave ratio and flip, use the ``cmd_reset`` method.

    Maximizing:

    To toggle a maximized layout, showing a single window, simply use the
    ``cmd_maximize`` on a focused client.

    Suggested Bindings::
        # these mirror the xmonad default config

        Key([modkey], "j", lazy.layout.down()),
        Key([modkey], "k", lazy.layout.up()),
        Key([modkey, "shift"], "j", lazy.layout.shuffle_down()),
        Key([modkey, "shift"], "k", lazy.layout.shuffle_up()),
        Key([modkey], "h", lazy.layout.shink_master()),
        Key([modkey], "l", lazy.layout.grow_master()),
        Key([modkey], "n", lazy.layout.normalize()),
        Key([modkey], "m", lazy.layout.master()),
        Key([modkey], "comma", lazy.layout.decrease_nmaster()),
        Key([modkey], "period", lazy.layout.increase_nmaster()),
        Key([modkey], "Return", lazy.layout.swap_master())

        # keybindings not in xmonad default config

        Key([modkey, "shift"], "n", lazy.layout.reset()),
        Key([modkey], "space", lazy.layout.flip()),
        Key([modkey, "shift"], "space", lazy.layout.flip_master()),
        Key([modkey, "shift"], "m", lazy.layout.maximize()),
    """

    _left = 0
    _right = 1
    _vert = 0
    _hori = 1
    _med_ratio = 0.5

    defaults = [
        ("border_focus", "#ff0000", "Border colour(s) for the focused window."),
        ("border_normal", "#000000", "Border colour(s) for un-focused windows."),
        ("border_width", 2, "Border width."),
        ("single_border_width", None, "Border width for single window"),
        ("single_margin", None, "Margin size for single window"),
        ("margin", 0, "Margin of the layout"),
        (
            "ratio",
            0.5,
            "The percent of the screen-space the master pane should occupy by default.",
        ),
        (
            "min_ratio",
            0.25,
            "The percent of the screen-space the master pane should occupy at minimum.",
        ),
        (
            "max_ratio",
            0.75,
            "The percent of the screen-space the master pane should occupy at maximum.",
        ),
        (
            "align",
            _left,
            "Which side master plane will be placed (one of ``MonadTall._left`` or "
            "``MonadTall._right``)",
        ),
        ("change_ratio", 0.05, "Resize ratio"),
        (
            "new_client_position",
            "before_current",
            "Place new windows: "
            " after_current - after the active window."
            " before_current - before the active window,"
            " top - at the top of the stack,"
            " bottom - at the bottom of the stack,",
        ),
        (
            "master_length",
            1,
            "Amount of windows displayed in the master stack. Surplus windows will be moved to "
            "the slave stack.",
        ),
        (
            "orientation",
            _vert,
            "Orientation in which master windows will be "
            "placed (one of ``MonadTall._vert`` or ``MonadTall._hori``)",
        ),
        ("maximized", False, "Start maximized"),
    ]

    def __init__(self, **config):
        _SimpleLayoutBase.__init__(self, **config)
        self.add_defaults(MonadTall.defaults)
        if self.single_border_width is None:
            self.single_border_width = self.border_width
        if self.single_margin is None:
            self.single_margin = self.margin
        self.screen_rect = None

    @property
    def focused(self):
        return self.clients.current_index

    @property
    def master_windows(self):
        return self.clients[: self.master_length]

    @property
    def slave_windows(self):
        return self.clients[self.master_length :]

    def clone(self, group):
        "Clone layout for other groups"
        c = _SimpleLayoutBase.clone(self, group)
        c.screen_rect = group.screen.get_rect() if group.screen else None
        c.ratio = self.ratio
        c.align = self.align
        c.orientation = self.orientation
        return c

    def add(self, client):
        "Add client to layout"
        self.clients.add(client, client_position=self.new_client_position)
        self.do_normalize = True

    def remove(self, client):
        "Remove client from layout"
        self.do_normalize = True
        return self.clients.remove(client)

    def cmd_normalize(self, redraw=True):
        "Evenly distribute screen-space between master and slave pane"
        if redraw:
            self.ratio = self._med_ratio
            self.group.layout_all()
        self.do_normalize = False

    def cmd_reset(self, redraw=True):
        "Reset Layout."
        self.ratio = self._med_ratio
        if self.align == self._right:
            self.align = self._left
        if self.orientation == self._hori:
            self.orientation = self._vert
        self.master_length = 1
        self.cmd_normalize(redraw)

    def cmd_maximize(self):
        "Grow the currently focused client to the max size"
        if self.maximized:
            self.maximized = False
        else:
            self.maximized = True
        self.group.layout_all()

    def configure(self, client, screen_rect):
        "Position client based on order and sizes"
        self.screen_rect = screen_rect

        if self.do_normalize:
            self.cmd_normalize(False)

        # if client not in this layout
        if not self.clients or client not in self.clients:
            client.hide()
            return

        # determine focus border-color
        if client.has_focus:
            px = self.border_focus
        else:
            px = self.border_normal

        # single client - fullscreen
        if len(self.clients) == 1 or self.maximized:
            if self.clients and client is self.clients.current_client:
                client.place(
                    self.screen_rect.x,
                    self.screen_rect.y,
                    self.screen_rect.width - 2 * self.single_border_width,
                    self.screen_rect.height - 2 * self.single_border_width,
                    self.single_border_width,
                    px,
                    margin=self.single_margin,
                )
                client.unhide()
            else:
                client.hide()
            return

        cidx = self.clients.index(client)
        self._configure_specific(client, screen_rect, px, cidx)
        client.unhide()

    def _configure_specific(self, client, screen_rect, px, cidx):
        """Specific configuration for xmonad tall."""
        self.screen_rect = screen_rect

        # calculate master/slave pane size
        width_master = int(self.screen_rect.width * self.ratio)
        width_slave = self.screen_rect.width - width_master

        if len(self.master_windows) == 0:
            width_master = 0
            width_slave = self.screen_rect.width
        if len(self.slave_windows) == 0:
            width_master = self.screen_rect.width
            width_slave = 0

        # calculate client's x offset
        if self.align == self._left:  # left or up orientation
            if client in self.master_windows:
                # master client
                xpos = self.screen_rect.x
            else:
                # slave client
                xpos = self.screen_rect.x + width_master
        else:  # right or down orientation
            if client in self.master_windows:
                # master client
                xpos = self.screen_rect.x + width_slave - self.margin
            else:
                # slave client
                xpos = self.screen_rect.x

        # calculate client height and place
        if client in self.slave_windows:
            pos = self.clients.index(client)
            # slave client
            width = width_slave - 2 * self.border_width
            # ypos is the sum of all clients above it
            height = self.screen_rect.height // len(self.slave_windows)
            ypos = self.screen_rect.y + self.clients[self.master_length :].index(client) * height
            # fix double margin
            if cidx > 1:
                ypos -= self.margin
                height += self.margin
            # place client based on calculated dimensions
            client.place(
                xpos,
                ypos,
                width,
                height - 2 * self.border_width,
                self.border_width,
                px,
                margin=self.margin,
            )
        else:
            pos = self.clients.index(client)
            if self.orientation == self._vert:
                height = self.screen_rect.height // self.master_length
                width = width_master
                ypos = self.screen_rect.y + pos * height
            else:
                height = self.screen_rect.height
                width = width_master // self.master_length
                overflow = width_master % self.master_length
                ypos = self.screen_rect.y
                if self.align == self._left:
                    xpos = self.screen_rect.x + pos * width + overflow
                else:
                    xpos = self.screen_rect.x + width_slave + pos * width

            # master client
            client.place(
                xpos,
                ypos,
                width,
                height,
                self.border_width,
                px,
                margin=[
                    self.margin,
                    2 * self.border_width,
                    self.margin + 2 * self.border_width,
                    self.margin,
                ],
            )

    def info(self):
        d = _SimpleLayoutBase.info(self)
        d.update(
            dict(
                master=[c.name for c in self.master_windows],
                slave=[c.name for c in self.slave_windows],
            )
        )
        return d

    def _grow_master(self, amt):
        """Will grow the client that is currently in the master pane"""
        self.ratio += amt
        self.ratio = min(self.max_ratio, self.ratio)

    def cmd_grow_master(self):
        """Grow master pane

        Will grow the master pane, reducing the size of clients in the slave
        pane.
        """
        self._grow_master(self.change_ratio)
        self.group.layout_all()

    def cmd_shrink_master(self):
        """Shrink master pane

        Will shrink the master pane, increasing the size of clients in the
        slave pane.
        """
        self._shrink_master(self.change_ratio)
        self.group.layout_all()

    def _shrink_master(self, amt):
        """Will shrink the client that currently in the master pane"""
        self.ratio -= amt
        self.ratio = max(self.min_ratio, self.ratio)

    cmd_next = _SimpleLayoutBase.next
    cmd_previous = _SimpleLayoutBase.previous

    cmd_up = cmd_previous
    cmd_down = cmd_next

    def cmd_shuffle_up(self):
        """Shuffle the client up the stack"""
        self.clients.shuffle_up()
        self.group.layout_all()
        self.group.focus(self.clients.current_client)

    def cmd_shuffle_down(self):
        """Shuffle the client down the stack"""
        self.clients.shuffle_down()
        self.group.layout_all()
        self.group.focus(self.clients[self.focused])

    def cmd_flip(self):
        """Flip the layout horizontally"""
        self.align = self._left if self.align == self._right else self._right
        self.group.layout_all()

    def cmd_swap(self, window1, window2):
        """Swap two windows"""
        self.clients.swap(window1, window2, 1)
        self.group.layout_all()
        self.group.focus(window1)

    def cmd_swap_master(self):
        """Swap current window to master pane"""
        win = self.clients.current_client
        cidx = self.clients.index(win)

        if cidx < self.master_length - 1:
            target = self.clients[cidx + 1]
        else:
            target = self.clients[0]

        self.cmd_swap(win, target)

    def cmd_decrease_nmaster(self):
        """Decrease number of windows in master pane"""
        self.master_length -= 1
        if self.master_length <= 0:
            self.master_length = 0
        self.group.layout_all()

    def cmd_increase_nmaster(self):
        """Increase number of windows in master pane"""
        self.master_length += 1
        if self.master_length >= len(self.clients):
            self.master_length = len(self.clients)
        self.group.layout_all()

    def cmd_master(self):
        """Focus windows in master pane"""
        win = self.clients.current_client
        cidx = self.clients.index(win)
        if cidx < self.master_length - 1:
            self.group.focus(self.clients[cidx + 1])
        else:
            self.group.focus(self.clients[0])

    def cmd_flip_master(self):
        """Flip the layout horizontally"""
        self.orientation = self._vert if self.orientation == self._hori else self._hori
        self.group.layout_all()


class MonadWide(MonadTall):
    """Emulate the behavior of XMonad's horizontal tiling scheme.

    This layout attempts to emulate the behavior of XMonad wide
    tiling scheme.

    Master-Pane:

    A master pane that contains a single window takes up a horizontal
    portion of the screen_rect based on the ratio setting. This ratio can be
    adjusted with the ``cmd_grow_master`` and ``cmd_shrink_master`` or.

    ::

        ---------------------
        |                   |
        |                   |
        |                   |
        |___________________|
        |                   |
        |                   |
        ---------------------

    Using the ``cmd_flip`` method will switch which vertical side the
    master pane will occupy. The master pane is considered the "top" of
    the stack.

    ::

        ---------------------
        |                   |
        |___________________|
        |                   |
        |                   |
        |                   |
        |                   |
        ---------------------

    Slave-panes:

    Occupying the rest of the screen_rect are one or more slave panes.
    The slave panes will share the horizontal space of the screen_rect.

    ::

        ---------------------
        |                   |
        |                   |
        |                   |
        |___________________|
        |     |       |     |
        |     |       |     |
        ---------------------

    Panes can be moved with the ``cmd_shuffle_up`` and ``cmd_shuffle_down``
    methods. As mentioned the master pane is considered the top of the
    stack; moving up is counter-clockwise and moving down is clockwise.

    The opposite is true if the layout is "flipped".

    ::

        ---------------------          ---------------------
        |                   |          |  2  |   3   |  4  |
        |         1         |          |_____|_______|_____|
        |                   |          |                   |
        |___________________|          |                   |
        |     |       |     |          |        1          |
        |  2  |   3   |  4  |          |                   |
        ---------------------          ---------------------

    Normalizing/Resetting:

    To restore master slave ratio use the ``cmd_normalize`` method.

    To reset the layout to its default state, including the master
    windows, master slave ratio and flip, use the ``cmd_reset`` method.

    Maximizing:

    To toggle a maximized layout, showing a single window, simply use the
    ``cmd_maximize`` on a focused client.

    Suggested Bindings::
        # these mirror the xmonad default config

        Key([modkey], "j", lazy.layout.down()),
        Key([modkey], "k", lazy.layout.up()),
        Key([modkey, "shift"], "j", lazy.layout.shuffle_down()),
        Key([modkey, "shift"], "k", lazy.layout.shuffle_up()),
        Key([modkey], "h", lazy.layout.shink_master()),
        Key([modkey], "l", lazy.layout.grow_master()),
        Key([modkey], "n", lazy.layout.normalize()),
        Key([modkey], "m", lazy.layout.master()),
        Key([modkey], "comma", lazy.layout.decrease_nmaster()),
        Key([modkey], "period", lazy.layout.increase_nmaster()),
        Key([modkey], "Return", lazy.layout.swap_master())

        # keybindings not in xmonad default config

        Key([modkey, "shift"], "n", lazy.layout.reset()),
        Key([modkey], "space", lazy.layout.flip()),
        Key([modkey, "shift"], "space", lazy.layout.flip_master()),
        Key([modkey, "shift"], "m", lazy.layout.maximize()),
    """

    _up = 0
    _down = 1
    _hori = 0
    _vert = 1

    def _configure_specific(self, client, screen_rect, px, cidx):
        """Specific configuration for xmonad wide."""
        self.screen_rect = screen_rect

        # calculate master/slave column widths
        height_master = int(self.screen_rect.height * self.ratio)
        height_slave = self.screen_rect.height - height_master

        if len(self.master_windows) == 0:
            height_master = 0
            height_slave = self.screen_rect.height
        if len(self.slave_windows) == 0:
            height_master = self.screen_rect.height
            height_slave = 0

        # calculate client's x offset
        if self.align == self._up:  # up orientation
            if client in self.master_windows:
                # master client
                ypos = self.screen_rect.y
            else:
                # slave client
                ypos = self.screen_rect.y + height_master
        else:  # right or down orientation
            if client in self.master_windows:
                # master client
                ypos = self.screen_rect.y + height_slave - self.margin
            else:
                # slave client
                ypos = self.screen_rect.y

        # calculate client height and place
        if client in self.slave_windows:
            # slave client
            height = height_slave - 2 * self.border_width
            # xpos is the sum of all clients left of it
            width = self.screen_rect.width // len(self.slave_windows)
            xpos = self.screen_rect.x + self.clients[self.master_length :].index(client) * width
            # get width from precalculated width list
            width = self.screen_rect.width // len(self.slave_windows)

            # fix double margin
            if cidx > 1:
                xpos -= self.margin
                width += self.margin
            # place client based on calculated dimensions
            client.place(
                xpos,
                ypos,
                width - 2 * self.border_width,
                height,
                self.border_width,
                px,
                margin=self.margin,
            )
        else:
            pos = self.clients.index(client)
            if self.orientation == self._hori:
                width = self.screen_rect.width // self.master_length
                height = height_master
                xpos = self.screen_rect.x + pos * width
            else:
                width = self.screen_rect.width
                height = height_master // self.master_length
                overflow = height_master % self.master_length
                xpos = self.screen_rect.x
                if self.align == self._up:
                    ypos = self.screen_rect.y + pos * height + overflow
                else:
                    ypos = self.screen_rect.y + height_slave + pos * height

            # master client
            client.place(
                xpos,
                ypos,
                width,
                height,
                self.border_width,
                px,
                margin=[
                    self.margin,
                    self.margin + 2 * self.border_width,
                    2 * self.border_width,
                    self.margin,
                ],
            )
