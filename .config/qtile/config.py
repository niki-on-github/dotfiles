# Qtile Config File
# NOTE: This config require my patched Qtile version
# sorry for the caotic code. i haven't had time to clean it up yet.
from typing import List
from libqtile.backend.base import Internal

import psutil
import os
from libqtile.config import Key
from libqtile.command import lazy
from libqtile import hook
from libqtile import qtile
from libqtile import hook
from libqtile.log_utils import logger
from libqtile.config import Screen
from libqtile import bar
from libqtile.log_utils import logger
from libqtile.config import Match
from libqtile.config import Key, Group, ScratchPad, DropDown
from libqtile.command import lazy
from libqtile.config import Drag, Click
from libqtile.command import lazy
from libqtile.log_utils import logger
from libqtile import qtile
from libqtile import widget
from libqtile import layout
from libqtile.config import Match
from libqtile import bar
from libqtile.log_utils import logger
from screeninfo import get_monitors

from layout.xmonad import MonadTall

ACCENT_COLOR = '#1d99f3'
groups = []

wttr_widget = widget.Wttr(
        location={'Waiblingen': 'WTR'},
        update_interval= 30*60,
        format="WTR %t"
        )

class WidescrenGapsController:

    def __init__(self, widescreen_gaps: int, workspaces: int = 9):
        self.widescreen_gaps = widescreen_gaps
        self.screens = []
        for monitor in get_monitors():
            is_widescreen = bool(monitor.width > 2 * monitor.height)
            self.screens.append({
                'width': monitor.width,
                'height': monitor.height,
                'is_widescreen': is_widescreen,
                'gaps': [self.widescreen_gaps if is_widescreen else 0 for _ in range(workspaces)]
            })

    def get_sceen_width(self):
        screen = qtile.current_screen.index
        return self.screens[screen]['width']

    def get_sceen_height(self):
        screen = qtile.current_screen.index
        return self.screens[screen]['height']

    def toogle_gaps(self):
        screen = qtile.current_screen.index
        workspace = int(''.join(qtile.current_group.name[1:]))
        if screen >= 0 and screen <= len(self.screens):
            if workspace > 0 and workspace <= len(self.screens[screen]['gaps']):
                if self.screens[screen]['is_widescreen']:
                    self.screens[screen]['gaps'][workspace - 1] = 0 if self.screens[screen]['gaps'][workspace - 1] != 0 else self.widescreen_gaps
                else:
                    self.screens[screen]['gaps'][workspace - 1] = 0

                # logger.warning('toogle %d, %d: %d', screen, workspace, self.screens[screen]['gaps'][workspace - 1])

        self.apply_gaps()


    def get_gaps(self, screen: int, workspace: int) -> int:
        if screen >= 0 and screen <= len(self.screens):
            if workspace > 0 and workspace <= len(self.screens[screen]['gaps']):
                return self.screens[screen]['gaps'][workspace - 1]

        return 0


    def apply_gaps(self, offset: int = 0):
        windows_count = 0
        for window in qtile.current_layout.get_windows():
            if not window.floating:
                windows_count += 1

        windows_count += offset

        widescreen_gaps = self.get_gaps(qtile.current_screen.index, int(''.join(qtile.current_group.name[1:])))
        if windows_count > 1:
            qtile.current_layout.margin = [12, 12, 12, 12]
        else:
            qtile.current_layout.margin = [0, widescreen_gaps, 0, widescreen_gaps]


        # logger.warning("set gaps: %d", widescreen_gaps)
        # logger.warning("set gaps: %s", str(dir(qtile.current_group)))
        # qtile.current_screen.right.size = 0 if windows_count > 1 else widescreen_gaps
        # qtile.current_screen.left.size = 0 if windows_count > 1 else widescreen_gaps

        qtile.current_group.layout_all()

    def debug(self):
        logger.warning("WidescrenGapsController Debug: %s", str(self.screens))


gaps_controller = WidescrenGapsController(680)


class FocusTracker:

    def __init__(self):
        self.current_focus = None
        self.prev_focus = None
        hook.subscribe.client_focus(self.on_focus)

    def on_focus(self, window):
        if self.current_focus == window:
            return
        prob = window.window.get_property("_NO_FOCUS", "STRING", unpack=str)
        # logger.warning("check %s", str(prob))
        if prob and prob == "1":
            window.window.set_property("_NO_FOCUS", "0", type="STRING", format=8)
            self.focus_prev()
        else:
            # logger.warning("focus change save")
            self.prev_focus = self.current_focus
            self.current_focus = window

    def focus_prev(self):
        prev = self.current_focus
        if prev:
            # logger.warning("focus last")
            prev.cmd_focus()
            # prev.cmd_bring_to_front()

focus_tracker = FocusTracker()



@hook.subscribe.client_new
def _swallow(window):
    pid = window.window.get_net_wm_pid()
    if pid is None:
        return

    rules = [
        Match(wm_class="pop-up"),
        Match(wm_class="dialog")
    ]

    if any(window.match(rule) for rule in rules):
        return

    process = psutil.Process(pid)
    cpids = {c.window.get_net_wm_pid(): wid for wid, c in window.qtile.windows_map.items()}
    for _ in range(5):
        if not process.ppid():
            return

        if process.ppid() in cpids:
            if psutil.Process(process.ppid()).name().lower() == "alacritty".lower():
                parent = window.qtile.windows_map.get(cpids[process.ppid()])
                parent.minimized = True
                window.parent = parent
                return

        process = psutil.Process(process.ppid())
        if process.ppid() in [0, 1] or process.pid in [0, 1]:
            return


@hook.subscribe.client_killed
def _unswallow(window):
    if hasattr(window, 'parent'):
        window.parent.minimized = False


@hook.subscribe.client_new
def test_22(window):
    infos = window.get_wm_class()
    no_focus_keywords = ["no-focus"]
    rules = [
            Match(wm_class="no-focus"),
            Match(title="no-focus"),
        ]
    if any(window.match(rule) for rule in rules) or any(item == infos[0] for item in no_focus_keywords) or any(item == infos[1] for item in no_focus_keywords):
        window.window.set_property("_NO_FOCUS", "1", type="STRING", format=8)


@hook.subscribe.client_managed
def handle_mpv(window):
    rules = [
        Match(wm_class="mpv")
    ]

    if any(window.match(rule) for rule in rules) and not window.fullscreen:
        window.floating = True

@hook.subscribe.client_managed
def floating_handler(window):
    rules = [
        Match(wm_class="pop-up"),
        Match(wm_class="dialog")
    ]

    if any(window.match(rule) for rule in rules) and not window.fullscreen:
        window.floating = True

@hook.subscribe.client_focus
def test_abc(window):
    infos = window.get_wm_class()
    # logger.warning("client_focus %s", str(infos))
    rules = [
        Match(wm_class="mpv")
    ]

    if any(window.match(rule) for rule in rules) and not window.fullscreen:
        # logger.warning("stuf: %s", str(dir(window)))
        # logger.warning("stuf: %s", str(window.cmd_hints()))
        if not window.floating:
            window.floating = True
            width = window.width
            height = window.height
            x = 5120 - width - 12 - (2*3)
            y = 2160 - height - 12 - (2*3)
            window.place(x, y, width, height, 3, ACCENT_COLOR)
            window.bring_to_front()
            window.cmd_focus()
            # window.border_width = 0
            # window.place(x, y, width, height, 0, ACCENT_COLOR)

        for position in ["top", "bottom", "left", "right"]:
            bar = getattr(qtile.current_screen, position)
            if bar:
                bar.show(True)
                # qtile.current_group.layout_all()



@hook.subscribe.group_window_add
@hook.subscribe.client_new
@hook.subscribe.client_focus
def handle_focus(*args):
    for window in qtile.current_group.windows:
        if not isinstance(window, Internal):
            if window.floating:
                window.cmd_bring_to_front()


@hook.subscribe.client_new
@hook.subscribe.client_managed
def handle_round_corner(window):
    if isinstance(window, Internal):
        return
    if window.floating:
        window.window.set_property("_XMONAD_TAGS", "round-corner", type="STRING", format=8)


@hook.subscribe.client_killed
def handle_gaps_client_killed(window):
    if isinstance(window, Internal):
        gaps_controller.apply_gaps(0)
    else:
        gaps_controller.apply_gaps(0 if window.floating else -1)

@hook.subscribe.client_managed
@hook.subscribe.startup_complete
@hook.subscribe.layout_change
@hook.subscribe.float_change
def handle_gaps_changed(*args):
    gaps_controller.apply_gaps()

@hook.subscribe.client_killed
def fix_fullescreen(window):
    if isinstance(window, Internal):
        return
    if window.fullscreen:
        for position in ["top", "bottom", "left", "right"]:
            bar = getattr(qtile.current_screen, position)
            if bar:
                bar.show(True)
                qtile.current_group.layout_all()

@hook.subscribe.startup_complete
def network_fix(*args):
    wttr_widget.timeout_add(120, wttr_widget.timer_setup)

home = os.path.expanduser('~')
terminal = 'alacritty'
mod = "mod1"

# # window to next group
# @lazy.window.function
# def window_to_next_group(window):
#     index = window.qtile.groups.index(window.group)
#     index = (index + 1) % len(window.qtile.groups)
#     window.cmd_togroup(window.qtile.groups[index].name)

@lazy.function
def toggle_gaps(qtile):
    gaps_controller.toogle_gaps()


def independent_sceen_go_to_group(name: str):
    def _inner(qtile) -> None:
        prefix = str(qtile.current_screen.index)
        qtile.groups_map[prefix+name].cmd_toscreen()

    return _inner

def independent_sceen_move_to_group(name: str):
    def _inner(qtile) -> None:
        prefix = str(qtile.current_screen.index)
        qtile.current_window.togroup(prefix+name)

    return _inner

keys = [

    # Key([mod], "g",
    #     lazy.screen.next_group(skip_empty=True),
    #     desc="Move to next active group"
    #     ),
    # Key([mod, "shift"], "g",
    #     lazy.screen.prev_group(skip_empty=True),
    #     desc="Move to previous active group"
    #     ),
    # Switch between windows in current stack pane
    Key([mod], "k",
        # lazy.layout.down(),
        lazy.group.prev_window(),
        desc="Move focus down in stack pane"
        ),
    Key([mod], "j",
        # lazy.layout.up(),
        lazy.group.next_window(),
        desc="Move focus up in stack pane"
        ),


    Key([mod, "shift"], "k",
        lazy.layout.shuffle_down(),
        desc='Shuffle down'
        ),
    Key([mod, "shift"], "j",
        lazy.layout.shuffle_up(),
        desc='Shuffle up'
        ),
    Key([mod, "shift"], "h",
        lazy.layout.shuffle_left(),
        desc='Shuffle left'
        ),
    Key([mod, "shift"], "l",
        lazy.layout.shuffle_right(),
        desc='Shuffle right'
        ),


    # testing
    Key([mod], "g",
        toggle_gaps,
        desc='testing'
        ),

    # Key([mod, "control"], "s", lazy.function(make_sticky)),


    Key([mod, "control"], "j",
        lazy.layout.grow_down(),
        desc='Grow down'
        ),
    Key([mod, "control"], "k",
        lazy.layout.grow_up(),
        desc='Grow up'
        ),
    Key([mod, "control"], "h",
        lazy.layout.grow_left(),
        desc='Grow left'
        ),
    Key([mod, "control"], "l",
        lazy.layout.grow_right(),
        desc='Grow right'
        ),
    Key([mod], "h",
        lazy.layout.grow(),
        lazy.layout.increase_nmaster(),
        desc='Expand window (MonadTall), increase number in master pane (Tile)'
        ),
    Key([mod], "l",
        lazy.layout.shrink(),
        lazy.layout.decrease_nmaster(),
        desc='Shrink window (MonadTall), decrease number in master pane (Tile)'
        ),

    # Toggle floating
    Key([mod, "shift"], "f", lazy.window.toggle_floating(),
        desc="Toggle floating"
        ),

    # Toggle Fullscreen
    Key([mod], "f",
        lazy.window.toggle_fullscreen(),
        lazy.hide_show_bar(position='all'),
        desc='Toggle fullscreen and the bars'
        ),

    # Switch window focus to other pane(s) of stack
    Key([mod], "space", lazy.layout.next(),
        desc="Switch window focus to other pane(s) of stack"
        ),

    # Swap panes of split stack
    Key([mod, "shift"], "space",
        lazy.layout.rotate(),
        desc="Swap panes of split stack"
        ),

    # Toggle between split and unsplit sides of stack.
    # Split = all windows displayed
    # Unsplit = 1 window displayed, like Max layout, but still with
    # multiple stack panes
    Key([mod, "shift"], "Return",
        lazy.layout.toggle_split(),
        desc="Toggle between split and unsplit sides of stack"
        ),
    Key([mod], "Return",
        lazy.spawn(terminal),
        desc="Launch terminal"
        ),

    # Toggle between different layouts as defined below
    Key([mod], "Tab",
        lazy.next_layout(),
        desc="Toggle between layouts"
        ),
    Key([mod], "q",
        lazy.window.kill(),
        desc="Kill focused window"
        ),

    # Toggle bars
    Key([mod], "b",
        lazy.hide_show_bar(position='all'),
        desc="Toggle bars"
        ),

    # Qtile system keys
    Key([mod, "control"], "r",
        lazy.reload_config(),
        desc="Restart qtile"
        ),
    Key([mod, "control"], "e",
        lazy.shutdown(),
        desc="Shutdown qtile"
        ),


    # Window Switcher
    # Key([mod, "control"], "w",
    #     lazy.spawn(home + "/.local/bin/qtile-window-switcher.py"),
    #     desc="Launch the Window Switcher",
    #     ),

    # Scripts
    Key([mod], "d",
        lazy.spawn(home + "/.config/dmenu/scripts/dmenu_apps.sh"),
        desc="App Launcher",
        ),

    Key([mod, "shift"], "d",
        lazy.spawn("sudo -A " + home + "/.config/dmenu/scripts/dmenu_apps.sh"),
        desc="App Launcher woth sudo",
        ),

    Key([mod, "shift"], "period",
        lazy.spawn(home + "/.config/dmenu/scripts/dmenu_edit.sh"),
        desc="dotfiles editor launcher",
        ),

    # Cycle through windows in the floating layout
    # Key([mod, "shift"], "i",
    #     lazy.window.toggle_minimize(),
    #     lazy.group.next_window(),
    #     lazy.window.bring_to_front()
    #     ),

    # ------------ Hardware Configs ------------
    # Volume
    Key([], "XF86AudioMute",
        lazy.spawn(home + "/.local/bin/volume-control toggle"),
        desc='Toggle Mute audio'
        ),
    Key([], "XF86AudioLowerVolume",
        lazy.spawn(home + "/.local/bin/volume-control down 1"),
        desc='Volume down'
        ),
    Key([], "XF86AudioRaiseVolume",
        lazy.spawn(home + "/.local/bin/volume-control up 1"),
        desc='Volume up'
        ),

    # Media keys
    Key([], "XF86AudioPlay",
        lazy.spawn(home + "/.local/bin/music-control toggle"),
        desc='Audio play'
        ),
    Key([], "XF86AudioNext",
        lazy.spawn(home + "/.local/bin/music-control next"),
        desc='Audio next'
        ),
    Key([], "XF86AudioPrev",
        lazy.spawn(home + "/.local/bin/music-control prev"),
        desc='Audio previous'
        ),

    # Audio control
    Key([mod], "m",
        lazy.spawn(home + "/.local/bin/volume-control toggle"),
        desc='Toggle Mute audio'
        ),
    Key([mod], "minus",
        lazy.spawn(home + "/.local/bin/volume-control down 1"),
        desc='Volume down'
        ),
    Key([mod], "plus",
        lazy.spawn(home + "/.local/bin/volume-control up 1"),
        desc='Volume up'
        ),
    Key([mod], "p",
        lazy.spawn(home + "/.local/bin/music-control toggle"),
        desc='Audio play/pause'
        ),
    Key([mod], "n",
        lazy.spawn(home + "/.local/bin/music-control next"),
        desc='Audio next'
        ),
    Key([mod, "shift"], "p",
        lazy.spawn(home + "/.local/bin/music-control prev"),
        desc='Audio previous'
        ),
    Key([mod, "shift"], "v",
        lazy.spawn(home + "/.config/dmenu/scripts/dmenu_kvm.sh"),
        desc='Script'
        ),

    Key([mod, "shift"], "e",
        lazy.spawn(home + "/.config/dmenu/scripts/dmenu_shutdown.sh"),
        desc='Script'
        ),

    Key([mod], "c",
        lazy.spawn(home + "/.config/dmenu/scripts/dmenu_clipboard.sh"),
        desc='Script'
        ),
    Key([mod], "s",
        lazy.spawn(home + "/.config/dmenu/scripts/dmenu_websearch.sh"),
        desc='Script'
        ),

    Key([mod], "w",
        lazy.spawn(home + "/.config/dmenu/scripts/dmenu_firefox.sh"),
        desc='Script'
        ),

    # Screenshots
    Key([], "Print",
        lazy.spawn(home + ".local/bin/screenshot"),
        desc='Screenshots'
        ),
    Key(["shift"], "Print",
        lazy.spawn("flameshot gui"),
        desc='flameshot'
        ),
]

for i in range(1, 5):
    keys.append(Key(["control", "mod1"], "F"+str(i),
                    lazy.core.change_vt(i),
                    desc='Change to virtual console '+str(i)
                    ),)




# DEFAULT THEME SETTINGS FOR LAYOUTS #
layout_theme = {"border_width": 3,
                "margin": 12,
                "border_focus": ACCENT_COLOR,
                "border_normal": 'bbbbbb',
                # "margin_on_single": False,
                "new_client_position": 'before_current'
                }
layout_border = dict(
    border_focus = ACCENT_COLOR,
    border_normal = 'bbbbbb',
)

layouts = [
    # layout.Stack(num_stacks=3, **layout_theme),)
    # layout.Max(),
    # layout.Bsp(**layout_theme),
    layout.Columns(**layout_theme, num_columns=3),
    # MonadTall(**layout_theme, single_border_width=0, singe_margin=0),
    # layout.Matrix(),
    # layout.MonadWide(),
    # layout.RatioTile(),
    # layout.Tile(**layout_theme, master_length=2),
    # layout.TreeTab(),
    # layout.VerticalTile(**layout_theme),
    # layout.Zoomy(),
    # layout.Stack(num_stacks=3, **layout_theme),)
    # layout.Max(),
    # layout.Bsp(**layout_theme),
    # layout.Columns(**layout_theme, num_columns=3),
    # layout.MonadTall(**layout_theme, single_border_width=0, singe_margin=-12),
    # layout.Matrix(),
    # layout.MonadWide(),
    # layout.RatioTile(),
    # layout.Tile(**layout_theme, master_length=2),
    # layout.TreeTab(),
    layout.VerticalTile(**layout_theme),
    # layout.Zoomy(),
]

floating_layout = layout.Floating(**layout_theme, float_rules=[
    *layout.Floating.default_float_rules,
])



# groups = []
# for workspace in workspaces:
#     matches = workspace["matches"] if "matches" in workspace else None
#     layouts = workspace["layout"] if "layout" in workspace else None
#     groups.append(Group(workspace["name"], matches=matches, layout=layouts))
#     keys.append(Key([mod], workspace["key"], lazy.group[workspace["name"]].toscreen()))
#     keys.append(Key([mod, "shift"], workspace["key"], lazy.window.togroup(workspace["name"])))

groups = []
for tag in [1,2,3,4,5,6,7,8,9]:
    keys.append(Key([mod], str(tag), lazy.function(independent_sceen_go_to_group(str(tag)))))
    keys.append( Key( [mod, "shift"], str(tag), lazy.function( independent_sceen_move_to_group( str(tag)))))
    for screen in [0,1]:
        groups.append(Group(str(screen)+str(tag), label=" "+str(tag)+" ", layouts=[layouts[screen]]))


groups.append(ScratchPad("scratchpad", [
    DropDown("todo-list", "alacritty  --class todo-list -t todo-list -e sh -c 'sleep 0.1; nvim ~/.local/share/TODO-list.md;'", x=0.25, y=0.1, width=0.5, height=0.7, on_focus_lost_hide=False),
    DropDown("ncmpcpp", "alacritty  --class ncmpcpp -t ncmpcpp -e sh -c 'sleep 0.1; ncmpcpp;'", x=0.25, y=0.1, width=0.5, height=0.7, on_focus_lost_hide=False),
    DropDown("newsboat", "alacritty  --class newsboat -t newsboat -e sh -c 'sleep 0.1; newsboat;'", x=0.25, y=0.1, width=0.5, height=0.7, on_focus_lost_hide=False),
    DropDown("pulsemixer", "alacritty  --class pulsemixer -t pulsemixer -e sh -c 'sleep 0.1; pulsemixer;'", x=0.25, y=0.1, width=0.5, height=0.7, on_focus_lost_hide=False),
    DropDown("htop", "alacritty  --class htop -t htpop -e sh -c 'sleep 0.1; htop;'", x=0.25, y=0.1, width=0.5, height=0.7, on_focus_lost_hide=False)
    ])
)

keys.append(Key([mod, "shift"], "n", lazy.group["scratchpad"].dropdown_toggle("newsboat")))
keys.append(Key([mod, "shift"], "i", lazy.group["scratchpad"].dropdown_toggle("htop")))
keys.append(Key([mod, "shift"], "a", lazy.group["scratchpad"].dropdown_toggle("pulsemixer")))
keys.append(Key([mod, "shift"], "m", lazy.group["scratchpad"].dropdown_toggle("ncmpcpp")))



mouse = [
    Drag(
        [mod],
        "Button1",
        lazy.window.set_position_floating().when(when_floating=True),
        start=lazy.window.get_position()
    ),
    Drag(
        [mod],
        "Button3",
        lazy.window.set_size_floating(),
        start=lazy.window.get_size()
    ),
    Click(
        [mod],
        "Button2",
        lazy.window.bring_to_front()
    )
]


widget_defaults = dict(
    font='Hack Nerd Font Regular',
    fontsize='22',
    padding=5,
)
extension_defaults = widget_defaults.copy()

primary_widgets = [
    widget.Spacer(-7),
    widget.GroupBox(
        borderwidth=0,
        inactive='ffffff',
        this_current_screen_border=ACCENT_COLOR,
        this_screen_border=ACCENT_COLOR,
        font='Hack Nerd Font',
        fontsize=23,
        hide_unused=True,
        highlight_method='block',
        highlight_color=[ACCENT_COLOR, ACCENT_COLOR],
        visible_groups=["0"+str(i) for i in range(1,10)]
    ),
    widget.Spacer(10),
    widget.CurrentLayoutIcon(scale=0.8),
    widget.Spacer(10),
    widget.WindowName(
        foreground = 'ffffff',
        background = '000000',
        padding = 5
    ),

    widget.Spacer(),


    # Volume(
    widget.Volume(
        foreground = 'ffffff',
        background = '000000',
        fmt = 'VOL {}',
        padding = 5,
        get_volume_command = "pamixer --get-volume-human",
        check_mute_command = "pamixer --get-mute",
        check_mute_string = "true",
        volume_up_command = "pamixer -i 1",
        volume_down_command = "pamixer -d 1",
        mute_command = "pamixer -t",
       ),

    widget.TextBox(" - "),
        widget.CPU(
        format= "CPU {load_percent:.1f}%"
        ),
    widget.TextBox(" - "),
    widget.Memory(
        format ='MEM {MemPercent:.1f}%'
        ),
    widget.TextBox(" - "),
    # widget.ThermalSensor(
    #            foreground = 'ffffff',
    #            background = '000000',
    #            threshold = 70,
    #            fmt = 'TMP {}',
    #            padding = 5
    #            ),
    # widget.TextBox(" - "),
    wttr_widget,
    # widget.Wttr(
    #     location={'Waiblingen': 'WTR'},
    #     update_interval= 30*60,
    #     format="WTR %t"
    #     ),
    widget.TextBox(" - "),
    widget.Net(
       # interface = "enp5s0",
       format = 'NET {down} {up}',
       foreground = 'ffffff',
       background = '000000',
       padding = 5
       ),
    widget.TextBox(" - "),

    widget.Clock(
           foreground = 'ffffff',
           background = '000000',
           format = "%a, %d. %b -  %H:%M:%S "
           ),
    widget.Systray(
       foreground = 'ffffff',
       background = '000000',
       padding = 0,
       icon_size = 25
   ),
]

secondary_widgets = [
    widget.Spacer(-7),
    widget.GroupBox(
        borderwidth=0,
        inactive='ffffff',
        this_current_screen_border=ACCENT_COLOR,
        this_screen_border=ACCENT_COLOR,
        font='Hack Nerd Font',
        fontsize=23,
        hide_unused=True,
        highlight_method='block',
        highlight_color=[ACCENT_COLOR, ACCENT_COLOR],
        visible_groups=["1"+str(i) for i in range(1,10)]
    ),
    widget.Spacer(10),
    widget.CurrentLayoutIcon(scale=0.8),
    widget.Spacer(10),
    widget.WindowName(
        foreground = 'ffffff',
        background = '000000',
        padding = 5
    ),

    widget.Spacer(),
    widget.Clock(
           foreground = 'ffffff',
           background = '000000',
           format = "%a, %d. %b -  %H:%M:%S "
           ),
]

def status_bar(widgets):
    return bar.Bar(widgets, 30, background="#000000FF", margin=[-3, 0, 0, 0], border_color= ['000000', '000000', ACCENT_COLOR, '000000'], border_width=4)

screens = [
        Screen(top=status_bar(primary_widgets)),
        Screen(top=status_bar(secondary_widgets))
    ]

dgroups_key_binder = None
dgroups_app_rules = []
follow_mouse_focus = True
bring_front_click = True
cursor_warp = False
auto_fullscreen = True
focus_on_window_activation = "smart"
reconfigure_screens = True
auto_minimize = False
wl_input_rules = None
wmname = "Qtile"
