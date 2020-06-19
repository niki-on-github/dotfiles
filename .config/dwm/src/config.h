/* See LICENSE file for copyright and license details. */

/* appearance */
static const unsigned int borderpx  = 3;        /* border pixel of windows */
static const unsigned int gappx     = 6;        /* gaps between windows */
static const unsigned int snap      = 32;       /* snap pixel */
static const unsigned int systraypinning = 0;   /* 0: sloppy systray follows selected monitor, >0: pin systray to monitor X */
static const unsigned int systrayspacing = 6;   /* systray spacing */
static const int systraypinningfailfirst = 1;   /* 1: if pinning fails, display systray on the first monitor, False: display systray on the last monitor*/
static const int showsystray        = 1;        /* 0 means no systray */
static const int showbar            = 1;        /* 0 means no bar */
static const int topbar             = 1;        /* 0 means bottom bar */
static const char *fonts[]          = { "Noto Sans Mono:size=12" };

/* default colors (override from .Xresources) */
static char normbgcolor[]           = "#000000";
static char normbordercolor[]       = "#444444";
static char normfgcolor[]           = "#cccccc";
static char selfgcolor[]            = "#ffffff";
static char selbordercolor[]        = "#ffffff";
static char selbgcolor[]            = "#1793DF";
static char *colors[][3] = {
       /*               fg           bg           border   */
       [SchemeNorm] = { normfgcolor, normbgcolor, normbordercolor },
       [SchemeSel]  = { selfgcolor,  selbgcolor,  selbordercolor  },
};

/* tagging */
static const char *tags[] = { "1", "2", "3", "4", "5", "6", "7", "8", "9" };

static const Rule rules[] = {
	/* 
     * First-Match Rule List
     *
     * xprop:
	 *	WM_CLASS(STRING) = instance, class
	 *	WM_NAME(STRING)  = title
     * 
     * Setup swallow: add your terminal class marked with isterminal and noswallow
	*/
	/* class              instance           title          tags    center floating terminal nofocus noswallow monitor scratch-key*/
     { "pop-up",           NULL,              NULL,           0,         1,      1,      0,      0,      1,      -1,      0  },
     { "bubble",           NULL,              NULL,           0,         1,      1,      0,      0,      1,      -1,      0  },
     { "dialog",           NULL,              NULL,           0,         1,      1,      0,      0,      1,      -1,      0  },
     { "menu",             NULL,              NULL,           0,         1,      1,      0,      0,      1,      -1,      0  },
     { "center",           NULL,              NULL,           0,         1,      1,      0,      1,      0,      -1,      0  },
     { "no-focus",         NULL,              NULL,           0,         0,      0,      0,      1,      0,      -1,      0  },
     { "floating",         NULL,              NULL,           0,         0,      1,      0,      1,      0,      -1,      0  },
     { NULL,               "center",          NULL,           0,         1,      1,      0,      1,      0,      -1,      0  },
     { NULL,               "no-focus",        NULL,           0,         0,      0,      0,      1,      0,      -1,      0  },
     { NULL,               "floating",        NULL,           0,         0,      1,      0,      1,      0,      -1,      0  },
     { NULL,               "ncmpcpp",         NULL,           0,         1,      1,      1,      0,      1,      -1,     'm' },
     { NULL,               "newsboat",        NULL,           0,         1,      1,      1,      0,      1,      -1,     'n' },
     { NULL,               "pulsemixer",      NULL,           0,         1,      1,      1,      0,      1,      -1,     'a' }, 
	 { "Gimp",             NULL,              NULL,           0,         0,      1,      0,      0,      0,      -1,      0  },
	 { "firefox",          NULL,              NULL,           0,         0,      0,      0,      1,      0,      -1,      0  },
	 { "Alacritty",        NULL,              NULL,           0,         0,      0,      1,      0,      1,      -1,      0  },
     { "mpv",              NULL,              NULL,           0,         0,      1,      0,      0,      0,      -1,      0  },
     //{ "mpv",              NULL,              NULL,           0,         1,      1,      0,      0,      0,      -1,      0  },
};

/* layout(s) */
static const float mfact     = 0.667; /* factor of master area size [0.05..0.95] */
static const int nmaster     = 2;    /* number of clients in master area */
static const int resizehints = 1;    /* 1 means respect size hints in tiled resizals */

/* first entry is default */
static const Layout layouts[] = {
	/* symbol     arrange function */
	{ "[HL]",     column_master_left },
    { "[HR]",     column_master_right },
	{ "[VL]",     row_master_left },
    { "[VR]",     row_master_right },
    { "[3C]",     threecolumnlayout },
    { "[G]",      horizgrid },
	{ "[F]",      monocle },
	{ "[?]",      NULL },    /* no layout function = floating behavior */
    {  NULL,      NULL },  /* last entry */
};

/* key definitions */
#define MODKEY Mod1Mask
#define TAGKEYS(KEY,TAG) \
	{ MODKEY,                       KEY,      view,           {.ui = 1 << TAG} }, \
	{ MODKEY|ControlMask,           KEY,      toggleview,     {.ui = 1 << TAG} }, \
	{ MODKEY|ShiftMask,             KEY,      tag,            {.ui = 1 << TAG} }, \
	{ MODKEY|ControlMask|ShiftMask, KEY,      toggletag,      {.ui = 1 << TAG} },

/* helper for spawning shell commands */
#define SHCMD(cmd) { .v = (const char*[]){ "/bin/sh", "-c", cmd, NULL } }

/*First arg only serves to match against key in rules*/
static const char *scratchpad_ncmpcpp[] = {"m", "alacritty", "--class", "ncmpcpp", "-t", "ncmpcpp", "-d", "150", "50", "-e", "ncmpcpp", NULL};
static const char *scratchpad_newsboat[] = {"n", "alacritty", "--class", "newsboat", "-t", "newsboat", "-d", "150", "50", "-e", "newsboat", NULL};
static const char *scratchpad_pulsemixer[] = {"a", "alacritty", "--class", "pulsemixer", "-t", "pulsemixer", "-d", "150", "50", "-e", "pulsemixer", NULL};
//static const char *scratchpad_spotify[] = {"s", "/bin/sh", "-c", "~/.local/bin/spotify-adblock", NULL};  // does not work

/* commands */
static const char *terminal[]  = { "alacritty", NULL };

#include <X11/XF86keysym.h>
static Key keys[] = {
	/* modifier                     key        function        argument */
	{ MODKEY,                       XK_d,      spawn,          SHCMD("$HOME/.config/dmenu/scripts/dmenu_apps.sh") },
	{ MODKEY|ShiftMask,             XK_d,      spawn,          SHCMD("sudo -A $HOME/.config/dmenu/scripts/dmenu_apps.sh") },
	{ MODKEY,                       XK_Return, spawn,          {.v = terminal} },
    { MODKEY|ShiftMask,             XK_period, spawn,          SHCMD("$HOME/.config/dmenu/scripts/dmenu_edit.sh") },
    { MODKEY|ShiftMask,             XK_u,      spawn,          SHCMD("$HOME/.config/dmenu/scripts/dmenu_unicode.sh") },
    { MODKEY|ShiftMask,             XK_v,      spawn,          SHCMD("$HOME/.config/dmenu/scripts/dmenu_virtualbox.sh") },
    { MODKEY,                       XK_c,      spawn,          SHCMD("$HOME/.config/dmenu/scripts/dmenu_clipboard.sh") },
    { MODKEY|ShiftMask,             XK_x,      spawn,          SHCMD("$HOME/.config/dmenu/scripts/dmenu_shutdown.sh") },
    { MODKEY|ShiftMask,             XK_o,      spawn,          SHCMD("$HOME/.config/dmenu/scripts/dmenu_scripts.sh") },
    { MODKEY,                       XK_plus,   spawn,          SHCMD("$HOME/.local/bin/volume-control up 1") },
    { MODKEY,                       XK_minus,  spawn,          SHCMD("$HOME/.local/bin/volume-control down 1") },
    { MODKEY,                       XK_m,      spawn,          SHCMD("$HOME/.local/bin/volume-control toggle") },
    { MODKEY|ShiftMask,             XK_b,      spawn,          SHCMD("$HOME/.local/bin/x11-wallpaper choice") },
    { MODKEY,                       XK_p,      spawn,          SHCMD("$HOME/.local/bin/music-control toggle") },
    { MODKEY,                       XK_n,      spawn,          SHCMD("$HOME/.local/bin/music-control next") },
    { MODKEY|ShiftMask,             XK_p,      spawn,          SHCMD("$HOME/.local/bin/music-control prev") },
    { MODKEY,                       XK_x,      spawn,          SHCMD("i3lock --nofork -B=100") },
    { 0,			                XK_Print,  spawn,          SHCMD("$HOME/.local/bin/screenshot") },
	{ MODKEY,                       XK_b,      togglebar,      {0} },
	{ MODKEY,                       XK_j,      focusstack,     {.i = +1 } },
	{ MODKEY,                       XK_k,      focusstack,     {.i = -1 } },
	{ MODKEY|ShiftMask,             XK_plus,   incnmaster,     {.i = +1 } },
    { MODKEY|ShiftMask,             XK_minus,  incnmaster,     {.i = -1 } },
	{ MODKEY|ShiftMask,             XK_j,      movestack,      {.i = +1 } },
	{ MODKEY|ShiftMask,             XK_k,      movestack,      {.i = -1 } },
	{ MODKEY,                       XK_h,      setmfact,       {.f = -0.05} },
	{ MODKEY,                       XK_l,      setmfact,       {.f = +0.05} },
    { MODKEY|ShiftMask,             XK_Return, zoom,           {0} },  // use selectet window as master
	{ MODKEY,                       XK_Tab,    cyclelayout,    {.i = +1 } },  
    { MODKEY|ShiftMask,             XK_Tab,    cyclelayout,    {.i = -1 } },
	{ MODKEY|ShiftMask,             XK_c,      killclient,     {0} },
    { MODKEY,                       XK_q,      killclient,     {0} },
	{ MODKEY|ShiftMask,             XK_space,  togglefloating, {0} },
    { MODKEY,                       XK_s,      togglesticky,   {0} },
	{ MODKEY,                       XK_0,      view,           {.ui = ~0 } },
	{ MODKEY|ShiftMask,             XK_0,      tag,            {.ui = ~0 } },
	{ MODKEY,                       XK_comma,  tagmon,         {.i = -1 } },
	{ MODKEY,                       XK_period, tagmon,         {.i = +1 } },
    { MODKEY,			            XK_f,	   togglefullscr,  {0} },
    { MODKEY|ShiftMask,             XK_m,      togglescratch,  {.v = scratchpad_ncmpcpp } },
    { MODKEY|ShiftMask,             XK_n,      togglescratch,  {.v = scratchpad_newsboat } },
    { MODKEY|ShiftMask,             XK_a,      togglescratch,  {.v = scratchpad_pulsemixer } },
    { MODKEY,			            XK_g,	   togglegaps,     {0} },
    { MODKEY,                       XK_F1,     setlayout,      {.v = &layouts[0] } },
    { MODKEY,                       XK_F2,     setlayout,      {.v = &layouts[1] } },
    { MODKEY,                       XK_F3,     setlayout,      {.v = &layouts[2] } },
    { MODKEY,                       XK_F4,     setlayout,      {.v = &layouts[3] } },
    { MODKEY,                       XK_F5,     setlayout,      {.v = &layouts[4] } },
    { MODKEY,                       XK_F6,     setlayout,      {.v = &layouts[5] } },
    { MODKEY,                       XK_F7,     setlayout,      {.v = &layouts[6] } },

    /*
    { MODKEY,                       XK_minus,  setgaps,        {.i = -1 } },
    { MODKEY,                       XK_equal,  setgaps,        {.i = +1 } },
    { MODKEY|ShiftMask,             XK_equal,  setgaps,        {.i = 0  } },
    { MODKEY,                       XK_comma,  focusmon,       {.i = -1 } },
    { MODKEY,                       XK_period, focusmon,       {.i = +1 } },
    */

    /* media keys */
    { 0, XF86XK_AudioMute,		    spawn,		SHCMD("$HOME/.local/bin/volume-control toggle") },
	{ 0, XF86XK_AudioRaiseVolume,	spawn,		SHCMD("$HOME/.local/bin/volume-control up 1") },
	{ 0, XF86XK_AudioLowerVolume,	spawn,		SHCMD("$HOME/.local/bin/volume-control down 1") },
	{ 0, XF86XK_AudioPrev,		    spawn,		SHCMD("$HOME/.local/bin/music-control prev") },
	{ 0, XF86XK_AudioNext,		    spawn,		SHCMD("$HOME/.local/bin/music-control next") },
	{ 0, XF86XK_AudioPause,		    spawn,		SHCMD("$HOME/.local/bin/music-control toggle") },
	{ 0, XF86XK_AudioPlay,		    spawn,		SHCMD("$HOME/.local/bin/music-control toggle") },
	{ 0, XF86XK_AudioStop,		    spawn,		SHCMD("$HOME/.local/bin/music-control stop") },
	{ 0, XF86XK_AudioRewind,	    spawn,		SHCMD("mpc seek -10") },
	{ 0, XF86XK_AudioForward,	    spawn,		SHCMD("mpc seek +10") },

    /* Mod + Ctrl + Num activate tag */
	TAGKEYS(                        XK_1,                      0)
	TAGKEYS(                        XK_2,                      1)
	TAGKEYS(                        XK_3,                      2)
	TAGKEYS(                        XK_4,                      3)
	TAGKEYS(                        XK_5,                      4)
	TAGKEYS(                        XK_6,                      5)
	TAGKEYS(                        XK_7,                      6)
	TAGKEYS(                        XK_8,                      7)
	TAGKEYS(                        XK_9,                      8)
//	{ MODKEY|ShiftMask,             XK_e,      quit,           {0} },  /* quit dwm */
    { MODKEY|ShiftMask,             XK_r,      quit,           {1} },  /* relaod dwm */
};

/* button definitions */
/* click can be ClkTagBar, ClkLtSymbol, ClkStatusText, ClkWinTitle, ClkClientWin, or ClkRootWin */
static Button buttons[] = {
	/* click                event mask      button          function        argument */
	{ ClkLtSymbol,          0,              Button1,        cyclelayout,    {.i = +1 } },
	{ ClkLtSymbol,          0,              Button3,        cyclelayout,    {.i = -1 } },
	{ ClkWinTitle,          0,              Button2,        zoom,           {0} },
	{ ClkStatusText,        0,              Button2,        spawn,          {.v = terminal } },
	{ ClkClientWin,         MODKEY,         Button1,        movemouse,      {0} },
	{ ClkClientWin,         MODKEY,         Button2,        togglefloating, {0} },
	{ ClkClientWin,         MODKEY,         Button3,        resizemouse,    {0} },
	{ ClkTagBar,            0,              Button1,        view,           {0} },
	{ ClkTagBar,            0,              Button3,        toggleview,     {0} },
	{ ClkTagBar,            MODKEY,         Button1,        tag,            {0} },
	{ ClkTagBar,            MODKEY,         Button3,        toggletag,      {0} },
};

