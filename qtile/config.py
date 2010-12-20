from libqtile.manager import Key, Screen
from libqtile.command import lazy
from libqtile import layout, bar, widget

modKey = "mod4"

keys = [
	Key([modKey], "Return", lazy.spawn("/usr/bin/urxvtc")),
	Key([modKey], "Tab", lazy.nextlayout()),
	Key([modKey], "q", lazy.window.kill()),
]

groups = ["a", "s", "d", "f", "v"]
for group in groups:
	keys.append(Key([modKey], str(group), lazy.group[group].toscreen()))

layouts = [layout.Stack(stacks=2, borderWidth=1),]
screens = [Screen()]
