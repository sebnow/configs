# http://yabatadesign.github.io/afterglow-theme

evaluate-commands %sh{
	foreground="rgb:d6d6d6"
	background="rgb:1a1a1a"
	selection="rgb:5a647e"
	line="rgb:393939"
	comment="rgb:797979"
	red="rgb:ac4142"
	orange="rgb:e87d3e"
	yellow="rgb:e5b567"
	green="rgb:b4c973"
	blue="rgb:6c99bb"
	wine="rgb:b05279"
	purple="rgb:9e86c8"
	window="rgb:4d5057"

	almostBlack="rgb:2a2a2a"

	# code
	echo "
		face global value $purple
		face global type $wine
		face global identifier $orange
		face global string $yellow
		face global error $red
		face global keyword $orange
		face global operator $purple
		face global attribute $blue
		face global comment $comment
		face global meta $blue
	"

	# text
	echo "
		face global title $comment+b
		face global header rgb:ffffff
		face global bold rgb:ffffff+b
		face global italic rgb:ededed+i
		face global mono rgb:cccccc,rgb:212121
		face global block $comment,rgb:212121
		face global link $blue
		face global bullet rgb:ffffff
		face global list rgb:dedede
	"

	# kakoune UI
	echo "
		face global Default $foreground,default
		face global PrimarySelection $foreground,$selection
		face global SecondarySelection $foreground,rgb:474d5c
		face global PrimaryCursor $background,$foreground+b
		face global SecondaryCursor rgb:121212,rgb:dedede+b
		face global PrimaryCursorEol default,rgb:e4e4e4
		face global MatchingChar $selection
		face global Search $background,$yellow
		face global Whitespace default,rgb:333333
		face global BufferPadding rgb:333333
		face global LineNumbers $line
		face global LineNumberCursor rgb:666666
		face global MenuForeground $foreground,rgb:59637d
		face global MenuBackground default,$almostBlack
		face global MenuInfo default,rgb:545454
		face global Information default,rgb:454545
		face global Error $foregroung,$red
		face global StatusLine $foreground,$almostBlack
		face global StatusLineMode $background,rgb:dedede
		face global StatusLineInfo $foreground,$comment
		face global StatusLineValue $foreground,rgb:454545+b
		face global StatusCursor default,$comment
		face global Prompt $foreground,$background
	"
}
