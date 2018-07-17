"=============================================================================
" Filename: autoload/lightline/colorscheme/afterglow.vim
" Author: sebnow
" License: MIT License
"
" Based on the airline theme by Danilo Augusto
" =============================================================================
let s:gui00 = "#1e1e1e" " ANSI Black
let s:gui01 = "#ac4142" " ANSI Red
let s:gui02 = "#b4c973" " ANSI Green
let s:gui03 = "#e5b567" " ANSI Yellow
let s:gui04 = "#6c99bb" " ANSI Blue
let s:gui05 = "#b05279" " ANSI Magenta
let s:gui06 = "#9e86c8" " ANSI Cyan
let s:gui07 = "#d6d6d6" " ANSI White
let s:gui08 = "#87875f"
let s:gui09 = "#af1600"
let s:gui0A = "#af875f"
let s:gui0B = "#878787"
let s:gui0C = "#af5f00"
let s:gui0D = "#5f5f87"
let s:gui0E = "#afd7d7"
let s:gui0F = "#dfdfaf"

let s:guiWhite = "#ffffff"
let s:guiGray = "#666666"
let s:guiDarkGray = "#545454"
let s:guiAlmostBlack = "#2a2a2a"

let s:p = {'normal': {}, 'inactive': {}, 'insert': {}, 'replace': {}, 'visual': {}, 'tabline': {}}
let s:p.normal.left = [ [ s:guiWhite, s:gui04 ], [ s:guiWhite, s:guiGray ] ]
let s:p.normal.right = [ [ s:guiWhite, s:guiGray ], [ s:gui07, s:guiAlmostBlack ] ]
let s:p.inactive.left = [ [ s:guiGray, s:guiAlmostBlack ], [ s:guiGray, s:guiAlmostBlack ] ]
let s:p.inactive.right = reverse(s:p.inactive.left)
let s:p.insert.left = [ [ s:guiAlmostBlack, s:gui02 ], s:p.normal.left[1] ]
let s:p.insert.right = copy(s:p.normal.right)
let s:p.replace.left = [ [ s:gui00, s:gui01 ], s:p.normal.left[1] ]
let s:p.visual.left = [ [ s:gui00, s:gui03 ], s:p.normal.left[1] ]
let s:p.normal.middle = [ [ s:gui07, s:guiAlmostBlack ] ]
let s:p.inactive.middle = [ [ s:guiGray, s:guiAlmostBlack ] ]
let s:p.tabline.left = [ [ s:guiGray, s:guiAlmostBlack ] ]
let s:p.tabline.tabsel = [ [ s:guiWhite, s:guiDarkGray ] ]
let s:p.tabline.middle = [ [ s:guiGray, s:guiAlmostBlack ] ]
let s:p.tabline.right = copy(s:p.normal.right)
let s:p.normal.error = [ [ s:gui00, s:gui01 ] ]
let s:p.normal.warning = [ [ s:gui00, s:gui03 ] ]

let g:lightline#colorscheme#afterglow#palette = lightline#colorscheme#fill(s:p)
