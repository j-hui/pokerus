#### Documentation ####
#
#   qute://help/configuring.html
#   qute://help/settings.html
#
#######################

#### Configuration ####

### System/environment boilerplate {{{

import os
import platform

import dracula.draw

# Load existing settings made via :set
config.load_autoconfig()

dracula.draw.blood(c, {
    'spacing': {
        'vertical': 6,
        'horizontal': 8
    }
})

# Put kitty, gvim, etc. in Qutebrowser's PATH, at least on macOS
os.environ['PATH'] = '/usr/local/bin' + os.pathsep + os.environ['PATH']

### System/environment boilerplate }}}

### Custom Userscripts {{{

USERSCRIPT_PATH = os.path.join(config.configdir, 'userscripts')

def userscript(script_name):
    return os.path.join(USERSCRIPT_PATH, script_name)

###### code_select ######
# https://github.com/LaurenceWarne/qute-code-hint

c.hints.selectors["code"] = [
    # Selects all code tags whose direct parent is not a pre tag
    ":not(pre) > code",
    "pre"
]

### Custom Userscripts }}}

### Assorted configs {{{

# Time interval (in milliseconds) between auto-saves of config/cookies/etc.
c.auto_save.interval = 15000

# Always restore open sites when qutebrowser is reopened. Type: Bool
c.auto_save.session = True

c.editor.command = ["kitty", "nvim", "-f", "{file}", "-c", "normal {line}G{column0}l"]

c.content.pdfjs = False
c.content.autoplay = False
c.tabs.background = True
c.tabs.close_mouse_button = 'right'

c.colors.webpage.prefers_color_scheme_dark = True

c.downloads.remove_finished = 696969

c.tabs.last_close = 'close'
c.tabs.width = '10%'
c.tabs.mousewheel_switching = False

c.zoom.mouse_divider = 0


# Open external applications
for site in ['zoommtg://*.zoom.us'
            ,'https://*.slack.com'
            ]:
    config.set('content.unknown_url_scheme_policy', 'allow-all', site)

# Acess clipboard
for site in ['https://github.com/*'
            ,'https://stackoverflow.com/*'
            ,'https://*.stackexchange.com/*'
            ]:
    config.set('content.javascript.can_access_clipboard', True, site)

### Assorted configs }}}

#### Aliases {{{

c.aliases['h'] = 'help'
c.aliases['py'] = 'debug-pyeval'
c.aliases['noh'] = 'search'
c.aliases['clear'] = 'clear-messages'
c.aliases['cl'] = 'clear-messages'

c.aliases['q'] = 'close'
c.aliases['qa'] = 'quit'

c.aliases['w'] = 'session-save'
c.aliases['wq'] = 'quit --save'
c.aliases['wqa'] = 'quit --save'

c.aliases['o'] = 'open'
c.aliases['O'] = 'open --tab'
c.aliases['t'] = 'open --background'

c.aliases['pt'] = 'mode-enter passthrough'
c.aliases['passthrough'] = 'mode-enter passthrough'

c.aliases['pin'] = 'tab-pin'

c.aliases['b'] = 'tab-focus'
for i in range(1, 20):
    c.aliases['b'+str(i)] = 'tab-focus ' + str(i)

c.aliases['priv'] = 'open --private'
c.aliases['bookmarks'] = 'open -t qute://bookmarks/'

c.aliases['mpv'] = 'spawn mpv --autofit=100%x100% --force-window=immediate --keep-open=yes {url}'
c.aliases['ompv'] = 'hint links spawn mpv --autofit=100%x100% --force-window=immediate {hint-url}'

c.aliases['dl'] = 'spawn --userscript open_download'

c.aliases['chrome'] = 'spawn open -a "Google Chrome" {url}'
c.aliases['ochrome'] = 'hint all spawn open -a "Google Chrome" {hint-url}'

c.aliases['user'] = 'spawn --userscript'
c.aliases['pass'] = 'spawn --userscript qute-pass'
c.aliases['readability'] = 'spawn --userscript readability'
c.aliases['reader'] = 'spawn --userscript readability'
c.aliases['bib'] = 'spawn --userscript getbib'
c.aliases['pocket'] = 'spawn --userscript qutepocket'

### Aliases }}}

### Bindings/mappings {{{

# Convenience helpers
def nmap(key, cmd):
    config.bind(key, cmd, mode='normal')
def vmap(key, cmd):
    config.bind(key, cmd, mode='caret')
def imap(key, cmd):
    config.bind(key, cmd, mode='insert')
def cmap(key, cmd):
    config.bind(key, cmd, mode='command')

c.bindings.key_mappings['<Ctrl-3>'] = '<Escape>'

## Defaults {{{

c.bindings.default = {} # I find many of the defaults unintuitive/unnecessary

# Recreate the defaults I prefer

nmap('h', 'scroll left')
nmap('j', 'scroll down')
nmap('k', 'scroll up')
nmap('l', 'scroll right')

nmap('<Ctrl+d>', 'scroll-page 0 0.5')
nmap('<Ctrl+u>', 'scroll-page 0 -0.5')

nmap('gg', 'scroll-to-perc 0')
nmap('G', 'scroll-to-perc')

nmap('-', 'zoom-out')
nmap('+', 'zoom-in')
nmap('=', 'zoom')

nmap('d', 'tab-close')
nmap('u', 'undo')

nmap('r', 'reload')
nmap('R', 'reload --force')

nmap('J', 'tab-next')
nmap('K', 'tab-prev')

nmap('H', 'back')
nmap('L', 'forward')
nmap('<Back>', 'back')
nmap('<Forward>', 'forward')

nmap('n', 'search-next')
nmap('N', 'search-prev')

nmap('[[', 'navigate prev')
nmap(']]', 'navigate next')

nmap('f', 'hint')
nmap('F', 'hint all tab')

nmap('i', 'mode-enter insert')
nmap('v', 'mode-enter caret')
nmap('V', 'mode-enter caret ;; toggle-selection --line')

nmap('yy', 'yank') # url
nmap('yt', 'yank title')
nmap('yd', 'yank domain')
nmap('yp', 'yank pretty-url')
nmap('ym', 'yank inline [{title}]({url})') # markdown style

nmap('pp', 'open -- {clipboard}')

nmap(':', 'set-cmd-text :')
nmap(';', 'set-cmd-text :')
nmap('/', 'set-cmd-text /')
nmap('?', 'set-cmd-text ?')

nmap('o', 'set-cmd-text -s :open')
nmap('O', 'set-cmd-text -s :open --tab')

nmap('m', 'quickmark-save')
nmap('b', 'set-cmd-text -s :quickmark-load')
nmap('B', 'set-cmd-text -s :quickmark-load --tab')

vmap('v', 'toggle-selection')
vmap('V', 'toggle-selection --line')
vmap('h', 'move-to-prev-char')
vmap('j', 'move-to-next-line')
vmap('k', 'move-to-prev-line')
vmap('l', 'move-to-next-char')
vmap('e', 'move-to-end-of-word')
vmap('w', 'move-to-next-word')
vmap('b', 'move-to-prev-word')
vmap('0', 'move-to-start-of-line')
vmap('$', 'move-to-end-of-line')
vmap('gg', 'move-to-start-of-document')
vmap('G', 'move-to-end-of-document')
vmap('y', 'yank selection')
vmap('[', 'move-to-start-of-prev-block')
vmap(']', 'move-to-start-of-next-block')
vmap('{', 'move-to-end-of-prev-block')
vmap('}', 'move-to-end-of-next-block')

for mode in ['prompt', 'register', 'hint', 'yesno', 'caret', 'insert', 'command']:
    config.bind('<Escape>', 'mode-leave', mode=mode)

config.bind('<Return>', 'command-accept', mode='command')

config.bind('<Shift+Escape>', 'mode-leave', mode='passthrough')

config.bind('<Return>', 'follow-hint', mode='hint')

config.bind('<Return>', 'prompt-accept', mode='prompt')
config.bind('<Up>', 'prompt-item-focus prev', mode='prompt')
config.bind('<Down>', 'prompt-item-focus next', mode='prompt')
config.bind('<Shift+Tab>', 'prompt-item-focus prev', mode='prompt')
config.bind('<Tab>', 'prompt-item-focus next', mode='prompt')

config.bind('<Return>', 'prompt-accept', mode='yesno')
config.bind('y', 'prompt-accept yes', mode='yesno')
config.bind('n', 'prompt-accept no', mode='yesno')
config.bind('Y', 'prompt-accept --save yes', mode='yesno')
config.bind('N', 'prompt-accept --save no', mode='yesno')

## Defaults }}}

## Boomer bindings {{{
# Muscle-memory compatability with Chrome/other GUI browsers

nmap('<Ctrl+t>', 'open --tab')
nmap('<Ctrl+Shift+t>', 'undo')
nmap('<Ctrl+Shift+n>', 'open --private')
nmap('<F5>', 'reload')
nmap('<F11>', 'fullscreen')
nmap('<Ctrl+Tab>', 'tab-next')
nmap('<Ctrl+Shift+Tab>', 'tab-prev')
for i in range(9):
    nmap('<Ctrl+{}>'.format(i), 'tab-focus {}'.format(i))
# }}}

## Misc. ergonomics {{{
nmap('<Esc>', 'fake-key <Esc>')
nmap('<Ctrl+[>', 'clear-messages')
imap('<Ctrl+o>', 'open-editor')
## Misc. ergonics }}}

## Hints (t*) {{{
nmap('T', 'hint --first inputs')
nmap('tt', 'hint inputs')
nmap('tf', 'hint --rapid links tab-bg')
nmap('ty', 'hint links yank')
nmap('td', 'hint links download')
nmap('tD', 'hint --rapid links download')
nmap('to', 'hint links fill :open {hint-url}')
nmap('tO', 'hint links fill :open --tab --related {hint-url}')
nmap('th', 'hint all hover')
nmap('ti', 'hint images')
nmap('tI', 'hint images tab')
## Hints

## Goto: (g*) {{{
nmap('gp', 'open -- {clipboard}')
nmap('gP', 'open --tab -- {clipboard}')
nmap('gc', 'tab-clone')
nmap('gC', 'tab-clone --tab')
nmap('gK', 'tab-move -')
nmap('gJ', 'tab-move +')
nmap('gL', 'forward --tab')
nmap('gH', 'back --tab')

# Not thematically consistent, but useful:
nmap('gi', 'hint --first inputs')
nmap('go', 'set-cmd-text -s :open --bg')

## Goto }}}

## Current page: (c*) {{{
nmap('co', 'set-cmd-text :open {url:pretty}')
nmap('cO', 'set-cmd-text :open --tab {url:pretty}')
nmap('cs', 'navigate strip')
nmap('cS', 'navigate --tab strip')
## }}}

## Userscript/externally-dependent bindings {{{
config.bind('<Ctrl-g>', 'config-cycle content.user_stylesheets     ' +
        '"~/.config/qutebrowser/css/darculized-all-sites.css"      ' +
        '"~/.config/qutebrowser/css/solarized-dark-all-sites.css"  ' +
        '""')

nmap('<Ctrl+Shift+l>', 'spawn --userscript qute-pass')
imap('<Ctrl+Shift+l>', 'spawn --userscript qute-pass')

nmap('yc', 'hint code userscript ' + userscript('code_select'))
## Userscript/externally-dependent bindings }}}

## Readline-/macOS-style bindings {{{

config.bind('<Ctrl+n>', 'prompt-item-focus next', mode='prompt')
config.bind('<Ctrl+p>', 'prompt-item-focus prev', mode='prompt')

config.bind('<Ctrl+n>', 'completion-item-focus --history next', mode='command')
config.bind('<Ctrl+p>', 'completion-item-focus --history prev', mode='command')

for mode in ['command', 'prompt']:
    # Readline-style mode
    config.bind('<Ctrl+d>'          , 'rl-delete-char'              , mode=mode)
    config.bind('<Alt+d>'           , 'rl-kill-word'                , mode=mode)
    config.bind('<Ctrl+k>'          , 'rl-kill-line'                , mode=mode)
    config.bind('<Ctrl+y>'          , 'rl-yank'                     , mode=mode)

    config.bind('<Ctrl+h>'          , 'rl-backward-delete-char'     , mode=mode)
    config.bind('<Alt+Backspace>'   , 'rl-backward-kill-word'       , mode=mode)
    config.bind('<Ctrl+Alt+h>'      , 'rl-backward-kill-word'       , mode=mode)
    config.bind('<Ctrl+w>'          , 'rl-unix-word-rubout'         , mode=mode)
    config.bind('<Ctrl+u>'          , 'rl-unix-line-discard'        , mode=mode)

    config.bind('<Ctrl+b>'          , 'rl-backward-char'            , mode=mode)
    config.bind('<Ctrl+f>'          , 'rl-forward-char'             , mode=mode)
    config.bind('<Alt+b>'           , 'rl-backward-word'            , mode=mode)
    config.bind('<Alt+f>'           , 'rl-forward-word'             , mode=mode)
    config.bind('<Ctrl+a>'          , 'rl-beginning-of-line'        , mode=mode)
    config.bind('<Ctrl+e>'          , 'rl-end-of-line'              , mode=mode)

if platform.system() == 'Linux':
    # Readline-style insert mode
    config.bind('<Ctrl+f>'          , 'fake-key <Right>'            , mode='insert')
    config.bind('<Ctrl+b>'          , 'fake-key <Left>'             , mode='insert')
    config.bind('<Ctrl+a>'          , 'fake-key <Home>'             , mode='insert')
    config.bind('<Ctrl+e>'          , 'fake-key <End>'              , mode='insert')
    config.bind('<Ctrl+n>'          , 'fake-key <Down>'             , mode='insert')
    config.bind('<Ctrl+p>'          , 'fake-key <Up>'               , mode='insert')
    config.bind('<Alt+f>'           , 'fake-key <Ctrl+Right>'       , mode='insert')
    config.bind('<Alt+b>'           , 'fake-key <Ctrl+Left>'        , mode='insert')
    config.bind('<Ctrl+d>'          , 'fake-key <Delete>'           , mode='insert')
    config.bind('<Alt+d>'           , 'fake-key <Ctrl+Delete>'      , mode='insert')
    config.bind('<Alt+Backspace>'   , 'fake-key <Ctrl+Backspace>'   , mode='insert')
    config.bind('<Ctrl+w>'          , 'fake-key <Ctrl+Backspace>'   , mode='insert')
    config.bind('<Ctrl+y>'          , 'insert-text {primary}'       , mode='insert')
    config.bind('<Ctrl+h>'          , 'fake-key <Backspace>'        , mode='insert')

    # macOS-like cut/copy/paste/select-all
    config.bind('<Meta+c>', 'fake-key <Ctrl+c>;;message-info "copied to clipboard"', mode='normal')
    config.bind('<Meta+v>', 'fake-key <Ctrl+v>;;message-info "pasted from clipboard"', mode='normal')
    config.bind('<Meta+x>', 'fake-key <Ctrl+x>;;message-info "cut to clipboard"', mode='normal')
    config.bind('<Meta+a>', 'fake-key <Ctrl+a>', mode='normal')

    config.bind('<Meta+a>', 'fake-key <Ctrl+a>', mode='insert')

    for mode in ['insert', 'command', 'prompt']:
        config.bind('<Meta+x>', 'fake-key -g <Ctrl+x>;;message-info "cut to clipboard"', mode=mode)
        config.bind('<Meta+c>', 'fake-key -g <Ctrl+c>;;message-info "copied to clipboard"', mode=mode)
        config.bind('<Meta+v>', 'fake-key -g <Ctrl+v>;;message-info "pasted from clipboard"', mode=mode)


if platform.system() == 'Darwin':
    # Readline-style insert mode
    config.bind('<Ctrl+n>'          , 'fake-key <Down>'             , mode='insert')
    config.bind('<Ctrl+p>'          , 'fake-key <Up>'               , mode='insert')
    config.bind('<Ctrl+w>'          , 'fake-key <Alt+Backspace>'    , mode='insert')

## Readline-/macOS-style bindings }}}

### Bindings/mappings }}}

### Auto-generated {{{

# User agent to send.  The following placeholders are defined:
# * `{os_info}`: Something like "X11; Linux x86_64".
# * `{webkit_version}`: The underlying WebKit version
#       (set to a fixed value with QtWebEngine).
# * `{qt_key}`: "Qt" for QtWebKit, "QtWebEngine" for QtWebEngine.
# * `{qt_version}`: The underlying Qt version.
# * `{upstream_browser_key}`: "Version" for QtWebKit, "Chrome" for QtWebEngine.
# * `{upstream_browser_version}`: The corresponding Safari/Chrome version.
# * `{qutebrowser_version}`: The currently running qutebrowser version.
#       The default value is equal to the unchanged user agent of QtWebKit/QtWebEngine.
#       Note that the value read from JavaScript is always the global value.
# Type: FormatString
config.set('content.headers.user_agent', 'Mozilla/5.0 ({os_info}) AppleWebKit/{webkit_version} (KHTML, like Gecko) {upstream_browser_key}/{upstream_browser_version} Safari/{webkit_version}', 'https://web.whatsapp.com/')
config.set('content.headers.user_agent', 'Mozilla/5.0 ({os_info}; rv:71.0) Gecko/20100101 Firefox/71.0', 'https://accounts.google.com/*')
config.set('content.headers.user_agent', 'Mozilla/5.0 ({os_info}) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/99 Safari/537.36', 'https://*.slack.com/*')
config.set('content.headers.user_agent', 'Mozilla/5.0 ({os_info}; rv:71.0) Gecko/20100101 Firefox/71.0', 'https://docs.google.com/*')

# Load images automatically in web pages. Type: Bool
config.set('content.images', True, 'chrome-devtools://*')
config.set('content.images', True, 'devtools://*')

# Enable JavaScript. Type: Bool
config.set('content.javascript.enabled', True, 'chrome-devtools://*')
config.set('content.javascript.enabled', True, 'devtools://*')
config.set('content.javascript.enabled', True, 'chrome://*/*')
config.set('content.javascript.enabled', True, 'qute://*/*')

# Auto-generated }}}
