#### Documentation ####
#
#   qute://help/configuring.html
#   qute://help/settings.html
#
#######################

#### Configuration ####

import os
import platform

# Put kitty, gvim, etc. in Qutebrowser's PATH, at least on macOS
os.environ['PATH'] = '/usr/local/bin' + os.pathsep + os.environ['PATH']

# Time interval (in milliseconds) between auto-saves of config/cookies/etc.
c.auto_save.interval = 15000

# Always restore open sites when qutebrowser is reopened. Type: Bool
c.auto_save.session = True

c.editor.command = ["kitty", "vim", "-f", "{file}", "-c", "normal {line}G{column0}l"]

c.content.pdfjs = True
c.content.autoplay = False
c.tabs.background = True
c.tabs.close_mouse_button = 'right'

c.colors.webpage.prefers_color_scheme_dark = True

config.set('content.unknown_url_scheme_policy', 'allow-all', 'zoommtg://*.zoom.us')
config.set('content.unknown_url_scheme_policy', 'allow-all', 'https://*.slack.com')
config.set('content.javascript.can_access_clipboard', True, 'https://github.com/*')


#### Aliases and bindings ####

c.aliases['h'] = 'help'
c.aliases['py'] = 'debug-pyeval'

c.aliases['q'] = 'close'
c.aliases['qa'] = 'quit'

c.aliases['w'] = 'session-save'
c.aliases['wq'] = 'quit --save'
c.aliases['wqa'] = 'quit --save'

c.aliases['o'] = 'open'
c.aliases['O'] = 'open --tab'
c.aliases['t'] = 'open --background'

c.aliases['priv'] = 'open --private'
c.aliases['bookmarks'] = 'open -t qute://bookmarks/'

c.aliases['mpv'] = 'spawn mpv --autofit=100%x100% --force-window=immediate {url}'
c.aliases['ompv'] = 'hint links spawn mpv --autofit=100%x100% --force-window=immediate {hint-url}'

c.aliases['chrome'] = 'spawn open -a "Google Chrome" {url}'
c.aliases['ochrome'] = 'hint all spawn open -a "Google Chrome" {hint-url}'


config.bind('<Ctrl+Tab>', 'tab-next', mode='normal')
config.bind('<Ctrl+Shift+Tab>', 'tab-prev', mode='normal')
config.bind('<Esc>', 'fake-key <Esc>', mode='normal')
config.bind('<Ctrl+[>', 'clear-messages', mode='normal')

config.bind('<Ctrl+n>', 'prompt-item-focus next', mode='prompt')
config.bind('<Ctrl+p>', 'prompt-item-focus prev', mode='prompt')

config.bind('<Ctrl+n>', 'completion-item-focus --history next', mode='command')
config.bind('<Ctrl+p>', 'completion-item-focus --history prev', mode='command')

config.unbind('<Ctrl+e>', mode='insert')
config.bind('<Ctrl+o>', 'open-editor', mode='insert')

for mode in ['command', 'prompt']:

    config.bind('<Ctrl+d>', 'rl-delete-char', mode=mode)
    config.bind('<Alt+d>', 'rl-kill-word', mode=mode)
    config.bind('<Ctrl+k>', 'rl-kill-line', mode=mode)

    config.bind('<Ctrl+h>', 'rl-backward-delete-char', mode=mode)
    config.bind('<Ctrl+w>', 'rl-unix-word-rubout', mode=mode)
    config.bind('<Alt+Backspace>', 'rl-unix-word-rubout', mode=mode)
    config.bind('<Ctrl+u>', 'rl-unix-line-discard', mode=mode)

    config.bind('<Ctrl+b>', 'rl-backward-char', mode=mode)
    config.bind('<Ctrl+f>', 'rl-forward-char', mode=mode)
    config.bind('<Alt+b>', 'rl-backward-word', mode=mode)
    config.bind('<Alt+f>', 'rl-forward-word', mode=mode)
    config.bind('<Ctrl+a>', 'rl-beginning-of-line', mode=mode)
    config.bind('<Ctrl+e>', 'rl-end-of-line', mode=mode)

    config.bind('<Ctrl+y>', 'rl-yank', mode=mode)

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
    config.bind('<Ctrl+y>'          , 'insert-text {primary}'       , mode='insert')

    # macOS-like cut/copy/paste/select-all
    config.bind('<Meta+c>', 'fake-key <Ctrl+c>;;message-info "copied to clipboard"', mode='normal')

    for mode in ['insert', 'command', 'prompt']:
        config.bind('<Meta+x>', 'fake-key <Ctrl+x>', mode=mode)
        config.bind('<Meta+c>', 'fake-key <Ctrl+c>', mode=mode)
        config.bind('<Meta+v>', 'fake-key <Ctrl+v>', mode=mode)
        config.bind('<Meta+a>', 'fake-key <Ctrl+a>', mode=mode)

### Userscripts ###

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
config.bind(';c', 'hint code userscript ' + userscript('code_select.py'), mode='normal')


#### Auto-generated ####

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
