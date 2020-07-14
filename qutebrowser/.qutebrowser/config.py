# Documentation:
#   qute://help/configuring.html
#   qute://help/settings.html

c.aliases = {
        'q': 'close',
        'qa': 'quit',
        'w': 'session-save',
        'wq': 'quit --save',
        'wqa': 'quit --save',
        'o': 'open',
        'O': 'open --tab',
        't': 'open --background',
        'priv': 'open --private',
        'bookmarks': 'open -t qute://bookmarks/',
}

# Time interval (in milliseconds) between auto-saves of config/cookies/etc.
c.auto_save.interval = 15000

# Always restore open sites when qutebrowser is reopened. Type: Bool
c.auto_save.session = True

c.editor.command = ["/usr/local/bin/gvim", "-f", "{file}", "-c", "normal {line}G{column0}l"]

c.content.pdfjs = True
c.content.autoplay = False

config.bind('<Ctrl-n>', 'prompt-item-focus next', mode='prompt')
config.bind('<Ctrl-p>', 'prompt-item-focus prev', mode='prompt')



#### Auto-generated stuff ####

# User agent to send.  The following placeholders are defined:  *
# `{os_info}`: Something like "X11; Linux x86_64". * `{webkit_version}`:
# The underlying WebKit version (set to a fixed value   with
# QtWebEngine). * `{qt_key}`: "Qt" for QtWebKit, "QtWebEngine" for
# QtWebEngine. * `{qt_version}`: The underlying Qt version. *
# `{upstream_browser_key}`: "Version" for QtWebKit, "Chrome" for
# QtWebEngine. * `{upstream_browser_version}`: The corresponding
# Safari/Chrome version. * `{qutebrowser_version}`: The currently
# running qutebrowser version.  The default value is equal to the
# unchanged user agent of QtWebKit/QtWebEngine.  Note that the value
# read from JavaScript is always the global value.
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
