settings.defaultSearchEngine = "d"; // DuckDuckGo
settings.smoothScroll = false;
settings.hintExplicit = true;
settings.hintShiftNonActive = true;

function dbg(s) { console.log("[pokerus]: " + s); }

var pk_version = 0.3
dbg("begin mapping (" + pk_version + ")")

/*** Unmapping ***/
// (Unused; unmap these first so they can be mapped to other things)

/* Misc */
unmap(';w');  // Focus top window
unmap('%');   // Scroll to percentage of current page
unmap(';m');  // Mouse out last element
unmap('B');   // Go on tab history back
unmap('gT');  // Go to first activated tab
unmap(';i');  // Insert jquery library on current page
unmap(';t');  // Translate selected text with google
unmap('gr');  // Read selected text or text from clipboard
unmap(';dh'); // Delete history older than 30 days
unmap(';s');  // Toggle PDF viewer

unmap('<Alt-p>'); // pin/unpin current tab
unmap('<Alt-m>'); // mute/unmute current tab

/* Search selection */

/* (Search selection doesn't make sense for normal mode) */
unmap('sg');
unmap('sd');
unmap('sb');
unmap('sw');
unmap('ss');
unmap('sh');
unmap('sy');

unmap('se');  // Open settings
unmap('sm');  // Open Markdown preview

unmap('oi');  // Open in Incognito window
unmap('ox');  // Open recently closed URL
unmap('om');  // Open URL from history
unmap('oh');  // Open URL from vim-like marks

// Delegate to DuckDuckGo to redirect searches
unmap('og');  // Open search with alias g
unmap('od');  // Open search with alias d
unmap('ow');  // Open search with alias w
unmap('oy');  // Open search with alias y
unmap('ob');  // Open search with alias b

unmap('gx0'); // Close all tabs on left
unmap('gxt'); // Close tab on left
unmap('gxT'); // Close tab on right
unmap('gx$'); // Close all tabs on right
unmap('gxx'); // Close all tabs except current one
unmap('gt');  // Go to last activated tab
unmap('<Ctrl-6>'); // Go to last used tab

// map('o.', 'sql'); // Show last action
// unmap('sql');

/* Yanking */
unmap('yq');  // Yank pre text
unmap('yc');  // Yank column
unmap('ymc'); // Yank multiple columns

unmap('yf');  // Copy form data in JSON on current page
unmap('yp');  // Copy form data for POST on current page
unmap(';pj'); // Restore settings data from clipboard
unmap(';pf'); // Fill form with data from yf
unmap(';pp'); // Paste html on current page

/* Sessions */
unmap('ZZ');
unmap('ZR');

/* Proxy */
unmap('cp');
unmap(';cp');
unmap(';ap');
unmap('spa');
unmap('spb');
unmap('spc');
unmap('spd');
unmap('sps');
unmap('sfr');

/* Query (translation) */
unmap('Q');  // Open omnibar for word translation
unmap('cq'); // Query word with Hints
unmap(';q'); // Toggle mouseSelectToQuery
vunmap('q'); // Query under cursor
unmap('yQ'); // Copy all query history of OmniQuery

unmap('H');   // Open opened in page in current tab (?)
unmap('<Ctrl-j>');
unmap('<Ctrl-k>');
unmap('<Ctrl-h>');
unmap('<Ctrl-l>');


/*** Mapping ***/

/* Modes */
unmap(':'); // Lets me map sequences beginning with ':'

imap('<Ctrl-[>', '<Esc>');
imap('<Ctrl-c>', '<Esc>');
cmap('<Ctrl-[>', '<Esc>');
cmap('<Ctrl-c>', '<Esc>');

vmapkey('<Ctrl-[>', '#9Exit visual mode', function () {
  if (Visual.state > 1) {
      Visual.hideCursor();
      Visual.selection.collapse(selection.anchorNode, selection.anchorOffset);
      Visual.showCursor();
  } else {
      Visual.visualClear();
      Visual.exit();
  }
  Visual.state--;
  Visual._onStateChange();
});

vmapkey('<Ctrl-c>', '#9Exit visual mode', function () {
  Visual.exit();
});

unmap('<Ctrl-i>'); // Go to edit box with vim editor (duplicate)

/* Page navigation */
map('<Ctrl-d>', 'd'); // Page down
map('<Ctrl-u>', 'e'); // Page up
unmap('e');
unmap('d');

map('^', '0');        // All the way to the left
unmap('0');

map('H', 'S'); // Go back in history
map('L', 'D'); // Go forward in history
unmap('S');
unmap('D');

map('u', 'X'); // Restore closed tab
unmap('X');

map('rw', 'W'); // New window with current tab
unmap('W');

map('gC', 'yt'); // Duplicate current tab
unmap('yt');
// map('rD', 'yT'); // Duplicate current tab in background
// unmap('yT');

map('pP', 'cc'); // Open selected link from clipboard
unmap('cc');

map('O', 'on'); // Open new tab
unmap('on');

map('gf', 'gs'); // View page source
unmap('gs');

/* Tabs */
// Note: <nth>T takes you to <nth> tab

map('K', 'E'); // Tab left
unmap('E');
map('J', 'R'); // Tab right
unmap('R');

map('d', 'x');   // Close tab
unmap('x');

/* Omnibar/search */

mapkey('::', '#8Open commands', function() {
    Front.openOmnibar({type: "Commands"});
});


map('o', 'sU'); // Edit url, reload
unmap('sU');
map('O', 'su'); // Edit url, new tab
unmap('su');

map('gO', 't');  // Open a URL in a new tab
unmap('t');


/* Page interaction */
map(';i', 'i');  // Go to edit box
unmap('i');
map(';I', 'I');  // Go to edit box with vim editor
unmap('I');

// map('I', '<Alt-s>'); // Toggle SurfingKeys
// unmap('<Alt-s>');
map('i', '<Alt-i>'); // Enter passthrough
unmap('<Alt-i>');
map('\\', 'p');       // Enter ephemeral passthrough
unmap('p');

map('F', 'C');   // Open link in non-active new tab
unmap('C'); unmap('af');
map(';R', 'cf'); // Open multiple links in new tab
unmap('cf');

map(';i', 'q'); // Click on an image or button
unmap('q');
map(';O', 'O');  // Open URL literal
unmap('O');

/* Scroll */
map('sf', ';fs'); // Display hints to focus scrollable elements
unmap(';fs');
map('sr', 'cS');  // Reset scroll target
unmap('cS');
map('ss', 'cs');  // Change scroll target
unmap('cs');

/* Clipboard */
mapkey('yY', '#7Copy all tabs url', function() {
    RUNTIME('getTabs', null, function (response) {
        Clipboard.write([window.location.href].concat(response.tabs.map(tab => tab.url)).join('\n'))
    })
});

map(';y', 'ya'); // Yank a link URL
unmap('ya');
// map('yF', 'yma'); // Yank a link URL
unmap('yma');

map(';Y', 'yv'); // Yank text of an element
unmap('yv');
// map('yW', 'ymv'); // Yank text of multiple elements
unmap('ymv');

/* Insert mode */
/* (Unmap most things here, delegate to OS readline shortcuts) */
imap('<Ctrl-a>', '<Ctrl-f>'); // Beginning of line
iunmap('<Ctrl-f>');
iunmap('<Ctrl-b>');
iunmap('<Ctrl-u>');
iunmap('<Alt-w>');
iunmap('<Alt-d>');

/* Vim text editor */
aceVimMap('kj', '<Esc>', 'insert');

/* Site-specific */

var numMappings = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9'];

['/', 'j', 'k'].forEach(function(k, i) {
   unmap(k, /duckduckgo\.com/);
});
[['m', 'f'], numMappings].flat().forEach(function(k, i) {
   unmap(k, /youtube\.com/);
});
[['n', 'p', 'z'], numMappings].flat().forEach(function(k, i) {
   unmap(k, /gradescope\.com/);
});
['/', 'j', 'k', 'n', 'p'].forEach(function(k, i) {
   unmap(k, /vimawesome\.com/);
});

unmapAllExcept(['J', 'K', 'H', 'L'],
    /mail\.google\.com/);
unmapAllExcept(['J', 'K', 'H', 'L'],
    /drive\.google\.com/);
unmapAllExcept(['J', 'K', 'H', 'L'],
    /meet\.google\.com/);
unmapAllExcept([],
    /docs\.google\.com/);
unmapAllExcept([],
    /.+\.slack\.com/);
unmapAllExcept([],
    /overleaf\.com/);
unmapAllExcept([],
    /hackerrank\.com/);
unmapAllExcept([],
    /cloud\.digitalocean\.com/);
unmapAllExcept([],
    /learn\.dvorak\.nl/);
unmapAllExcept([],
    /monkeytype\.com/);

dbg("Qute keys mapped");

// set theme
settings.theme = `
.sk_theme {
    font-family: Input Sans Condensed, Charcoal, sans-serif;
    font-size: 10pt;
    background: #24272e;
    color: #abb2bf;
}
.sk_theme tbody {
    color: #fff;
}
.sk_theme input {
    color: #d0d0d0;
}
.sk_theme .url {
    color: #61afef;
}
.sk_theme .annotation {
    color: #56b6c2;
}
.sk_theme .omnibar_highlight {
    color: #528bff;
}
.sk_theme .omnibar_timestamp {
    color: #e5c07b;
}
.sk_theme .omnibar_visitcount {
    color: #98c379;
}
.sk_theme #sk_omnibarSearchResult>ul>li:nth-child(odd) {
    background: #303030;
}
.sk_theme #sk_omnibarSearchResult>ul>li.focused {
    background: #3e4452;
}
#sk_status, #sk_find {
    font-size: 20pt;
}

:root {
    --theme-ace-bg:#282828ab; /*Note the fourth channel, this adds transparency*/
    --theme-ace-bg-accent:#3c3836;
    --theme-ace-fg:#ebdbb2;
    --theme-ace-fg-accent:#7c6f64;
    --theme-ace-cursor:#928374;
    --theme-ace-select:#458588;
}
#sk_editor {
    height: 50% !important; /*Remove this to restore the default editor size*/
    background: var(--theme-ace-bg) !important;
}
.ace_dialog-bottom{
    border-top: 1px solid var(--theme-ace-bg) !important;
}
.ace-chrome .ace_print-margin, .ace_gutter, .ace_gutter-cell, .ace_dialog{
    background: var(--theme-ace-bg-accent) !important;
}
.ace-chrome{
    color: var(--theme-ace-fg) !important;
}
.ace_gutter, .ace_dialog {
    color: var(--theme-ace-fg-accent) !important;
}
.ace_cursor{
    color: var(--theme-ace-cursor) !important;
}
.normal-mode .ace_cursor{
    background-color: var(--theme-ace-cursor) !important;
    border: var(--theme-ace-cursor) !important;
}
.ace_marker-layer .ace_selection {
    background: var(--theme-ace-select) !important;
}
`;
