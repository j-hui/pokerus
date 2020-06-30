settings.defaultSearchEngine = "d"; // DuckDuckGo
settings.smoothScroll = false;
settings.hintExplicit = true;
settings.hintShiftNonActive = true;

function dbg(s) { console.log("[pokerus]: " + s); }

/* Chord prefix mnemonics:
 *
 *   (r)eload: related to current page
 *   (z)oom: related to page resolution
 *   (:): related to omnibar
 *   (g)oto: links, input, and other graphical interaction
 *   (s)croll: select scroll elements
 *   (y)ank: pull to clipboard
 *
 */

var pk_version = 0.1
dbg("begin mapping (" + pk_version + ")");

/* Misc */
// (Unused; unmap these first so they can be mapped to other things)

unmap(';w');  // Focus top window
unmap('%');   // Scroll to percentage of current page
unmap(';m');  // Mouse out last element
unmap('B');   // Go on tab history back
unmap('gT');  // Go to first activated tab
unmap(';i');  // Insert jquery library on current page
unmap(';t');  // Translate selected text with google
unmap('gr');  // Read selected text or text from clipboard
unmap(';dh'); // Delete history older than 30 days

unmap('<Alt-p>'); // pin/unpin current tab
unmap('<Alt-m>'); // mute/unmute current tab

// Search selection
unmap('sg');
unmap('sd');
unmap('sb');
unmap('sw');
unmap('ss');
unmap('sh');
unmap('sy');

/* (Search selection doesn't make sense for normal mode) */
unmap('sg');
unmap('sd');
unmap('sb');
unmap('sw');
unmap('ss');
unmap('sh');
unmap('sy');

/* Modes */
unmap(':'); // Lets me map chords beginning with ':'

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

map('!', '<Alt-s>'); // Toggle SurfingKeys
unmap('<Alt-s>');
map('|', '<Alt-i>'); // Enter passthrough
unmap('<Alt-i>');
map('\\', 'p');       // Enter ephemeral passthrough
unmap('p');

/* Page navigation */
map('<Ctrl-d>', 'd'); // Page down
map('<Ctrl-u>', 'e'); // Page up
// map('<Ctrl-j>', 'd'); // Page down
// map('<Ctrl-k>', 'e'); // Page up
unmap('e');
unmap('d');

vmap('<Ctrl-j>', '<Ctrl-d>'); // Page down
vmap('<Ctrl-k>', '<Ctrl-u>'); // Page down

map('<Ctrl-a>', '0'); // All the way to the left
map('^', '0');        // All the way to the left
map('<Ctrl-e>', '$'); // All the way to the right
unmap('0');
unmap('<Ctrl-j>');
unmap('<Ctrl-k>');
unmap('<Ctrl-h>');
unmap('<Ctrl-l>');

var ctrlMappings = ['<Ctrl-d>', '<Ctrl-u>', '<Ctrl-j>', '<Ctrl-k>', '<Ctrl-h>', '<Ctrl-l>'];


/* Reload/current page */
unmap('r');     // We're going to use 'r' to chord

mapkey('rr', '#4Reload current page', function() {
    RUNTIME("reloadTab", { nocache: false });
});

map('r?', 'g?'); // Reload current page without query string
map('r#', 'g#'); // Reload current page without hash fragment
unmap('g?');
unmap('g#');

map('rk', 'gu'); // Go up one path in the URL
map('rj', 'gU'); // Go up one path in the URL
unmap('gu');
unmap('gU');

map('rh', 'S'); // Go back in history
map('rl', 'D'); // Go forward in history
unmap('S');
unmap('D');

map('ru', 'X'); // Restore closed tab
unmap('X');

map('rw', 'W'); // New window with current tab
unmap('W');

map('rd', 'yt'); // Duplicate current tab
map('rD', 'yT'); // Duplicate current tab in background
unmap('yt');
unmap('yT');

map('rp', 'cc'); // Open selected link from clipboard
unmap('cc');

map('ro', 'on'); // Open new tab
unmap('on');

map('rq', 'oi'); // Open incognito window
unmap('oi');

map('r,', 'se'); // Open settings
unmap('se');

map('rm', 'sm'); // Open markdown preview
unmap('sm');

map('rz', 'gs'); // View page source
unmap('gs');

map('r/', '/');
map('r?', '?');

/* Tabs */
// Note: <nth>T takes you to <nth> tab

map('J', 'E'); // Tab left
map('rJ', 'E');
map('K', 'R'); // Tab right
map('rK', 'R');
unmap('E');
unmap('R');

map('rx', 'x');   // Close tab

map('H', 'g0');   // Go to first tab
map('rH', 'g0');
map('L', 'g$');   // Go to last tab
map('rL', 'g$');
unmap('g0');
unmap('g$');

unmap('gx0'); // Close all tabs on left
unmap('gxt'); // Close tab on left
unmap('gxT'); // Close tab on right
unmap('gx$'); // Close all tabs on right
unmap('gxx'); // Close all tabs except current one
unmap('gt');  // Go to last activated tab
unmap('<Ctrl-6>'); // Go to last used tab

// My r-chord mappings are mostly common, handy, one-shot commands,
// so website shortcuts that begin with 'r' be damned.
var rMappings = ['rr', 'r?', 'r#', 'rk', 'rj', 'rh', 'rl',
    'ru', 'rw', 'rd', 'rD', 'rp', 'ro', 'rq', 'r,', 'rm', 'rz',
    'r/', 'r?', 'rJ', 'rK', 'rH', 'rL', 'rx'];

/* Omnibar/search */

// Delegate to DuckDuckGo to redirect searches, but keep these handy
map(':/g', 'og');
unmap('og'); // Open search with alias g
map(':/d', 'od');
unmap('od'); // Open search with alias d
map(':/w', 'ow');
unmap('ow'); // Open search with alias w
map(':/y', 'oy');
unmap('oy'); // Open search with alias y
map(':/b', 'ob');
unmap('ob'); // Open search with alias b

mapkey('::', '#8Open commands', function() {
    Front.openOmnibar({type: "Commands"});
});

map(':r', 'sU'); // Edit url, reload
unmap('sU');
map(':R', 'su'); // Edit url, new tab
unmap('su');

map(':e', 'go'); // Open a URL in current tab
unmap('go');
map(':t', 't');  // Open a URL in a new tab
unmap('t');
map(':O', 'H');  // Open opened page in current tab
unmap('H');

map(':u', 'ox'); // Open recently closed URL
unmap('ox');
map(':U', 'oh'); // Open URL from history
unmap('oh');

map(':m', 'om'); // Open URL from vim-like marks
unmap('om');

map(':bo', 'b');   // Open a bookmark
unmap('b');
map(':ba', 'ab');  // Add bookmark
unmap('ab');
map(':bd', ';db'); // Delete bookmark
unmap(';db');

map(':v', ';s'); // Toggle PDF viewer
unmap(';s');

map(':q<Enter>', 'x'); // Close tab

var colonMappings = [':e', ':E', ':o', ':t', ':O', ':u', ':U',
    ':m', ':bo', ':ba', ':bd', ':v', ':q<Enter>'];

map('o.', 'sql'); // Show last action
unmap('sql');

/* Page interaction */
map('gI', 'gi'); // Go to the first edit box
map('rgI', 'gi');
unmap('gi');
map('gi', 'i');  // Go to edit box
map('rgi', 'i');
unmap('i');
map('ge', 'I');  // Go to edit box with vim editor
map('rge', 'I');
unmap('I');

map('gf', 'f');  // Follow link
map('rgf', 'f');
map('F', 'C');   // Open link in non-active new tab
map('gt', 'C');  // Open link in non-active new tab
unmap('C'); unmap('af');
map('gT', 'cf'); // Open multiple links in new tab
unmap('cf');

map('ga', 'q'); // Click on an image or button
unmap('q');
map('gu', 'O');  // Open URL literal
unmap('O');

map('g[', '[['); // Click on previous link on current page
unmap('[[');
map('g]', ']]'); // Click on next link on current page
unmap(']]');

var gMappings = ['gI', 'gi', 'ge', 'gf', 'gt', 'gT', 'gu', 'g[', 'g]', 'ga'];
rMappings = rMappings.concat(['rgi', 'rgI', 'rge', 'rgf']);

/* Scroll */
map('sf', ';fs'); // Display hints to focus scrollable elements
unmap(';fs');
map('sr', 'cS');  // Reset scroll target
map(':w', 'cS');
unmap('cS');
map('ss', 'cs');  // Change scroll target
unmap('cs');

var sMappings = ['sf', 'sr', 'ss'];
colonMappings = colonMappings.concat([':w']);

/* Clipboard */
mapkey('yY', '#7Copy all tabs url', function() {
    RUNTIME('getTabs', null, function (response) {
        Clipboard.write([window.location.href].concat(response.tabs.map(tab => tab.url)).join('\n'))
    })
});

map('yf', 'ya'); // Yank a link URL
unmap('ya');
map('yF', 'yma'); // Yank a link URL
unmap('yma');

map('yw', 'yv'); // Yank text of an element
unmap('yv');
map('yW', 'ymv'); // Yank text of multiple elements
unmap('ymv');

unmap('yq');  // Yank pre text
unmap('yc');  // Yank column
unmap('ymc'); // Yank multiple columns

unmap('yf');  // Copy form data in JSON on current page
unmap('yp');  // Copy form data for POST on current page
unmap(';pj'); // Restore settings data from clipboard
unmap(';pf'); // Fill form with data from yf
unmap(';pp'); // Paste html on current page

var yMappings = ['yy', 'yY', 'yf', 'yF', 'yw', 'yW'];

map('q', 'x');

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
// aceVimMap('<Ctrl-j>', '<Ctrl-d>', 'normal');
// aceVimMap('<Ctrl-k>', '<Ctrl-u>', 'normal');

/* Site-specific */

var numMappings = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9'];
var zMappings = ['zi', 'zo', 'zr'];

// unmapAllExcept([ctrlMappings, rMappings, colonMappings, gMappings, sMappings, yMappings, zMappings].flat(),

['/', 'j', 'k'].forEach(function(k, i) {
   unmap(k, /duckduckgo\.com/);
});
// [['m', 'f'], numMappings].flat().forEach(function(k, i) {
//    unmap(k, /youtube\.com/);
// });
[['n', 'p', 'z'], numMappings].flat().forEach(function(k, i) {
   unmap(k, /gradescope\.com/);
});
['/', 'j', 'k', 'n', 'p'].forEach(function(k, i) {
   unmap(k, /vimawesome\.com/);
});

unmapAllExcept([ctrlMappings, rMappings, colonMappings, sMappings, yMappings].flat(),
    /mail\.google\.com/);
unmapAllExcept([ctrlMappings, rMappings, colonMappings, sMappings, yMappings].flat(),
    /drive\.google\.com/);
unmapAllExcept([],
    /docs\.google\.com/);
unmapAllExcept([ctrlMappings, rMappings, colonMappings, gMappings, sMappings, yMappings].flat(),
    /.+\.slack\.com/);
unmapAllExcept([],
    /overfleaf\.com/);
unmapAllExcept([],
    /hackerrank\.com/);
unmapAllExcept([],
    /cloud\.digitalocean\.com/);
unmapAllExcept([ctrlMappings].flat(),
    /learn\.dvorak\.nl/);

dbg("keys mapped");

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
