// my override firefox config

// Improved protection for browser fingerprinting with fonts
user_pref("browser.display.use_document_fonts", 0);

// Enable search with search engine in address bar
user_pref("keyword.enabled", true);

// Don't use bookmarks for address bar suggestion
user_pref("browser.urlbar.suggest.bookmark", false);

// Don't use browser history for address bar suggestion
user_pref("browser.urlbar.suggest.history", false);

// Don't use open pages for address bar suggestion
user_pref("browser.urlbar.suggest.openpage", false);

// Start Firefox with home page
user_pref("browser.startup.page", 1);

// Set Firefox home page to about:home
user_pref("browser.startup.homepage", "about:home");

// Enable new tabpage
user_pref("browser.newtabpage.enabled", true);

// Don't show the sites i visit most on Firefox Home screen
user_pref("browser.newtabpage.activity-stream.feeds.topsites", false);

// Don't show Highlights section on Firefox Home screen
user_pref("browser.newtabpage.activity-stream.feeds.section.highlights", false);

// Don't show updates from Mozilla and Firefox on Firefox Home screen
user_pref("browser.newtabpage.activity-stream.feeds.snippets", false);

// Disable Recommended Extensions Suggestions in Firefox
user_pref("browser.newtabpage.activity-stream.asrouter.userprefs.cfr.addons", false);

// Disable Recommended Features Suggestions in Firefox
user_pref("browser.newtabpage.activity-stream.asrouter.userprefs.cfr.features", false);

// Disable browser history
user_pref("places.history.enabled", false);

// Disable Firefox save password prompt
user_pref("signon.rememberSignons", false);

// Disable Firefox Pocket
user_pref("extensions.pocket.enabled", false);

// Disable Firefox Sync
user_pref("identity.fxaccounts.enabled", false);

// Cookie expires at the end of the session
user_pref("network.cookie.lifetimePolicy", 2);

// Disable alt + mousewheel = Back/Forward in history
user_pref("mousewheel.with_alt.action", 1);

// Enable chrome/userChrome.css
user_pref("toolkit.legacyUserProfileCustomizations.stylesheets", true);

// When you open a link in a new tab, switch to it immediately
// user_pref("browser.tabs.loadInBackground", false);
