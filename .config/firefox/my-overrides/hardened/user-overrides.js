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

// Dont use topsites for address bar suggestion
user_pref("browser.urlbar.suggest.topsites", false);

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

// Disable search and form history
user_pref("browser.formfill.enable", false);

// Clear history when Firefox closes
user_pref("privacy.sanitize.sanitizeOnShutdown", true);

// Always send "Do Not Track" signal
user_pref("privacy.donottrackheader.enable", true);

// Disable Firefox crash reports
user_pref("browser.crashReports.unsubmittedCheck.autoSubmit2", false);

// Disable Mozilla telemetry
user_pref("datareporting.healthreport.uploadEnabled", false);

// Disable Firefox studies
user_pref("app.shield.optoutstudies.enabled", false);

// Disable Firefox crash reports
user_pref("browser.crashReports.unsubmittedCheck.autoSubmit2", false);

// Custom tracking protection
user_pref("browser.contentblocking.category", "custom");
user_pref("privacy.trackingprotection.enabled", true);
user_pref("privacy.trackingprotection.socialtracking.enabled", true);
user_pref("privacy.trackingprotection.cryptomining.enabled", true);
user_pref("privacy.trackingprotection.fingerprinting.enabled", true);

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
