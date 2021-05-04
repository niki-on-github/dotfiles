/***
 This will reset the preferences that have been removed completely from the arkenfox user.js.

 Last updated: 04-Mar-2021

 For instructions see:
 https://github.com/arkenfox/user.js/wiki/3.1-Resetting-Inactive-Prefs-[Scripts]
***/
 
(function() {
  let ops = [
    /* removed in arkenfox user.js */
    /* 52-alpha */
    'browser.search.reset.enabled',
    'browser.search.reset.whitelist',
    /* 54-alpha */
    'browser.migrate.automigrate.enabled',
    'services.sync.enabled',
    'webextensions.storage.sync.enabled',
    'webextensions.storage.sync.serverURL',
    /* 55-alpha */
    'dom.keyboardevent.dispatch_during_composition', // default is false anyway
    'dom.vr.oculus.enabled', // covered by dom.vr.enabled
    'dom.vr.openvr.enabled', // ditto
    'dom.vr.osvr.enabled', // ditto
    'extensions.pocket.api', // covered by extensions.pocket.enabled
    'extensions.pocket.oAuthConsumerKey', // ditto
    'extensions.pocket.site', // ditto
    /* 57-alpha */
    'geo.wifi.xhr.timeout', // covered by geo.enabled
    'browser.search.geoip.timeout', // ditto
    'media.webspeech.recognition.enable', // default is false anyway
    'gfx.layerscope.enabled', // default is false anyway
    /* 58-alpha */
    //  excluding these e10 settings
       // 'browser.tabs.remote.autostart',
       // 'browser.tabs.remote.autostart.2',
       // 'browser.tabs.remote.force-enable',
       // 'browser.tabs.remote.separateFileUriProcess',
       // 'extensions.e10sBlocksEnabling',
       // 'extensions.webextensions.remote',
       // 'dom.ipc.processCount',
       // 'dom.ipc.shims.enabledWarnings',
       // 'dom.ipc.processCount.extension',
       // 'dom.ipc.processCount.file',
       // 'security.sandbox.content.level',
       // 'dom.ipc.plugins.sandbox-level.default',
       // 'dom.ipc.plugins.sandbox-level.flash',
       // 'security.sandbox.logging.enabled',
    'dom.presentation.controller.enabled',
    'dom.presentation.discoverable',
    'dom.presentation.discovery.enabled',
    'dom.presentation.enabled',
    'dom.presentation.receiver.enabled',
    'dom.presentation.session_transport.data_channel.enable',
    /* 59-alpha */
    'browser.stopReloadAnimation.enabled',
    'browser.tabs.insertRelatedAfterCurrent',
    'browser.tabs.loadDivertedInBackground',
    'browser.tabs.loadInBackground',
    'browser.tabs.selectOwnerOnClose',
    'browser.urlbar.clickSelectsAll',
    'browser.urlbar.doubleClickSelectsAll',
    'media.flac.enabled',
    'media.mediasource.enabled',
    'media.mediasource.mp4.enabled',
    'media.mediasource.webm.audio.enabled',
    'media.mediasource.webm.enabled',
    'media.mp4.enabled',
    'media.ogg.enabled',
    'media.ogg.flac.enabled',
    'media.opus.enabled',
    'media.raw.enabled',
    'media.wave.enabled',
    'media.webm.enabled',
    'media.wmf.amd.vp9.enabled',
    'media.wmf.enabled',
    'media.wmf.vp9.enabled',
    'ui.submenuDelay',
    /* 60-beta - these were all at default anyway */
    'device.storage.enabled',
    'general.useragent.compatMode.firefox',
    'network.dns.blockDotOnion',
    'network.stricttransportsecurity.preloadlist',
    'security.block_script_with_wrong_mime',
    'security.fileuri.strict_origin_policy',
    'security.sri.enable',
    /* 61-beta */
    'browser.laterrun.enabled',
    'browser.offline-apps.notify',
    'browser.rights.3.shown',
    'browser.slowStartup.maxSamples',
    'browser.slowStartup.notificationDisabled',
    'browser.slowStartup.samples',
    'browser.storageManager.enabled',
    'dom.allow_scripts_to_close_windows',
    'dom.disable_window_flip',
    'network.http.fast-fallback-to-IPv4',
    'offline-apps.quota.warn',
    'services.blocklist.signing.enforced',
    /* 62-beta */
    'browser.urlbar.autoFill.typed',
    'security.tls.version.fallback-limit',
    /* 63-beta */
    'extensions.webextensions.keepStorageOnUninstall',
    'extensions.webextensions.keepUuidOnUninstall',
    'privacy.trackingprotection.ui.enabled',
    /* 64-beta */
    'browser.eme.ui.enabled',
    'browser.sessionstore.max_windows_undo',
    'network.auth.subresource-img-cross-origin-http-auth-allow',
    'media.peerconnection.ice.tcp',
    'media.peerconnection.identity.enabled',
    'media.peerconnection.identity.timeout',
    'media.peerconnection.turn.disable',
    'media.peerconnection.use_document_iceservers',
    'media.peerconnection.video.enabled',
    'media.navigator.video.enabled',
    /* 65-beta */
    'browser.contentblocking.enabled',
    'browser.urlbar.maxHistoricalSearchSuggestions',
    /* 67-beta */
    'app.update.service.enabled',
    'app.update.silent',
    'app.update.staging.enabled',
    'browser.cache.disk.capacity',
    'browser.cache.disk.smart_size.enabled',
    'browser.cache.disk.smart_size.first_run',
    'browser.cache.offline.insecure.enable',
    'browser.safebrowsing.downloads.remote.url',
    'browser.safebrowsing.provider.google.reportMalwareMistakeURL',
    'browser.safebrowsing.provider.google.reportPhishMistakeURL',
    'browser.safebrowsing.provider.google.reportURL',
    'browser.safebrowsing.provider.google4.dataSharing.enabled',
    'browser.safebrowsing.provider.google4.dataSharingURL',
    'browser.safebrowsing.provider.google4.reportMalwareMistakeURL',
    'browser.safebrowsing.provider.google4.reportPhishMistakeURL',
    'browser.safebrowsing.provider.google4.reportURL',
    'browser.safebrowsing.reportPhishURL',
    'browser.sessionhistory.max_total_viewers',
    'browser.urlbar.filter.javascript',
    'canvas.capturestream.enabled',
    'dom.imagecapture.enabled',
    'dom.popup_maximum',
    'gfx.offscreencanvas.enabled',
    'javascript.options.shared_memory',
    'media.gmp-gmpopenh264.autoupdate',
    'media.gmp-gmpopenh264.enabled',
    'media.gmp-manager.updateEnabled',
    'media.gmp-manager.url',
    'media.gmp-manager.url.override',
    'media.gmp.trial-create.enabled',
    'media.gmp-widevinecdm.autoupdate',
    'network.cookie.leave-secure-alone',
    'network.cookie.same-site.enabled',
    'network.dnsCacheEntries',
    'network.dnsCacheExpiration',
    'network.proxy.autoconfig_url.include_path',
    'pdfjs.enableWebGL',
    'plugin.default.state',
    'plugin.defaultXpi.state',
    'plugin.scan.plid.all',
    'security.data_uri.block_toplevel_data_uri_navigations',
    'security.insecure_field_warning.contextual.enabled',
    'security.insecure_password.ui.enabled',
    'signon.autofillForms.http',
    'signon.storeWhenAutocompleteOff',
    'xpinstall.whitelist.required',
    /* 67-beta: Blocklist, SB & TP cleanup: these were all inactive */
    'browser.safebrowsing.downloads.remote.block_dangerous',
    'browser.safebrowsing.downloads.remote.block_dangerous_host',
    'browser.safebrowsing.blockedURIs.enabled',
    'browser.safebrowsing.provider.google.gethashURL',
    'browser.safebrowsing.provider.google.updateURL',
    'browser.safebrowsing.provider.google4.gethashURL',
    'browser.safebrowsing.provider.google4.updateURL',
    'browser.safebrowsing.provider.mozilla.gethashURL',
    'browser.safebrowsing.provider.mozilla.updateURL',
    'browser.urlbar.userMadeSearchSuggestionsChoice',
    'privacy.trackingprotection.annotate_channels',
    'privacy.trackingprotection.enabled',
    'privacy.trackingprotection.lower_network_priority',
    'privacy.trackingprotection.pbmode.enabled',
    'services.blocklist.addons.collection',
    'services.blocklist.gfx.collection',
    'services.blocklist.onecrl.collection',
    'services.blocklist.plugins.collection',
    'services.blocklist.update_enabled',
    'urlclassifier.trackingTable',
    /* 68-beta */
    'dom.forms.datetime',
    'font.blacklist.underline_offset',
    'font.name.monospace.x-unicode',
    'font.name.monospace.x-western',
    'font.name.sans-serif.x-unicode',
    'font.name.sans-serif.x-western',
    'font.name.serif.x-unicode',
    'font.name.serif.x-western',
    'layout.css.font-loading-api.enabled',
    'toolkit.telemetry.cachedClientID',
    /* 69-beta */
    'plugin.sessionPermissionNow.intervalInMinutes',
    /* 70-beta */
    'browser.cache.disk_cache_ssl',
    'browser.sessionhistory.max_entries',
    'dom.push.connection.enabled',
    'dom.push.serverURL',
    'extensions.getAddons.discovery.api_url',
    'extensions.htmlaboutaddons.discover.enabled',
    'extensions.webservice.discoverURL',
    'intl.locale.requested',
    'intl.regional_prefs.use_os_locales',
    'privacy.usercontext.about_newtab_segregation.enabled',
    'security.insecure_connection_icon.pbmode.enabled',
    'security.insecure_connection_text.pbmode.enabled',
    'webgl.dxgl.enabled',
    /* 71-beta */
    'media.block-autoplay-until-in-foreground',
    'middlemouse.paste',
    /* 75-beta */
    'browser.search.geoip.url',
    'browser.search.region',
    /* 79-beta */
    'browser.urlbar.usepreloadedtopurls.enabled',
    /* 82-beta */
    'dom.IntersectionObserver.enabled',
    'extensions.screenshots.upload-disabled',
    'privacy.partition.network_state',
    'security.ssl3.dhe_rsa_aes_128_sha',
    'security.ssl3.dhe_rsa_aes_256_sha',
    /* 84-beta */
    'browser.newtabpage.activity-stream.asrouter.providers.snippets',
    /* 85-beta */
    'network.http.redirection-limit',
    /* 86-beta */
    'media.gmp-widevinecdm.visible',
    /* reset parrot: check your open about:config after running the script */
    '_user.js.parrot'
  ]

  if("undefined" === typeof(Services)) {
    alert("about:config needs to be the active tab!");
    return;
  }
  
  let c = 0;
  for (let i = 0, len = ops.length; i < len; i++) {
    if (Services.prefs.prefHasUserValue(ops[i])) {   
      Services.prefs.clearUserPref(ops[i]);
      if (!Services.prefs.prefHasUserValue(ops[i])) {
        console.log("reset", ops[i]);
        c++;
      } else { console.log("failed to reset", ops[i]); }
    }
  }
  
  focus();
  
  let d = (c==1) ? " pref" : " prefs";
  if (c > 0) {
    alert("successfully reset " + c + d + "\n\nfor details check the Browser Console (Ctrl+Shift+J)");
  } else { alert("nothing to reset"); }
  
})();
