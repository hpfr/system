// much of this configuration is modified from https://github.com/b0o/surfingkeys-conf
settings.smoothScroll = false;

const openLink = (url, newTab = false, active = true) => {
  if (newTab) {
     api.RUNTIME("openLink", { tab: { tabbed: true, active },
                               url: url instanceof URL ? url.href : url
                })
  } else {
     window.location.assign(url)
  }
};

const canonicalFallback = () =>
      document.querySelector("link[rel='canonical']")?.getAttribute("href") || document.location.href;
// TODO: org-protocol for capture
const getOrgLink = (href = canonicalFallback(), title = document.title) =>
      `[[${href}][${title}]]`;
const getMarkdownLink = (href = canonicalFallback(), title = document.title) =>
      `[${title}](${href})`;

// TODO: deduplicate
const getWayBackLatestUrl = (href = canonicalFallback()) =>
      `https://web.archive.org/web/${href}`;
const getWayBackIndexUrl = (href = canonicalFallback()) =>
      `https://web.archive.org/web/*/${href}`;
const getArchiveTodayLatestUrl = (href = canonicalFallback()) =>
      `https://archive.today/newest/${href}`;
const getArchiveTodayIndexUrl = (href = canonicalFallback()) =>
      `https://archive.today/${href}*`;
const getDiscuEuUrl = (href = canonicalFallback()) =>
      `https://discu.eu/?${(new URLSearchParams({ url: href }))}`;

// search aliases
const defaultAliases = "bdeghswy";
defaultAliases.split('').forEach(c => api.removeSearchAlias(c));

api.addSearchAlias('dd', 'duckduckgo', 'https://duckduckgo.com/?q=', 's',
               'https://duckduckgo.com/ac/?q=',
               response => JSON.parse(response.text).map(r => r.phrase)
);
api.addSearchAlias('wk', 'Wikipedia', 'https://en.wikipedia.org/w/index.php?title=Special%3ASearch&search=', 's',
               'https://en.wikipedia.org/w/api.php?action=opensearch&format=json&formatversion=2&namespace=0&limit=40&search=',
               response => JSON.parse(response.text)[1]
);
api.addSearchAlias('wb', 'WayBack Machine', getWayBackLatestUrl(''));
api.addSearchAlias('at', 'archive.today', getArchiveTodayLatestUrl(''));
api.addSearchAlias('wba', 'WayBack Machine (all)', getWayBackIndexUrl(''));
api.addSearchAlias('ata', 'archive.today (all)', getArchiveTodayIndexUrl('{0}'));
api.addSearchAlias('wd', 'Wordnik', 'https://www.wordnik.com/words?myWord=');
api.addSearchAlias('ol', 'Open Library', 'https://openlibrary.org/search?q=');
api.addSearchAlias('mg', 'Marginalia', 'https://search.marginalia.nu/search?query=')
api.addSearchAlias('du', 'Discu.eu', 'https://discu.eu/?q=');
api.addSearchAlias('hn', 'Hacker News', 'https://hn.algolia.com/?q=');
api.addSearchAlias('rd', 'Reddit', 'https://old.reddit.com/search?q=');
api.addSearchAlias('rdu', 'Reddit (URL)', 'https://old.reddit.com/search?q=url:');
api.addSearchAlias('so', 'Stack Overflow', 'https:stackoverflow.com/search?q=');
api.addSearchAlias('gh', 'GitHub', 'https://github.com/search?q=', 's',
                   'https://api.github.com/search/repositories?order=desc&q=',
                   response => {
                      var res = JSON.parse(response.text)['items'];
                      return res ? res.map(r => ({title: r.description, url: r.html_url}))
                         : [];
                   }
);
api.addSearchAlias('yt', 'YouTube', 'https:www.youtube.com/results?search_query=', 's',
               'https:clients1.google.com/complete/search?client=youtube&ds=yt&callback=cb&q=',
               response => JSON.parse(response.text.substr(9, response.text.length-10))[1].map(d => d[0])
);
api.addSearchAlias('gen', 'Genius', 'https://genius.com/search?q=');
api.addSearchAlias('man', 'ManKier', 'https://www.mankier.com/?q=');
api.addSearchAlias('nxp', 'Nix Packages', 'https://search.nixos.org/packages?channel=unstable&query=');
api.addSearchAlias('nxo', 'NixOS Options', 'https://search.nixos.org/options?channel=unstable&query=');
api.addSearchAlias('nxpr', 'Nixpkgs PR Tracker', 'https://nixpk.gs/pr-tracker.html?pr=');
api.addSearchAlias('nxh', 'Nix Home Manager Options', 'https://mipmip.github.io/home-manager-option-search/?');
api.addSearchAlias('gg', 'Google', 'https://www.google.com/search?q=', 's',
               'https://www.google.com/complete/search?client=chrome-omni&gs_ri=chrome-ext&oit=1&cp=1&pgcl=7&q=',
               response => JSON.parse(response.text)[1]
);
api.addSearchAlias('bg', 'Bing', 'https://www.bing.com/search?setmkt=en-us&setlang=en-us&q=',
               's', 'https://api.bing.com/osjson.aspx?query=',
               response => JSON.parse(response.text)[1]
);

settings.defaultSearchEngine = "dd";

// keys
api.mapkey("yO", "Copy current URL as Org link",
           () => api.Clipboard.write(getOrgLink())
);
api.mapkey("yM", "Copy current URL as Markdown link",
           () => api.Clipboard.write(getMarkdownLink())
);

api.mapkey("=w", "Open latest snapshot of current URL in the WayBack machine",
           () => openLink(getWayBackLatestUrl())
);
api.mapkey("=W", "Open snapshot index of current URL in the WayBack machine",
           () => openLink(getWayBackIndexUrl(), true)
);
api.mapkey("=a", "Open latest snapshot of current URL from archive.today",
           () => openLink(getArchiveTodayLatestUrl())
);
api.mapkey("=A", "Open snapshot index of current URL from archive.today",
           () => openLink(getArchiveTodayIndexUrl(), true)
);
api.mapkey("=d", "Find discussions on Discu.eu for the current URL",
           () => openLink(getDiscuEuUrl(), true)
);


// site-specific
api.mapkey("=p", "View PR in Nixpkgs PR Tracker",
           () => {
              if (/^\/NixOS\/nixpkgs\/pull\/\d/i.test(document.location.pathname)) {
                 openLink(`https://nixpk.gs/pr-tracker.html?pr=${document.location.pathname.split('/')[4]}`)
              }
           },
           {domain: /github\.com/i}
);
