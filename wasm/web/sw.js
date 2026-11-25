// Service worker: the whole game runs in the browser, so with these files cached
// it plays offline too (saved games are in localStorage anyway).
// Network first, cache as fallback: a new build shows up on the next visit,
// and without a connection the last downloaded version plays.
const CACHE = "mmm-v1";
const FILES = [
  "./", "index.html", "esej.html", "manifest.webmanifest", "favicon-48.png", "icon-192.png",
  "game.js", "state.js", "map.js", "layout.js", "data.js", "cp852.js",
  "../dist/mmm.js", "../dist/mmm.wasm", "../dist/mmm-cz.js", "../dist/mmm-cz.wasm",
  "../dist/mmm-en.js", "../dist/mmm-en.wasm",
];

self.addEventListener("install", (e) => {
  e.waitUntil(caches.open(CACHE)
    .then((c) => Promise.all(FILES.map((f) => c.add(new Request(f, { cache: "no-cache" })).catch(() => {}))))
    .then(() => self.skipWaiting()));
});

self.addEventListener("activate", (e) => {
  e.waitUntil(caches.keys()
    .then((keys) => Promise.all(keys.filter((k) => k !== CACHE).map((k) => caches.delete(k))))
    .then(() => self.clients.claim()));
});

self.addEventListener("fetch", (e) => {
  const req = e.request;
  if (req.method !== "GET" || new URL(req.url).origin !== location.origin) return;
  e.respondWith(
    fetch(req).then((res) => {
      if (res.ok && res.type === "basic") {
        const copy = res.clone();
        caches.open(CACHE).then((c) => c.put(req, copy));
      }
      return res;
    }).catch(async () =>
      (await caches.match(req, { ignoreSearch: true })) ||
      (req.mode === "navigate" ? caches.match("index.html") : Response.error())),
  );
});
