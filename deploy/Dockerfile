# Magical Mystery Mansion — static web build.
#   docker build -f deploy/Dockerfile -t mmm-web .        (from the repo root)
# Stage 1 translates the Fortran sources to WebAssembly (prep.py -> f2c -> emcc),
# stage 2 serves the page and the three engines with nginx. No backend: the
# game runs in the browser, saved games live in the player's localStorage.

FROM emscripten/emsdk:7.0.1 AS build
RUN apt-get -qq update && apt-get -qq install -y --no-install-recommends f2c unzip >/dev/null \
 && rm -rf /var/lib/apt/lists/*
WORKDIR /src
COPY MMM_LAT/ MMM_LAT/
COPY native/ native/
COPY original_en/hp1000-fortran/ original_en/hp1000-fortran/
COPY wasm/ wasm/
RUN cd wasm && ./build.sh web puvodni && ./build.sh web cz && ./en/build.sh web

# flatten: page files at /, engines at /dist (game.js imports ../dist/*.js)
RUN mkdir -p /site/dist \
 && cd wasm/web && cp index.html esej.html game.js state.js map.js layout.js data.js cp852.js \
      sw.js manifest.webmanifest favicon-48.png icon-192.png icon-512.png icon-maskable-512.png apple-touch-icon.png og-esej.png /site/ \
 && cd ../dist && cp mmm.js mmm.wasm mmm-cz.js mmm-cz.wasm mmm-en.js mmm-en.wasm /site/dist/

FROM nginx:1.27-alpine
COPY deploy/nginx.conf /etc/nginx/conf.d/default.conf
COPY --from=build /site/ /usr/share/nginx/html/
EXPOSE 80
HEALTHCHECK CMD wget -q -O /dev/null http://127.0.0.1/ || exit 1
