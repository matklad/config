
with import <nixpkgs> {};
(mkShell {
  buildInputs = [];
  shellHook = ''
    export PUPPETEER_EXECUTABLE_PATH=${chromium}/bin/chromium
'';
})
