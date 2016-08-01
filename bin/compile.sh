#!/bin/sh

uglifyjs src/utils.js -o js/utils.min.js

uglifyjs src/worker.js -o js/worker.min.js

uglifyjs src/view/view.js src/bib.js src/bib.js src/model.js \
  src/view/about.js src/view/doc.js src/view/model.js src/view/model_list.js \
  src/view/topic.js src/view/word.js src/view/bib.js src/view/model_yearly.js \
  src/view/model_plot.js src/view/settings.js src/view/words.js \
  src/main.js  -o js/dfb.min.js
