# Do it yourself slides

This uses https://github.com/Mogztter/asciidoctor-web-pdf to make pdf slides.

The workflow is:

* the contents of the presentation is written in the asciidoctod format in the `slides.adoc`
* `asciidoctor-web-pdf` is then used with `--template-require ./template.js` to convert slides to an html document.
* template is manually written -- I don't find presentation frameworks a-la reveal js useful
* the resulting html document is viewable in the browser
* `asciidoctor-web-pdf` use it to convert to PDF via puppeter


```
$ npm install
$ npm run adoc
$ open slides.pdf
```
