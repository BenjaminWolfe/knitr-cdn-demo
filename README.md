# Knitr CDN Demo
## What This Repo Is

This repo goes with the blog post
[R Markdown: Shrink & Tidy Your HTML Output with CDNs][1].

It helps illustrate the file bloat caused by inline JavaScript + CSS code,
what that means for diffing your HTML output,
and how you can externalize that code
and employ Content Delivery Networks (CDNs).

## How to Use It

The repo has two important files:

* [cdnify.R](../blob/master/cdnify.R)
* [pre-commit](../blob/master/pre-commit)

Place the first anywhere in your repository,
and follow the commented instructions for the second.

Then in the YAML of any R Markdown file,
specify the output format like this:

```yaml
output:
  html_document:
    self_contained: false
```

You can run `cdnify.R` manually, or just let it run automatically
every time you commit changes to your repo.

## Final Note

You're likely used to your R Markdown outputting a single HTML file.
For example, `report.Rmd` becomes `report.html`.
Setting `self_contained` to `false` means
you'll get an additional folder named after the report as well,
e.g. `report_files`.

That folder needs to stay with the HTML file
if you're attaching it to an email
or uploading it to a shared drive.

[1]: https://benjamin-wolfe.netlify.com/post/2020-03-14-r-markdown-shrink-tidy-your-html-output-with-cdns/