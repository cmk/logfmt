{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

{- | Html5 formatting.

 The API is similar to < https://hackage.haskell.org/package/blaze-html >.
-}
module Data.Fmt.Html (
    -- * Html
    Html,
    Attr,
    toHtml,
    comment,
    Element (..),
    (!?),
   
    -- * Elements
    docType,
    docTypeHtml,
    a,
    abbr,
    address,
    area,
    article,
    aside,
    audio,
    b,
    base,
    bdo,
    blockquote,
    body,
    br,
    button,
    canvas,
    caption,
    cite,
    code,
    col,
    colgroup,
    command,
    datalist,
    dd,
    del,
    details,
    dfn,
    div,
    dl,
    dt,
    em,
    embed,
    fieldset,
    figcaption,
    figure,
    footer,
    form,
    h1,
    h2,
    h3,
    h4,
    h5,
    h6,
    head,
    header,
    hgroup,
    hr,
    html,
    i,
    iframe,
    img,
    input,
    ins,
    kbd,
    keygen,
    label,
    legend,
    li,
    link,
    main,
    map,
    mark,
    menu,
    menuitem,
    meta,
    meter,
    nav,
    noscript,
    object,
    ol,
    optgroup,
    option,
    output,
    p,
    param,
    pre,
    progress,
    q,
    rp,
    rt,
    ruby,
    samp,
    script,
    section,
    select,
    small,
    source,
    span,
    strong,
    style,
    sub,
    summary,
    sup,
    table,
    tbody,
    td,
    textarea,
    tfoot,
    th,
    thead,
    time,
    title,
    tr,
    track,
    u,
    ul,
    var,
    video,
    wbr,
) where

import Data.Fmt
import Prelude hiding (div, head, map, span)

import Data.Fmt.Attr (href)

-- | Create a < https://en.wikipedia.org/wiki/HTML_element#Syntax tag > for an element.
element :: String -> Html a -> Html a
element = enclose <$> (enclose "<" ">" . toHtml) <*> (enclose "</" ">" . toHtml)

element_ :: String -> Html a
element_ = enclose "<" " />" . toHtml

-- Elements

-------------------------

{- | Combinator for the document type. This should be placed at the top
 of every HTML page.

 Example:

 > docType

 Result:

 > <!DOCTYPE HTML>
-}
docType ::
    -- | The document type HTML.
    Html a
docType = "<!DOCTYPE HTML>\n"
{-# INLINE docType #-}

{- | Combinator for the @\<html>@ element. This combinator will also
 insert the correct doctype.

 Example:

 > docTypeHtml $ span $ fmt "foo"

 Result:

 > <!DOCTYPE HTML>
 > <html><span>foo</span></html>
-}
docTypeHtml ::
    -- | Inner HTML.
    Html a ->
    -- | Resulting HTML.
    Html a
docTypeHtml inner = docType % html inner
{-# INLINE docTypeHtml #-}

{- | Combinator for the @\<a>@ element.

 Example:

 > a $ span $ fmt "foo"

 Result:

 > <a><span>foo</span></a>
-}
a ::
    -- | Inner HTML.
    Html a ->
    -- | Resulting HTML.
    Html a
a = element "a"
{-# INLINE a #-}

{- | Combinator for the @\<abbr>@ element.

 Example:

 > abbr $ span $ fmt "foo"

 Result:

 > <abbr><span>foo</span></abbr>
-}
abbr ::
    -- | Inner HTML.
    Html a ->
    -- | Resulting HTML.
    Html a
abbr = element "abbr"
{-# INLINE abbr #-}

{- | Combinator for the @\<address>@ element.

 Example:

 > address $ span $ fmt "foo"

 Result:

 > <address><span>foo</span></address>
-}
address ::
    -- | Inner HTML.
    Html a ->
    -- | Resulting HTML.
    Html a
address = element "address"
{-# INLINE address #-}

{- | Combinator for the @\<area />@ element.

 Example:

 > area

 Result:

 > <area />
-}
area ::
    -- | Resulting HTML.
    Html a
area = element_ "area"
{-# INLINE area #-}

{- | Combinator for the @\<article>@ element.

 Example:

 > article $ span $ fmt "foo"

 Result:

 > <article><span>foo</span></article>
-}
article ::
    -- | Inner HTML.
    Html a ->
    -- | Resulting HTML.
    Html a
article = element "article"
{-# INLINE article #-}

{- | Combinator for the @\<aside>@ element.

 Example:

 > aside $ span $ fmt "foo"

 Result:

 > <aside><span>foo</span></aside>
-}
aside ::
    -- | Inner HTML.
    Html a ->
    -- | Resulting HTML.
    Html a
aside = element "aside"
{-# INLINE aside #-}

{- | Combinator for the @\<audio>@ element.

 Example:

 > audio $ span $ fmt "foo"

 Result:

 > <audio><span>foo</span></audio>
-}
audio ::
    -- | Inner HTML.
    Html a ->
    -- | Resulting HTML.
    Html a
audio = element "audio"
{-# INLINE audio #-}

{- | Combinator for the @\<b>@ element.

 Example:

 > b $ span $ fmt "foo"

 Result:

 > <b><span>foo</span></b>
-}
b ::
    -- | Inner HTML.
    Html a ->
    -- | Resulting HTML.
    Html a
b = element "b"
{-# INLINE b #-}

{- | Combinator for the @\<base />@ element.

 Example:

 > base

 Result:

 > <base />
-}
base ::
    -- | Resulting HTML.
    Html a
base = element_ "base"
{-# INLINE base #-}

{- | Combinator for the @\<bdo>@ element.

 Example:

 > bdo $ span $ fmt "foo"

 Result:

 > <bdo><span>foo</span></bdo>
-}
bdo ::
    -- | Inner HTML.
    Html a ->
    -- | Resulting HTML.
    Html a
bdo = element "bdo"
{-# INLINE bdo #-}

{- | Combinator for the @\<blockquote>@ element.

 Example:

 > blockquote $ span $ fmt "foo"

 Result:

 > <blockquote><span>foo</span></blockquote>
-}
blockquote ::
    -- | Inner HTML.
    Html a ->
    -- | Resulting HTML.
    Html a
blockquote = element "blockquote"
{-# INLINE blockquote #-}

{- | Combinator for the @\<body>@ element.

 Example:

 > body $ span $ fmt "foo"

 Result:

 > <body><span>foo</span></body>
-}
body ::
    -- | Inner HTML.
    Html a ->
    -- | Resulting HTML.
    Html a
body = element "body"
{-# INLINE body #-}

{- | Combinator for the @\<br />@ element.

 Example:

 > br

 Result:

 > <br />
-}
br ::
    -- | Resulting HTML.
    Html a
br = element_ "br"
{-# INLINE br #-}

{- | Combinator for the @\<button>@ element.

 Example:

 > button $ span $ fmt "foo"

 Result:

 > <button><span>foo</span></button>
-}
button ::
    -- | Inner HTML.
    Html a ->
    -- | Resulting HTML.
    Html a
button = element "button"
{-# INLINE button #-}

{- | Combinator for the @\<canvas>@ element.

 Example:

 > canvas $ span $ fmt "foo"

 Result:

 > <canvas><span>foo</span></canvas>
-}
canvas ::
    -- | Inner HTML.
    Html a ->
    -- | Resulting HTML.
    Html a
canvas = element "canvas"
{-# INLINE canvas #-}

{- | Combinator for the @\<caption>@ element.

 Example:

 > caption $ span $ fmt "foo"

 Result:

 > <caption><span>foo</span></caption>
-}
caption ::
    -- | Inner HTML.
    Html a ->
    -- | Resulting HTML.
    Html a
caption = element "caption"
{-# INLINE caption #-}

{- | Combinator for the @\<cite>@ element.

 Example:

 > cite $ span $ fmt "foo"

 Result:

 > <cite><span>foo</span></cite>
-}
cite ::
    -- | Inner HTML.
    Html a ->
    -- | Resulting HTML.
    Html a
cite = element "cite"
{-# INLINE cite #-}

{- | Combinator for the @\<code>@ element.

 Example:

 > code $ span $ fmt "foo"

 Result:

 > <code><span>foo</span></code>
-}
code ::
    -- | Inner HTML.
    Html a ->
    -- | Resulting HTML.
    Html a
code = element "code"
{-# INLINE code #-}

{- | Combinator for the @\<col />@ element.

 Example:

 > col

 Result:

 > <col />
-}
col ::
    -- | Resulting HTML.
    Html a
col = element_ "col"
{-# INLINE col #-}

{- | Combinator for the @\<colgroup>@ element.

 Example:

 > colgroup $ span $ fmt "foo"

 Result:

 > <colgroup><span>foo</span></colgroup>
-}
colgroup ::
    -- | Inner HTML.
    Html a ->
    -- | Resulting HTML.
    Html a
colgroup = element "colgroup"
{-# INLINE colgroup #-}

{- | Combinator for the @\<command>@ element.

 Example:

 > command $ span $ fmt "foo"

 Result:

 > <command><span>foo</span></command>
-}
command ::
    -- | Inner HTML.
    Html a ->
    -- | Resulting HTML.
    Html a
command = element "command"
{-# INLINE command #-}

{- | Combinator for the @\<datalist>@ element.

 Example:

 > datalist $ span $ fmt "foo"

 Result:

 > <datalist><span>foo</span></datalist>
-}
datalist ::
    -- | Inner HTML.
    Html a ->
    -- | Resulting HTML.
    Html a
datalist = element "datalist"
{-# INLINE datalist #-}

{- | Combinator for the @\<dd>@ element.

 Example:

 > dd $ span $ fmt "foo"

 Result:

 > <dd><span>foo</span></dd>
-}
dd ::
    -- | Inner HTML.
    Html a ->
    -- | Resulting HTML.
    Html a
dd = element "dd"
{-# INLINE dd #-}

{- | Combinator for the @\<del>@ element.

 Example:

 > del $ span $ fmt "foo"

 Result:

 > <del><span>foo</span></del>
-}
del ::
    -- | Inner HTML.
    Html a ->
    -- | Resulting HTML.
    Html a
del = element "del"
{-# INLINE del #-}

{- | Combinator for the @\<details>@ element.

 Example:

 > details $ span $ fmt "foo"

 Result:

 > <details><span>foo</span></details>
-}
details ::
    -- | Inner HTML.
    Html a ->
    -- | Resulting HTML.
    Html a
details = element "details"
{-# INLINE details #-}

{- | Combinator for the @\<dfn>@ element.

 Example:

 > dfn $ span $ fmt "foo"

 Result:

 > <dfn><span>foo</span></dfn>
-}
dfn ::
    -- | Inner HTML.
    Html a ->
    -- | Resulting HTML.
    Html a
dfn = element "dfn"
{-# INLINE dfn #-}

{- | Combinator for the @\<div>@ element.

 Example:

 > div $ span $ fmt "foo"

 Result:

 > <div><span>foo</span></div>
-}
div ::
    -- | Inner HTML.
    Html a ->
    -- | Resulting HTML.
    Html a
div = element "div"
{-# INLINE div #-}

{- | Combinator for the @\<dl>@ element.

 Example:

 > dl $ span $ fmt "foo"

 Result:

 > <dl><span>foo</span></dl>
-}
dl ::
    -- | Inner HTML.
    Html a ->
    -- | Resulting HTML.
    Html a
dl = element "dl"
{-# INLINE dl #-}

{- | Combinator for the @\<dt>@ element.

 Example:

 > dt $ span $ fmt "foo"

 Result:

 > <dt><span>foo</span></dt>
-}
dt ::
    -- | Inner HTML.
    Html a ->
    -- | Resulting HTML.
    Html a
dt = element "dt"
{-# INLINE dt #-}

{- | Combinator for the @\<em>@ element.

 Example:

 > em $ span $ fmt "foo"

 Result:

 > <em><span>foo</span></em>
-}
em ::
    -- | Inner HTML.
    Html a ->
    -- | Resulting HTML.
    Html a
em = element "em"
{-# INLINE em #-}

{- | Combinator for the @\<embed />@ element.

 Example:

 > embed

 Result:

 > <embed />
-}
embed ::
    -- | Resulting HTML.
    Html a
embed = element_ "embed"
{-# INLINE embed #-}

{- | Combinator for the @\<fieldset>@ element.

 Example:

 > fieldset $ span $ fmt "foo"

 Result:

 > <fieldset><span>foo</span></fieldset>
-}
fieldset ::
    -- | Inner HTML.
    Html a ->
    -- | Resulting HTML.
    Html a
fieldset = element "fieldset"
{-# INLINE fieldset #-}

{- | Combinator for the @\<figcaption>@ element.

 Example:

 > figcaption $ span $ fmt "foo"

 Result:

 > <figcaption><span>foo</span></figcaption>
-}
figcaption ::
    -- | Inner HTML.
    Html a ->
    -- | Resulting HTML.
    Html a
figcaption = element "figcaption"
{-# INLINE figcaption #-}

{- | Combinator for the @\<figure>@ element.

 Example:

 > figure $ span $ fmt "foo"

 Result:

 > <figure><span>foo</span></figure>
-}
figure ::
    -- | Inner HTML.
    Html a ->
    -- | Resulting HTML.
    Html a
figure = element "figure"
{-# INLINE figure #-}

{- | Combinator for the @\<footer>@ element.

 Example:

 > footer $ span $ fmt "foo"

 Result:

 > <footer><span>foo</span></footer>
-}
footer ::
    -- | Inner HTML.
    Html a ->
    -- | Resulting HTML.
    Html a
footer = element "footer"
{-# INLINE footer #-}

{- | Combinator for the @\<form>@ element.

 Example:

 > form $ span $ fmt "foo"

 Result:

 > <form><span>foo</span></form>
-}
form ::
    -- | Inner HTML.
    Html a ->
    -- | Resulting HTML.
    Html a
form = element "form"
{-# INLINE form #-}

{- | Combinator for the @\<h1>@ element.

 Example:

 > h1 $ span $ fmt "foo"

 Result:

 > <h1><span>foo</span></h1>
-}
h1 ::
    -- | Inner HTML.
    Html a ->
    -- | Resulting HTML.
    Html a
h1 = element "h1"
{-# INLINE h1 #-}

{- | Combinator for the @\<h2>@ element.

 Example:

 > h2 $ span $ fmt "foo"

 Result:

 > <h2><span>foo</span></h2>
-}
h2 ::
    -- | Inner HTML.
    Html a ->
    -- | Resulting HTML.
    Html a
h2 = element "h2"
{-# INLINE h2 #-}

{- | Combinator for the @\<h3>@ element.

 Example:

 > h3 $ span $ fmt "foo"

 Result:

 > <h3><span>foo</span></h3>
-}
h3 ::
    -- | Inner HTML.
    Html a ->
    -- | Resulting HTML.
    Html a
h3 = element "h3"
{-# INLINE h3 #-}

{- | Combinator for the @\<h4>@ element.

 Example:

 > h4 $ span $ fmt "foo"

 Result:

 > <h4><span>foo</span></h4>
-}
h4 ::
    -- | Inner HTML.
    Html a ->
    -- | Resulting HTML.
    Html a
h4 = element "h4"
{-# INLINE h4 #-}

{- | Combinator for the @\<h5>@ element.

 Example:

 > h5 $ span $ fmt "foo"

 Result:

 > <h5><span>foo</span></h5>
-}
h5 ::
    -- | Inner HTML.
    Html a ->
    -- | Resulting HTML.
    Html a
h5 = element "h5"
{-# INLINE h5 #-}

{- | Combinator for the @\<h6>@ element.

 Example:

 > h6 $ span $ fmt "foo"

 Result:

 > <h6><span>foo</span></h6>
-}
h6 ::
    -- | Inner HTML.
    Html a ->
    -- | Resulting HTML.
    Html a
h6 = element "h6"
{-# INLINE h6 #-}

{- | Combinator for the @\<head>@ element.

 Example:

 > head $ span $ fmt "foo"

 Result:

 > <head><span>foo</span></head>
-}
head ::
    -- | Inner HTML.
    Html a ->
    -- | Resulting HTML.
    Html a
head = element "head"
{-# INLINE head #-}

{- | Combinator for the @\<header>@ element.

 Example:

 > header $ span $ fmt "foo"

 Result:

 > <header><span>foo</span></header>
-}
header ::
    -- | Inner HTML.
    Html a ->
    -- | Resulting HTML.
    Html a
header = element "header"
{-# INLINE header #-}

{- | Combinator for the @\<hgroup>@ element.

 Example:

 > hgroup $ span $ fmt "foo"

 Result:

 > <hgroup><span>foo</span></hgroup>
-}
hgroup ::
    -- | Inner HTML.
    Html a ->
    -- | Resulting HTML.
    Html a
hgroup = element "hgroup"
{-# INLINE hgroup #-}

{- | Combinator for the @\<hr />@ element.

 Example:

 > hr

 Result:

 > <hr />
-}
hr ::
    -- | Resulting HTML.
    Html a
hr = element_ "hr"
{-# INLINE hr #-}

{- | Combinator for the @\<html>@ element.

 Example:

 > html $ span $ fmt "foo"

 Result:

 > <html><span>foo</span></html>
-}
html ::
    -- | Inner HTML.
    Html a ->
    -- | Resulting HTML.
    Html a
html = element "html"
{-# INLINE html #-}

{- | Combinator for the @\<i>@ element.

 Example:

 > i $ span $ fmt "foo"

 Result:

 > <i><span>foo</span></i>
-}
i ::
    -- | Inner HTML.
    Html a ->
    -- | Resulting HTML.
    Html a
i = element "i"
{-# INLINE i #-}

{- | Combinator for the @\<iframe>@ element.

 Example:

 > iframe $ span $ fmt "foo"

 Result:

 > <iframe><span>foo</span></iframe>
-}
iframe ::
    -- | Inner HTML.
    Html a ->
    -- | Resulting HTML.
    Html a
iframe = element "iframe"
{-# INLINE iframe #-}

{- | Combinator for the @\<img />@ element.

 Example:

 > img

 Result:

 > <img />
-}
img ::
    -- | Resulting HTML.
    Html a
img = element_ "img"
{-# INLINE img #-}

{- | Combinator for the @\<input />@ element.

 Example:

 > input

 Result:

 > <input />
-}
input ::
    -- | Resulting HTML.
    Html a
input = element_ "input"
{-# INLINE input #-}

{- | Combinator for the @\<ins>@ element.

 Example:

 > ins $ span $ fmt "foo"

 Result:

 > <ins><span>foo</span></ins>
-}
ins ::
    -- | Inner HTML.
    Html a ->
    -- | Resulting HTML.
    Html a
ins = element "ins"
{-# INLINE ins #-}

{- | Combinator for the @\<kbd>@ element.

 Example:

 > kbd $ span $ fmt "foo"

 Result:

 > <kbd><span>foo</span></kbd>
-}
kbd ::
    -- | Inner HTML.
    Html a ->
    -- | Resulting HTML.
    Html a
kbd = element "kbd"
{-# INLINE kbd #-}

{- | Combinator for the @\<keygen />@ element.

 Example:

 > keygen

 Result:

 > <keygen />
-}
keygen ::
    -- | Resulting HTML.
    Html a
keygen = element_ "keygen"
{-# INLINE keygen #-}

{- | Combinator for the @\<label>@ element.

 Example:

 > label $ span $ fmt "foo"

 Result:

 > <label><span>foo</span></label>
-}
label ::
    -- | Inner HTML.
    Html a ->
    -- | Resulting HTML.
    Html a
label = element "label"
{-# INLINE label #-}

{- | Combinator for the @\<legend>@ element.

 Example:

 > legend $ span $ fmt "foo"

 Result:

 > <legend><span>foo</span></legend>
-}
legend ::
    -- | Inner HTML.
    Html a ->
    -- | Resulting HTML.
    Html a
legend = element "legend"
{-# INLINE legend #-}

{- | Combinator for the @\<li>@ element.

 Example:

 > li $ span $ fmt "foo"

 Result:

 > <li><span>foo</span></li>
-}
li ::
    -- | Inner HTML.
    Html a ->
    -- | Resulting HTML.
    Html a
li = element "li"
{-# INLINE li #-}

{- | Combinator for the @\<link />@ element.

 Example:

 > link

 Result:

 > <link />
-}
link ::
    -- | Resulting HTML.
    Html a
link = element_ "link"
{-# INLINE link #-}

{- | Combinator for the @\<main>@ element.

 Example:

 > main $ span $ fmt "foo"

 Result:

 > <main><span>foo</span></main>
-}
main ::
    -- | Inner HTML.
    Html a ->
    -- | Resulting HTML.
    Html a
main = element "main"
{-# INLINE main #-}

{- | Combinator for the @\<map>@ element.

 Example:

 > map $ span $ fmt "foo"

 Result:

 > <map><span>foo</span></map>
-}
map ::
    -- | Inner HTML.
    Html a ->
    -- | Resulting HTML.
    Html a
map = element "map"
{-# INLINE map #-}

{- | Combinator for the @\<mark>@ element.

 Example:

 > mark $ span $ fmt "foo"

 Result:

 > <mark><span>foo</span></mark>
-}
mark ::
    -- | Inner HTML.
    Html a ->
    -- | Resulting HTML.
    Html a
mark = element "mark"
{-# INLINE mark #-}

{- | Combinator for the @\<menu>@ element.

 Example:

 > menu $ span $ fmt "foo"

 Result:

 > <menu><span>foo</span></menu>
-}
menu ::
    -- | Inner HTML.
    Html a ->
    -- | Resulting HTML.
    Html a
menu = element "menu"
{-# INLINE menu #-}

{- | Combinator for the @\<menuitem />@ element.

 Example:

 > menuitem

 Result:

 > <menuitem />
-}
menuitem ::
    -- | Resulting HTML.
    Html a
menuitem = element_ "menuitem"
{-# INLINE menuitem #-}

{- | Combinator for the @\<meta />@ element.

 Example:

 > meta

 Result:

 > <meta />
-}
meta ::
    -- | Resulting HTML.
    Html a
meta = element_ "meta"
{-# INLINE meta #-}

{- | Combinator for the @\<meter>@ element.

 Example:

 > meter $ span $ fmt "foo"

 Result:

 > <meter><span>foo</span></meter>
-}
meter ::
    -- | Inner HTML.
    Html a ->
    -- | Resulting HTML.
    Html a
meter = element "meter"
{-# INLINE meter #-}

{- | Combinator for the @\<nav>@ element.

 Example:

 > nav $ span $ fmt "foo"

 Result:

 > <nav><span>foo</span></nav>
-}
nav ::
    -- | Inner HTML.
    Html a ->
    -- | Resulting HTML.
    Html a
nav = element "nav"
{-# INLINE nav #-}

{- | Combinator for the @\<noscript>@ element.

 Example:

 > noscript $ span $ fmt "foo"

 Result:

 > <noscript><span>foo</span></noscript>
-}
noscript ::
    -- | Inner HTML.
    Html a ->
    -- | Resulting HTML.
    Html a
noscript = element "noscript"
{-# INLINE noscript #-}

{- | Combinator for the @\<object>@ element.

 Example:

 > object $ span $ fmt "foo"

 Result:

 > <object><span>foo</span></object>
-}
object ::
    -- | Inner HTML.
    Html a ->
    -- | Resulting HTML.
    Html a
object = element "object"
{-# INLINE object #-}

{- | Combinator for the @\<ol>@ element.

 Example:

 > ol $ span $ fmt "foo"

 Result:

 > <ol><span>foo</span></ol>
-}
ol ::
    -- | Inner HTML.
    Html a ->
    -- | Resulting HTML.
    Html a
ol = element "ol"
{-# INLINE ol #-}

{- | Combinator for the @\<optgroup>@ element.

 Example:

 > optgroup $ span $ fmt "foo"

 Result:

 > <optgroup><span>foo</span></optgroup>
-}
optgroup ::
    -- | Inner HTML.
    Html a ->
    -- | Resulting HTML.
    Html a
optgroup = element "optgroup"
{-# INLINE optgroup #-}

{- | Combinator for the @\<option>@ element.

 Example:

 > option $ span $ fmt "foo"

 Result:

 > <option><span>foo</span></option>
-}
option ::
    -- | Inner HTML.
    Html a ->
    -- | Resulting HTML.
    Html a
option = element "option"
{-# INLINE option #-}

{- | Combinator for the @\<output>@ element.

 Example:

 > output $ span $ fmt "foo"

 Result:

 > <output><span>foo</span></output>
-}
output ::
    -- | Inner HTML.
    Html a ->
    -- | Resulting HTML.
    Html a
output = element "output"
{-# INLINE output #-}

{- | Combinator for the @\<p>@ element.

 Example:

 > p $ span $ fmt "foo"

 Result:

 > <p><span>foo</span></p>
-}
p ::
    -- | Inner HTML.
    Html a ->
    -- | Resulting HTML.
    Html a
p = element "p"
{-# INLINE p #-}

{- | Combinator for the @\<param />@ element.

 Example:

 > param

 Result:

 > <param />
-}
param ::
    -- | Resulting HTML.
    Html a
param = element_ "param"
{-# INLINE param #-}

{- | Combinator for the @\<pre>@ element.

 Example:

 > pre $ span $ fmt "foo"

 Result:

 > <pre><span>foo</span></pre>
-}
pre ::
    -- | Inner HTML.
    Html a ->
    -- | Resulting HTML.
    Html a
pre = element "pre"
{-# INLINE pre #-}

{- | Combinator for the @\<progress>@ element.

 Example:

 > progress $ span $ fmt "foo"

 Result:

 > <progress><span>foo</span></progress>
-}
progress ::
    -- | Inner HTML.
    Html a ->
    -- | Resulting HTML.
    Html a
progress = element "progress"
{-# INLINE progress #-}

{- | Combinator for the @\<q>@ element.

 Example:

 > q $ span $ fmt "foo"

 Result:

 > <q><span>foo</span></q>
-}
q ::
    -- | Inner HTML.
    Html a ->
    -- | Resulting HTML.
    Html a
q = element "q"
{-# INLINE q #-}

{- | Combinator for the @\<rp>@ element.

 Example:

 > rp $ span $ fmt "foo"

 Result:

 > <rp><span>foo</span></rp>
-}
rp ::
    -- | Inner HTML.
    Html a ->
    -- | Resulting HTML.
    Html a
rp = element "rp"
{-# INLINE rp #-}

{- | Combinator for the @\<rt>@ element.

 Example:

 > rt $ span $ fmt "foo"

 Result:

 > <rt><span>foo</span></rt>
-}
rt ::
    -- | Inner HTML.
    Html a ->
    -- | Resulting HTML.
    Html a
rt = element "rt"
{-# INLINE rt #-}

{- | Combinator for the @\<ruby>@ element.

 Example:

 > ruby $ span $ fmt "foo"

 Result:

 > <ruby><span>foo</span></ruby>
-}
ruby ::
    -- | Inner HTML.
    Html a ->
    -- | Resulting HTML.
    Html a
ruby = element "ruby"
{-# INLINE ruby #-}

{- | Combinator for the @\<samp>@ element.

 Example:

 > samp $ span $ fmt "foo"

 Result:

 > <samp><span>foo</span></samp>
-}
samp ::
    -- | Inner HTML.
    Html a ->
    -- | Resulting HTML.
    Html a
samp = element "samp"
{-# INLINE samp #-}

{- | Combinator for the @\<script>@ element.

 Example:

 > script $ span $ fmt "foo"

 Result:

 > <script><span>foo</span></script>
-}
script ::
    -- | Inner HTML.
    Html a ->
    -- | Resulting HTML.
    Html a
script = element "script"
{-# INLINE script #-}

{- | Combinator for the @\<section>@ element.

 Example:

 > section $ span $ fmt "foo"

 Result:

 > <section><span>foo</span></section>
-}
section ::
    -- | Inner HTML.
    Html a ->
    -- | Resulting HTML.
    Html a
section = element "section"
{-# INLINE section #-}

{- | Combinator for the @\<select>@ element.

 Example:

 > select $ span $ fmt "foo"

 Result:

 > <select><span>foo</span></select>
-}
select ::
    -- | Inner HTML.
    Html a ->
    -- | Resulting HTML.
    Html a
select = element "select"
{-# INLINE select #-}

{- | Combinator for the @\<small>@ element.

 Example:

 > small $ span $ fmt "foo"

 Result:

 > <small><span>foo</span></small>
-}
small ::
    -- | Inner HTML.
    Html a ->
    -- | Resulting HTML.
    Html a
small = element "small"
{-# INLINE small #-}

{- | Combinator for the @\<source />@ element.

 Example:

 > source

 Result:

 > <source />
-}
source ::
    -- | Resulting HTML.
    Html a
source = element_ "source"
{-# INLINE source #-}

{- | Combinator for the @\<span>@ element.

 Example:

 > span $ span $ fmt "foo"

 Result:

 > <span><span>foo</span></span>
-}
span ::
    -- | Inner HTML.
    Html a ->
    -- | Resulting HTML.
    Html a
span = element "span"
{-# INLINE span #-}

{- | Combinator for the @\<strong>@ element.

 Example:

 > strong $ span $ fmt "foo"

 Result:

 > <strong><span>foo</span></strong>
-}
strong ::
    -- | Inner HTML.
    Html a ->
    -- | Resulting HTML.
    Html a
strong = element "strong"
{-# INLINE strong #-}

{- | Combinator for the @\<style>@ element.

 Example:

 > style $ span $ fmt "foo"

 Result:

 > <style><span>foo</span></style>
-}
style ::
    -- | Inner HTML.
    Html a ->
    -- | Resulting HTML.
    Html a
style = element "style"
{-# INLINE style #-}

{- | Combinator for the @\<sub>@ element.

 Example:

 > sub $ span $ fmt "foo"

 Result:

 > <sub><span>foo</span></sub>
-}
sub ::
    -- | Inner HTML.
    Html a ->
    -- | Resulting HTML.
    Html a
sub = element "sub"
{-# INLINE sub #-}

{- | Combinator for the @\<summary>@ element.

 Example:

 > summary $ span $ fmt "foo"

 Result:

 > <summary><span>foo</span></summary>
-}
summary ::
    -- | Inner HTML.
    Html a ->
    -- | Resulting HTML.
    Html a
summary = element "summary"
{-# INLINE summary #-}

{- | Combinator for the @\<sup>@ element.

 Example:

 > sup $ span $ fmt "foo"

 Result:

 > <sup><span>foo</span></sup>
-}
sup ::
    -- | Inner HTML.
    Html a ->
    -- | Resulting HTML.
    Html a
sup = element "sup"
{-# INLINE sup #-}

{- | Combinator for the @\<table>@ element.

 Example:

 > table $ span $ fmt "foo"

 Result:

 > <table><span>foo</span></table>
-}
table ::
    -- | Inner HTML.
    Html a ->
    -- | Resulting HTML.
    Html a
table = element "table"
{-# INLINE table #-}

{- | Combinator for the @\<tbody>@ element.

 Example:

 > tbody $ span $ fmt "foo"

 Result:

 > <tbody><span>foo</span></tbody>
-}
tbody ::
    -- | Inner HTML.
    Html a ->
    -- | Resulting HTML.
    Html a
tbody = element "tbody"
{-# INLINE tbody #-}

{- | Combinator for the @\<td>@ element.

 Example:

 > td $ span $ fmt "foo"

 Result:

 > <td><span>foo</span></td>
-}
td ::
    -- | Inner HTML.
    Html a ->
    -- | Resulting HTML.
    Html a
td = element "td"
{-# INLINE td #-}

{- | Combinator for the @\<textarea>@ element.

 Example:

 > textarea $ span $ fmt "foo"

 Result:

 > <textarea><span>foo</span></textarea>
-}
textarea ::
    -- | Inner HTML.
    Html a ->
    -- | Resulting HTML.
    Html a
textarea = element "textarea"
{-# INLINE textarea #-}

{- | Combinator for the @\<tfoot>@ element.

 Example:

 > tfoot $ span $ fmt "foo"

 Result:

 > <tfoot><span>foo</span></tfoot>
-}
tfoot ::
    -- | Inner HTML.
    Html a ->
    -- | Resulting HTML.
    Html a
tfoot = element "tfoot"
{-# INLINE tfoot #-}

{- | Combinator for the @\<th>@ element.

 Example:

 > th $ span $ fmt "foo"

 Result:

 > <th><span>foo</span></th>
-}
th ::
    -- | Inner HTML.
    Html a ->
    -- | Resulting HTML.
    Html a
th = element "th"
{-# INLINE th #-}

{- | Combinator for the @\<thead>@ element.

 Example:

 > thead $ span $ fmt "foo"

 Result:

 > <thead><span>foo</span></thead>
-}
thead ::
    -- | Inner HTML.
    Html a ->
    -- | Resulting HTML.
    Html a
thead = element "thead"
{-# INLINE thead #-}

{- | Combinator for the @\<time>@ element.

 Example:

 > time $ span $ fmt "foo"

 Result:

 > <time><span>foo</span></time>
-}
time ::
    -- | Inner HTML.
    Html a ->
    -- | Resulting HTML.
    Html a
time = element "time"
{-# INLINE time #-}

{- | Combinator for the @\<title>@ element.

 Example:

 > title $ span $ fmt "foo"

 Result:

 > <title><span>foo</span></title>
-}
title ::
    -- | Inner HTML.
    Html a ->
    -- | Resulting HTML.
    Html a
title = element "title"
{-# INLINE title #-}

{- | Combinator for the @\<tr>@ element.

 Example:

 > tr $ span $ fmt "foo"

 Result:

 > <tr><span>foo</span></tr>
-}
tr ::
    -- | Inner HTML.
    Html a ->
    -- | Resulting HTML.
    Html a
tr = element "tr"
{-# INLINE tr #-}

{- | Combinator for the @\<track />@ element.

 Example:

 > track

 Result:

 > <track />
-}
track ::
    -- | Resulting HTML.
    Html a
track = element_ "track"
{-# INLINE track #-}

{- | Combinator for the @\<u>@ element.

 Example:

 > u $ span $ fmt "foo"

 Result:

 > <u><span>foo</span></u>
-}
u ::
    -- | Inner HTML.
    Html a ->
    -- | Resulting HTML.
    Html a
u = element "u"
{-# INLINE u #-}

{- | Combinator for the @\<ul>@ element.

 Example:

 > ul $ span $ fmt "foo"

 Result:

 > <ul><span>foo</span></ul>
-}
ul ::
    -- | Inner HTML.
    Html a ->
    -- | Resulting HTML.
    Html a
ul = element "ul"
{-# INLINE ul #-}

{- | Combinator for the @\<var>@ element.

 Example:

 > var $ span $ fmt "foo"

 Result:

 > <var><span>foo</span></var>
-}
var ::
    -- | Inner HTML.
    Html a ->
    -- | Resulting HTML.
    Html a
var = element "var"
{-# INLINE var #-}

{- | Combinator for the @\<video>@ element.

 Example:

 > video $ span $ fmt "foo"

 Result:

 > <video><span>foo</span></video>
-}
video ::
    -- | Inner HTML.
    Html a ->
    -- | Resulting HTML.
    Html a
video = element "video"
{-# INLINE video #-}

{- | Combinator for the @\<wbr />@ element.

 Example:

 > wbr

 Result:

 > <wbr />
-}
wbr :: Html a
wbr = element_ "wbr"
{-# INLINE wbr #-}
