{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ViewPatterns #-}

{- | Html5 formatting.

 The API is similar to < https://hackage.haskell.org/package/blaze-html >.
-}
module Data.Fmt.Attr (
    -- * Attributes
      accept
    , acceptCharset
    , accesskey
    , action
    , alt
    , async
    , autocomplete
    , autofocus
    , autoplay
    , challenge
    , charset
    , checked
    , cite
    , class_
    , cols
    , colspan
    , content
    , contenteditable
    , contextmenu
    , controls
    , coords
    , data_
    , datetime
    , defer
    , dir
    , disabled
    , draggable
    , enctype
    , for
    , form
    , formaction
    , formenctype
    , formmethod
    , formnovalidate
    , formtarget
    , headers
    , height
    , hidden
    , high
    , href
    , hreflang
    , httpEquiv
    , icon
    , id
    , ismap
    , item
    , itemprop
    , itemscope
    , itemtype
    , keytype
    , label
    , lang
    , list
    , loop
    , low
    , manifest
    , max
    , maxlength
    , media
    , method
    , min
    , multiple
    , name
    , novalidate
    , onbeforeonload
    , onbeforeprint
    , onblur
    , oncanplay
    , oncanplaythrough
    , onchange
    , onclick
    , oncontextmenu
    , ondblclick
    , ondrag
    , ondragend
    , ondragenter
    , ondragleave
    , ondragover
    , ondragstart
    , ondrop
    , ondurationchange
    , onemptied
    , onended
    , onerror
    , onfocus
    , onformchange
    , onforminput
    , onhaschange
    , oninput
    , oninvalid
    , onkeydown
    , onkeyup
    , onload
    , onloadeddata
    , onloadedmetadata
    , onloadstart
    , onmessage
    , onmousedown
    , onmousemove
    , onmouseout
    , onmouseover
    , onmouseup
    , onmousewheel
    , ononline
    , onpagehide
    , onpageshow
    , onpause
    , onplay
    , onplaying
    , onprogress
    , onpropstate
    , onratechange
    , onreadystatechange
    , onredo
    , onresize
    , onscroll
    , onseeked
    , onseeking
    , onselect
    , onstalled
    , onstorage
    , onsubmit
    , onsuspend
    , ontimeupdate
    , onundo
    , onunload
    , onvolumechange
    , onwaiting
    , open
    , optimum
    , pattern
    , ping
    , placeholder
    , preload
    , pubdate
    , radiogroup
    , readonly
    , rel
    , required
    , reversed
    , role
    , rows
    , rowspan
    , sandbox
    , scope
    , scoped
    , seamless
    , selected
    , shape
    , size
    , sizes
    , span
    , spellcheck
    , src
    , srcdoc
    , start
    , step
    , style
    , subject
    , summary
    , tabindex
    , target
    , title
    , type_
    , usemap
    , value
    , width
    , wrap
    , xmlns
) where

import Prelude ()
import Data.Fmt (ToLogStr)
import Data.Fmt.Html (attr, Attr)

-- Attributes

-------------------------

{- | Combinator for the @accept@ attribute.

 Example:

 > div ! accept "bar" $ "Hello."

 Result:

 > <div accept="bar">Hello.</div>
-}
accept ::
    ToLogStr s =>
    -- | Attribute value.
    s ->
    -- | Resulting attribute.
    Attr
accept = attr "accept"
{-# INLINE accept #-}

{- | Combinator for the @accept-charset@ attribute.

 Example:

 > div ! acceptCharset "bar" $ "Hello."

 Result:

 > <div accept-charset="bar">Hello.</div>
-}
acceptCharset ::
    ToLogStr s =>
    -- | Attribute value.
    s ->
    -- | Resulting attribute.
    Attr
acceptCharset = attr "accept-charset"
{-# INLINE acceptCharset #-}

{- | Combinator for the @accesskey@ attribute.

 Example:

 > div ! accesskey "bar" $ "Hello."

 Result:

 > <div accesskey="bar">Hello.</div>
-}
accesskey ::
    ToLogStr s =>
    -- | Attribute value.
    s ->
    -- | Resulting attribute.
    Attr
accesskey = attr "accesskey"
{-# INLINE accesskey #-}

{- | Combinator for the @action@ attribute.

 Example:

 > div ! action "bar" $ "Hello."

 Result:

 > <div action="bar">Hello.</div>
-}
action ::
    ToLogStr s =>
    -- | Attribute value.
    s ->
    -- | Resulting attribute.
    Attr
action = attr "action"
{-# INLINE action #-}

{- | Combinator for the @alt@ attribute.

 Example:

 > div ! alt "bar" $ "Hello."

 Result:

 > <div alt="bar">Hello.</div>
-}
alt ::
    ToLogStr s =>
    -- | Attribute value.
    s ->
    -- | Resulting attribute.
    Attr
alt = attr "alt"
{-# INLINE alt #-}

{- | Combinator for the @async@ attribute.

 Example:

 > div ! async "bar" $ "Hello."

 Result:

 > <div async="bar">Hello.</div>
-}
async ::
    ToLogStr s =>
    -- | Attribute value.
    s ->
    -- | Resulting attribute.
    Attr
async = attr "async"
{-# INLINE async #-}

{- | Combinator for the @autocomplete@ attribute.

 Example:

 > div ! autocomplete "bar" $ "Hello."

 Result:

 > <div autocomplete="bar">Hello.</div>
-}
autocomplete ::
    ToLogStr s =>
    -- | Attribute value.
    s ->
    -- | Resulting attribute.
    Attr
autocomplete = attr "autocomplete"
{-# INLINE autocomplete #-}

{- | Combinator for the @autofocus@ attribute.

 Example:

 > div ! autofocus "bar" $ "Hello."

 Result:

 > <div autofocus="bar">Hello.</div>
-}
autofocus ::
    ToLogStr s =>
    -- | Attribute value.
    s ->
    -- | Resulting attribute.
    Attr
autofocus = attr "autofocus"
{-# INLINE autofocus #-}

{- | Combinator for the @autoplay@ attribute.

 Example:

 > div ! autoplay "bar" $ "Hello."

 Result:

 > <div autoplay="bar">Hello.</div>
-}
autoplay ::
    ToLogStr s =>
    -- | Attribute value.
    s ->
    -- | Resulting attribute.
    Attr
autoplay = attr "autoplay"
{-# INLINE autoplay #-}

{- | Combinator for the @challenge@ attribute.

 Example:

 > div ! challenge "bar" $ "Hello."

 Result:

 > <div challenge="bar">Hello.</div>
-}
challenge ::
    ToLogStr s =>
    -- | Attribute value.
    s ->
    -- | Resulting attribute.
    Attr
challenge = attr "challenge"
{-# INLINE challenge #-}

{- | Combinator for the @charset@ attribute.

 Example:

 > div ! charset "bar" $ "Hello."

 Result:

 > <div charset="bar">Hello.</div>
-}
charset ::
    ToLogStr s =>
    -- | Attribute value.
    s ->
    -- | Resulting attribute.
    Attr
charset = attr "charset"
{-# INLINE charset #-}

{- | Combinator for the @checked@ attribute.

 Example:

 > div ! checked "bar" $ "Hello."

 Result:

 > <div checked="bar">Hello.</div>
-}
checked ::
    ToLogStr s =>
    -- | Attribute value.
    s ->
    -- | Resulting attribute.
    Attr
checked = attr "checked"
{-# INLINE checked #-}

{- | Combinator for the @cite@ attribute.

 Example:

 > div ! cite "bar" $ "Hello."

 Result:

 > <div cite="bar">Hello.</div>
-}
cite ::
    ToLogStr s =>
    -- | Attribute value.
    s ->
    -- | Resulting attribute.
    Attr
cite = attr "cite"
{-# INLINE cite #-}

{- | Combinator for the @class@ attribute.

 Example:

 > div ! class_ "bar" $ "Hello."

 Result:

 > <div class="bar">Hello.</div>
-}
class_ ::
    ToLogStr s =>
    -- | Attribute value.
    s ->
    -- | Resulting attribute.
    Attr
class_ = attr "class"
{-# INLINE class_ #-}

{- | Combinator for the @cols@ attribute.

 Example:

 > div ! cols "bar" $ "Hello."

 Result:

 > <div cols="bar">Hello.</div>
-}
cols ::
    ToLogStr s =>
    -- | Attribute value.
    s ->
    -- | Resulting attribute.
    Attr
cols = attr "cols"
{-# INLINE cols #-}

{- | Combinator for the @colspan@ attribute.

 Example:

 > div ! colspan "bar" $ "Hello."

 Result:

 > <div colspan="bar">Hello.</div>
-}
colspan ::
    ToLogStr s =>
    -- | Attribute value.
    s ->
    -- | Resulting attribute.
    Attr
colspan = attr "colspan"
{-# INLINE colspan #-}

{- | Combinator for the @content@ attribute.

 Example:

 > div ! content "bar" $ "Hello."

 Result:

 > <div content="bar">Hello.</div>
-}
content ::
    ToLogStr s =>
    -- | Attribute value.
    s ->
    -- | Resulting attribute.
    Attr
content = attr "content"
{-# INLINE content #-}

{- | Combinator for the @contenteditable@ attribute.

 Example:

 > div ! contenteditable "bar" $ "Hello."

 Result:

 > <div contenteditable="bar">Hello.</div>
-}
contenteditable ::
    ToLogStr s =>
    -- | Attribute value.
    s ->
    -- | Resulting attribute.
    Attr
contenteditable = attr "contenteditable"
{-# INLINE contenteditable #-}

{- | Combinator for the @contextmenu@ attribute.

 Example:

 > div ! contextmenu "bar" $ "Hello."

 Result:

 > <div contextmenu="bar">Hello.</div>
-}
contextmenu ::
    ToLogStr s =>
    -- | Attribute value.
    s ->
    -- | Resulting attribute.
    Attr
contextmenu = attr "contextmenu"
{-# INLINE contextmenu #-}

{- | Combinator for the @controls@ attribute.

 Example:

 > div ! controls "bar" $ "Hello."

 Result:

 > <div controls="bar">Hello.</div>
-}
controls ::
    ToLogStr s =>
    -- | Attribute value.
    s ->
    -- | Resulting attribute.
    Attr
controls = attr "controls"
{-# INLINE controls #-}

{- | Combinator for the @coords@ attribute.

 Example:

 > div ! coords "bar" $ "Hello."

 Result:

 > <div coords="bar">Hello.</div>
-}
coords ::
    ToLogStr s =>
    -- | Attribute value.
    s ->
    -- | Resulting attribute.
    Attr
coords = attr "coords"
{-# INLINE coords #-}

{- | Combinator for the @data@ attribute.

 Example:

 > div ! data_ "bar" $ "Hello."

 Result:

 > <div data="bar">Hello.</div>
-}
data_ ::
    ToLogStr s =>
    -- | Attribute value.
    s ->
    -- | Resulting attribute.
    Attr
data_ = attr "data"
{-# INLINE data_ #-}

{- | Combinator for the @datetime@ attribute.

 Example:

 > div ! datetime "bar" $ "Hello."

 Result:

 > <div datetime="bar">Hello.</div>
-}
datetime ::
    ToLogStr s =>
    -- | Attribute value.
    s ->
    -- | Resulting attribute.
    Attr
datetime = attr "datetime"
{-# INLINE datetime #-}

{- | Combinator for the @defer@ attribute.

 Example:

 > div ! defer "bar" $ "Hello."

 Result:

 > <div defer="bar">Hello.</div>
-}
defer ::
    ToLogStr s =>
    -- | Attribute value.
    s ->
    -- | Resulting attribute.
    Attr
defer = attr "defer"
{-# INLINE defer #-}

{- | Combinator for the @dir@ attribute.

 Example:

 > div ! dir "bar" $ "Hello."

 Result:

 > <div dir="bar">Hello.</div>
-}
dir ::
    ToLogStr s =>
    -- | Attribute value.
    s ->
    -- | Resulting attribute.
    Attr
dir = attr "dir"
{-# INLINE dir #-}

{- | Combinator for the @disabled@ attribute.

 Example:

 > div ! disabled "bar" $ "Hello."

 Result:

 > <div disabled="bar">Hello.</div>
-}
disabled ::
    ToLogStr s =>
    -- | Attribute value.
    s ->
    -- | Resulting attribute.
    Attr
disabled = attr "disabled"
{-# INLINE disabled #-}

{- | Combinator for the @draggable@ attribute.

 Example:

 > div ! draggable "bar" $ "Hello."

 Result:

 > <div draggable="bar">Hello.</div>
-}
draggable ::
    ToLogStr s =>
    -- | Attribute value.
    s ->
    -- | Resulting attribute.
    Attr
draggable = attr "draggable"
{-# INLINE draggable #-}

{- | Combinator for the @enctype@ attribute.

 Example:

 > div ! enctype "bar" $ "Hello."

 Result:

 > <div enctype="bar">Hello.</div>
-}
enctype ::
    ToLogStr s =>
    -- | Attribute value.
    s ->
    -- | Resulting attribute.
    Attr
enctype = attr "enctype"
{-# INLINE enctype #-}

{- | Combinator for the @for@ attribute.

 Example:

 > div ! for "bar" $ "Hello."

 Result:

 > <div for="bar">Hello.</div>
-}
for ::
    ToLogStr s =>
    -- | Attribute value.
    s ->
    -- | Resulting attribute.
    Attr
for = attr "for"
{-# INLINE for #-}

{- | Combinator for the @form@ attribute.

 Example:

 > div ! form "bar" $ "Hello."

 Result:

 > <div form="bar">Hello.</div>
-}
form ::
    ToLogStr s =>
    -- | Attribute value.
    s ->
    -- | Resulting attribute.
    Attr
form = attr "form"
{-# INLINE form #-}

{- | Combinator for the @formaction@ attribute.

 Example:

 > div ! formaction "bar" $ "Hello."

 Result:

 > <div formaction="bar">Hello.</div>
-}
formaction ::
    ToLogStr s =>
    -- | Attribute value.
    s ->
    -- | Resulting attribute.
    Attr
formaction = attr "formaction"
{-# INLINE formaction #-}

{- | Combinator for the @formenctype@ attribute.

 Example:

 > div ! formenctype "bar" $ "Hello."

 Result:

 > <div formenctype="bar">Hello.</div>
-}
formenctype ::
    ToLogStr s =>
    -- | Attribute value.
    s ->
    -- | Resulting attribute.
    Attr
formenctype = attr "formenctype"
{-# INLINE formenctype #-}

{- | Combinator for the @formmethod@ attribute.

 Example:

 > div ! formmethod "bar" $ "Hello."

 Result:

 > <div formmethod="bar">Hello.</div>
-}
formmethod ::
    ToLogStr s =>
    -- | Attribute value.
    s ->
    -- | Resulting attribute.
    Attr
formmethod = attr "formmethod"
{-# INLINE formmethod #-}

{- | Combinator for the @formnovalidate@ attribute.

 Example:

 > div ! formnovalidate "bar" $ "Hello."

 Result:

 > <div formnovalidate="bar">Hello.</div>
-}
formnovalidate ::
    ToLogStr s =>
    -- | Attribute value.
    s ->
    -- | Resulting attribute.
    Attr
formnovalidate = attr "formnovalidate"
{-# INLINE formnovalidate #-}


{- | Combinator for the @formtarget@ attribute.

 Example:

 > div ! formtarget "bar" $ "Hello."

 Result:

 > <div formtarget="bar">Hello.</div>
-}
formtarget ::
    ToLogStr s =>
    -- | Attribute value.
    s ->
    -- | Resulting attribute.
    Attr
formtarget = attr "formtarget"
{-# INLINE formtarget #-}

{- | Combinator for the @headers@ attribute.

 Example:

 > div ! headers "bar" $ "Hello."

 Result:

 > <div headers="bar">Hello.</div>
-}
headers ::
    ToLogStr s =>
    -- | Attribute value.
    s ->
    -- | Resulting attribute.
    Attr
headers = attr "headers"
{-# INLINE headers #-}

{- | Combinator for the @height@ attribute.

 Example:

 > div ! height "bar" $ "Hello."

 Result:

 > <div height="bar">Hello.</div>
-}
height ::
    ToLogStr s =>
    -- | Attribute value.
    s ->
    -- | Resulting attribute.
    Attr
height = attr "height"
{-# INLINE height #-}

{- | Combinator for the @hidden@ attribute.

 Example:

 > div ! hidden "bar" $ "Hello."

 Result:

 > <div hidden="bar">Hello.</div>
-}
hidden ::
    ToLogStr s =>
    -- | Attribute value.
    s ->
    -- | Resulting attribute.
    Attr
hidden = attr "hidden"
{-# INLINE hidden #-}

{- | Combinator for the @high@ attribute.

 Example:

 > div ! high "bar" $ "Hello."

 Result:

 > <div high="bar">Hello.</div>
-}
high ::
    ToLogStr s =>
    -- | Attribute value.
    s ->
    -- | Resulting attribute.
    Attr
high = attr "high"
{-# INLINE high #-}

{- | Combinator for the @href@ attribute.

 Example:

 > div ! href "bar" $ "Hello."

 Result:

 > <div href="bar">Hello.</div>
-}
href ::
    ToLogStr s =>
    -- | Attribute value.
    s ->
    -- | Resulting attribute.
    Attr
href = attr "href"
{-# INLINE href #-}

{- | Combinator for the @hreflang@ attribute.

 Example:

 > div ! hreflang "bar" $ "Hello."

 Result:

 > <div hreflang="bar">Hello.</div>
-}
hreflang ::
    ToLogStr s =>
    -- | Attribute value.
    s ->
    -- | Resulting attribute.
    Attr
hreflang = attr "hreflang"
{-# INLINE hreflang #-}

{- | Combinator for the @http-equiv@ attribute.

 Example:

 > div ! httpEquiv "bar" $ "Hello."

 Result:

 > <div http-equiv="bar">Hello.</div>
-}
httpEquiv ::
    ToLogStr s =>
    -- | Attribute value.
    s ->
    -- | Resulting attribute.
    Attr
httpEquiv = attr "http-equiv"
{-# INLINE httpEquiv #-}

{- | Combinator for the @icon@ attribute.

 Example:

 > div ! icon "bar" $ "Hello."

 Result:

 > <div icon="bar">Hello.</div>
-}
icon ::
    ToLogStr s =>
    -- | Attribute value.
    s ->
    -- | Resulting attribute.
    Attr
icon = attr "icon"
{-# INLINE icon #-}

{- | Combinator for the @id@ attribute.

 Example:

 > div ! id "bar" $ "Hello."

 Result:

 > <div id="bar">Hello.</div>
-}
id ::
    ToLogStr s =>
    -- | Attribute value.
    s ->
    -- | Resulting attribute.
    Attr
id = attr "id"
{-# INLINE id #-}

{- | Combinator for the @ismap@ attribute.

 Example:

 > div ! ismap "bar" $ "Hello."

 Result:

 > <div ismap="bar">Hello.</div>
-}
ismap ::
    ToLogStr s =>
    -- | Attribute value.
    s ->
    -- | Resulting attribute.
    Attr
ismap = attr "ismap"
{-# INLINE ismap #-}

{- | Combinator for the @item@ attribute.

 Example:

 > div ! item "bar" $ "Hello."

 Result:

 > <div item="bar">Hello.</div>
-}
item ::
    ToLogStr s =>
    -- | Attribute value.
    s ->
    -- | Resulting attribute.
    Attr
item = attr "item"
{-# INLINE item #-}

{- | Combinator for the @itemprop@ attribute.

 Example:

 > div ! itemprop "bar" $ "Hello."

 Result:

 > <div itemprop="bar">Hello.</div>
-}
itemprop ::
    ToLogStr s =>
    -- | Attribute value.
    s ->
    -- | Resulting attribute.
    Attr
itemprop = attr "itemprop"
{-# INLINE itemprop #-}

{- | Combinator for the @itemscope@ attribute.

 Example:

 > div ! itemscope "bar" $ "Hello."

 Result:

 > <div itemscope="bar">Hello.</div>
-}
itemscope ::
    ToLogStr s =>
    -- | Attribute value.
    s ->
    -- | Resulting attribute.
    Attr
itemscope = attr "itemscope"
{-# INLINE itemscope #-}

{- | Combinator for the @itemtype@ attribute.

 Example:

 > div ! itemtype "bar" $ "Hello."

 Result:

 > <div itemtype="bar">Hello.</div>
-}
itemtype ::
    ToLogStr s =>
    -- | Attribute value.
    s ->
    -- | Resulting attribute.
    Attr
itemtype = attr "itemtype"
{-# INLINE itemtype #-}

{- | Combinator for the @keytype@ attribute.

 Example:

 > div ! keytype "bar" $ "Hello."

 Result:

 > <div keytype="bar">Hello.</div>
-}
keytype ::
    ToLogStr s =>
    -- | Attribute value.
    s ->
    -- | Resulting attribute.
    Attr
keytype = attr "keytype"
{-# INLINE keytype #-}

{- | Combinator for the @label@ attribute.

 Example:

 > div ! label "bar" $ "Hello."

 Result:

 > <div label="bar">Hello.</div>
-}
label ::
    ToLogStr s =>
    -- | Attribute value.
    s ->
    -- | Resulting attribute.
    Attr
label = attr "label"
{-# INLINE label #-}

{- | Combinator for the @lang@ attribute.

 Example:

 > div ! lang "bar" $ "Hello."

 Result:

 > <div lang="bar">Hello.</div>
-}
lang ::
    ToLogStr s =>
    -- | Attribute value.
    s ->
    -- | Resulting attribute.
    Attr
lang = attr "lang"
{-# INLINE lang #-}

{- | Combinator for the @list@ attribute.

 Example:

 > div ! list "bar" $ "Hello."

 Result:

 > <div list="bar">Hello.</div>
-}
list ::
    ToLogStr s =>
    -- | Attribute value.
    s ->
    -- | Resulting attribute.
    Attr
list = attr "list"
{-# INLINE list #-}

{- | Combinator for the @loop@ attribute.

 Example:

 > div ! loop "bar" $ "Hello."

 Result:

 > <div loop="bar">Hello.</div>
-}
loop ::
    ToLogStr s =>
    -- | Attribute value.
    s ->
    -- | Resulting attribute.
    Attr
loop = attr "loop"
{-# INLINE loop #-}

{- | Combinator for the @low@ attribute.

 Example:

 > div ! low "bar" $ "Hello."

 Result:

 > <div low="bar">Hello.</div>
-}
low ::
    ToLogStr s =>
    -- | Attribute value.
    s ->
    -- | Resulting attribute.
    Attr
low = attr "low"
{-# INLINE low #-}

{- | Combinator for the @manifest@ attribute.

 Example:

 > div ! manifest "bar" $ "Hello."

 Result:

 > <div manifest="bar">Hello.</div>
-}
manifest ::
    ToLogStr s =>
    -- | Attribute value.
    s ->
    -- | Resulting attribute.
    Attr
manifest = attr "manifest"
{-# INLINE manifest #-}

{- | Combinator for the @max@ attribute.

 Example:

 > div ! max "bar" $ "Hello."

 Result:

 > <div max="bar">Hello.</div>
-}
max ::
    ToLogStr s =>
    -- | Attribute value.
    s ->
    -- | Resulting attribute.
    Attr
max = attr "max"
{-# INLINE max #-}

{- | Combinator for the @maxlength@ attribute.

 Example:

 > div ! maxlength "bar" $ "Hello."

 Result:

 > <div maxlength="bar">Hello.</div>
-}
maxlength ::
    ToLogStr s =>
    -- | Attribute value.
    s ->
    -- | Resulting attribute.
    Attr
maxlength = attr "maxlength"
{-# INLINE maxlength #-}

{- | Combinator for the @media@ attribute.

 Example:

 > div ! media "bar" $ "Hello."

 Result:

 > <div media="bar">Hello.</div>
-}
media ::
    ToLogStr s =>
    -- | Attribute value.
    s ->
    -- | Resulting attribute.
    Attr
media = attr "media"
{-# INLINE media #-}

{- | Combinator for the @method@ attribute.

 Example:

 > div ! method "bar" $ "Hello."

 Result:

 > <div method="bar">Hello.</div>
-}
method ::
    ToLogStr s =>
    -- | Attribute value.
    s ->
    -- | Resulting attribute.
    Attr
method = attr "method"
{-# INLINE method #-}

{- | Combinator for the @min@ attribute.

 Example:

 > div ! min "bar" $ "Hello."

 Result:

 > <div min="bar">Hello.</div>
-}
min ::
    ToLogStr s =>
    -- | Attribute value.
    s ->
    -- | Resulting attribute.
    Attr
min = attr "min"
{-# INLINE min #-}

{- | Combinator for the @multiple@ attribute.

 Example:

 > div ! multiple "bar" $ "Hello."

 Result:

 > <div multiple="bar">Hello.</div>
-}
multiple ::
    ToLogStr s =>
    -- | Attribute value.
    s ->
    -- | Resulting attribute.
    Attr
multiple = attr "multiple"
{-# INLINE multiple #-}

{- | Combinator for the @name@ attribute.

 Example:

 > div ! name "bar" $ "Hello."

 Result:

 > <div name="bar">Hello.</div>
-}
name ::
    ToLogStr s =>
    -- | Attribute value.
    s ->
    -- | Resulting attribute.
    Attr
name = attr "name"
{-# INLINE name #-}

{- | Combinator for the @novalidate@ attribute.

 Example:

 > div ! novalidate "bar" $ "Hello."

 Result:

 > <div novalidate="bar">Hello.</div>
-}
novalidate ::
    ToLogStr s =>
    -- | Attribute value.
    s ->
    -- | Resulting attribute.
    Attr
novalidate = attr "novalidate"
{-# INLINE novalidate #-}

{- | Combinator for the @onbeforeonload@ attribute.

 Example:

 > div ! onbeforeonload "bar" $ "Hello."

 Result:

 > <div onbeforeonload="bar">Hello.</div>
-}
onbeforeonload ::
    ToLogStr s =>
    -- | Attribute value.
    s ->
    -- | Resulting attribute.
    Attr
onbeforeonload = attr "onbeforeonload"
{-# INLINE onbeforeonload #-}

{- | Combinator for the @onbeforeprint@ attribute.

 Example:

 > div ! onbeforeprint "bar" $ "Hello."

 Result:

 > <div onbeforeprint="bar">Hello.</div>
-}
onbeforeprint ::
    ToLogStr s =>
    -- | Attribute value.
    s ->
    -- | Resulting attribute.
    Attr
onbeforeprint = attr "onbeforeprint"
{-# INLINE onbeforeprint #-}

{- | Combinator for the @onblur@ attribute.

 Example:

 > div ! onblur "bar" $ "Hello."

 Result:

 > <div onblur="bar">Hello.</div>
-}
onblur ::
    ToLogStr s =>
    -- | Attribute value.
    s ->
    -- | Resulting attribute.
    Attr
onblur = attr "onblur"
{-# INLINE onblur #-}

{- | Combinator for the @oncanplay@ attribute.

 Example:

 > div ! oncanplay "bar" $ "Hello."

 Result:

 > <div oncanplay="bar">Hello.</div>
-}
oncanplay ::
    ToLogStr s =>
    -- | Attribute value.
    s ->
    -- | Resulting attribute.
    Attr
oncanplay = attr "oncanplay"
{-# INLINE oncanplay #-}

{- | Combinator for the @oncanplaythrough@ attribute.

 Example:

 > div ! oncanplaythrough "bar" $ "Hello."

 Result:

 > <div oncanplaythrough="bar">Hello.</div>
-}
oncanplaythrough ::
    ToLogStr s =>
    -- | Attribute value.
    s ->
    -- | Resulting attribute.
    Attr
oncanplaythrough = attr "oncanplaythrough"
{-# INLINE oncanplaythrough #-}

{- | Combinator for the @onchange@ attribute.

 Example:

 > div ! onchange "bar" $ "Hello."

 Result:

 > <div onchange="bar">Hello.</div>
-}
onchange ::
    ToLogStr s =>
    -- | Attribute value.
    s ->
    -- | Resulting attribute.
    Attr
onchange = attr "onchange"
{-# INLINE onchange #-}

{- | Combinator for the @onclick@ attribute.

 Example:

 > div ! onclick "bar" $ "Hello."

 Result:

 > <div onclick="bar">Hello.</div>
-}
onclick ::
    ToLogStr s =>
    -- | Attribute value.
    s ->
    -- | Resulting attribute.
    Attr
onclick = attr "onclick"
{-# INLINE onclick #-}

{- | Combinator for the @oncontextmenu@ attribute.

 Example:

 > div ! oncontextmenu "bar" $ "Hello."

 Result:

 > <div oncontextmenu="bar">Hello.</div>
-}
oncontextmenu ::
    ToLogStr s =>
    -- | Attribute value.
    s ->
    -- | Resulting attribute.
    Attr
oncontextmenu = attr "oncontextmenu"
{-# INLINE oncontextmenu #-}

{- | Combinator for the @ondblclick@ attribute.

 Example:

 > div ! ondblclick "bar" $ "Hello."

 Result:

 > <div ondblclick="bar">Hello.</div>
-}
ondblclick ::
    ToLogStr s =>
    -- | Attribute value.
    s ->
    -- | Resulting attribute.
    Attr
ondblclick = attr "ondblclick"
{-# INLINE ondblclick #-}

{- | Combinator for the @ondrag@ attribute.

 Example:

 > div ! ondrag "bar" $ "Hello."

 Result:

 > <div ondrag="bar">Hello.</div>
-}
ondrag ::
    ToLogStr s =>
    -- | Attribute value.
    s ->
    -- | Resulting attribute.
    Attr
ondrag = attr "ondrag"
{-# INLINE ondrag #-}

{- | Combinator for the @ondragend@ attribute.

 Example:

 > div ! ondragend "bar" $ "Hello."

 Result:

 > <div ondragend="bar">Hello.</div>
-}
ondragend ::
    ToLogStr s =>
    -- | Attribute value.
    s ->
    -- | Resulting attribute.
    Attr
ondragend = attr "ondragend"
{-# INLINE ondragend #-}

{- | Combinator for the @ondragenter@ attribute.

 Example:

 > div ! ondragenter "bar" $ "Hello."

 Result:

 > <div ondragenter="bar">Hello.</div>
-}
ondragenter ::
    ToLogStr s =>
    -- | Attribute value.
    s ->
    -- | Resulting attribute.
    Attr
ondragenter = attr "ondragenter"
{-# INLINE ondragenter #-}

{- | Combinator for the @ondragleave@ attribute.

 Example:

 > div ! ondragleave "bar" $ "Hello."

 Result:

 > <div ondragleave="bar">Hello.</div>
-}
ondragleave ::
    ToLogStr s =>
    -- | Attribute value.
    s ->
    -- | Resulting attribute.
    Attr
ondragleave = attr "ondragleave"
{-# INLINE ondragleave #-}

{- | Combinator for the @ondragover@ attribute.

 Example:

 > div ! ondragover "bar" $ "Hello."

 Result:

 > <div ondragover="bar">Hello.</div>
-}
ondragover ::
    ToLogStr s =>
    -- | Attribute value.
    s ->
    -- | Resulting attribute.
    Attr
ondragover = attr "ondragover"
{-# INLINE ondragover #-}

{- | Combinator for the @ondragstart@ attribute.

 Example:

 > div ! ondragstart "bar" $ "Hello."

 Result:

 > <div ondragstart="bar">Hello.</div>
-}
ondragstart ::
    ToLogStr s =>
    -- | Attribute value.
    s ->
    -- | Resulting attribute.
    Attr
ondragstart = attr "ondragstart"
{-# INLINE ondragstart #-}

{- | Combinator for the @ondrop@ attribute.

 Example:

 > div ! ondrop "bar" $ "Hello."

 Result:

 > <div ondrop="bar">Hello.</div>
-}
ondrop ::
    ToLogStr s =>
    -- | Attribute value.
    s ->
    -- | Resulting attribute.
    Attr
ondrop = attr "ondrop"
{-# INLINE ondrop #-}

{- | Combinator for the @ondurationchange@ attribute.

 Example:

 > div ! ondurationchange "bar" $ "Hello."

 Result:

 > <div ondurationchange="bar">Hello.</div>
-}
ondurationchange ::
    ToLogStr s =>
    -- | Attribute value.
    s ->
    -- | Resulting attribute.
    Attr
ondurationchange = attr "ondurationchange"
{-# INLINE ondurationchange #-}

{- | Combinator for the @onemptied@ attribute.

 Example:

 > div ! onemptied "bar" $ "Hello."

 Result:

 > <div onemptied="bar">Hello.</div>
-}
onemptied ::
    ToLogStr s =>
    -- | Attribute value.
    s ->
    -- | Resulting attribute.
    Attr
onemptied = attr "onemptied"
{-# INLINE onemptied #-}

{- | Combinator for the @onended@ attribute.

 Example:

 > div ! onended "bar" $ "Hello."

 Result:

 > <div onended="bar">Hello.</div>
-}
onended ::
    ToLogStr s =>
    -- | Attribute value.
    s ->
    -- | Resulting attribute.
    Attr
onended = attr "onended"
{-# INLINE onended #-}

{- | Combinator for the @onerror@ attribute.

 Example:

 > div ! onerror "bar" $ "Hello."

 Result:

 > <div onerror="bar">Hello.</div>
-}
onerror ::
    ToLogStr s =>
    -- | Attribute value.
    s ->
    -- | Resulting attribute.
    Attr
onerror = attr "onerror"
{-# INLINE onerror #-}

{- | Combinator for the @onfocus@ attribute.

 Example:

 > div ! onfocus "bar" $ "Hello."

 Result:

 > <div onfocus="bar">Hello.</div>
-}
onfocus ::
    ToLogStr s =>
    -- | Attribute value.
    s ->
    -- | Resulting attribute.
    Attr
onfocus = attr "onfocus"
{-# INLINE onfocus #-}

{- | Combinator for the @onformchange@ attribute.

 Example:

 > div ! onformchange "bar" $ "Hello."

 Result:

 > <div onformchange="bar">Hello.</div>
-}
onformchange ::
    ToLogStr s =>
    -- | Attribute value.
    s ->
    -- | Resulting attribute.
    Attr
onformchange = attr "onformchange"
{-# INLINE onformchange #-}

{- | Combinator for the @onforminput@ attribute.

 Example:

 > div ! onforminput "bar" $ "Hello."

 Result:

 > <div onforminput="bar">Hello.</div>
-}
onforminput ::
    ToLogStr s =>
    -- | Attribute value.
    s ->
    -- | Resulting attribute.
    Attr
onforminput = attr "onforminput"
{-# INLINE onforminput #-}

{- | Combinator for the @onhaschange@ attribute.

 Example:

 > div ! onhaschange "bar" $ "Hello."

 Result:

 > <div onhaschange="bar">Hello.</div>
-}
onhaschange ::
    ToLogStr s =>
    -- | Attribute value.
    s ->
    -- | Resulting attribute.
    Attr
onhaschange = attr "onhaschange"
{-# INLINE onhaschange #-}

{- | Combinator for the @oninput@ attribute.

 Example:

 > div ! oninput "bar" $ "Hello."

 Result:

 > <div oninput="bar">Hello.</div>
-}
oninput ::
    ToLogStr s =>
    -- | Attribute value.
    s ->
    -- | Resulting attribute.
    Attr
oninput = attr "oninput"
{-# INLINE oninput #-}

{- | Combinator for the @oninvalid@ attribute.

 Example:

 > div ! oninvalid "bar" $ "Hello."

 Result:

 > <div oninvalid="bar">Hello.</div>
-}
oninvalid ::
    ToLogStr s =>
    -- | Attribute value.
    s ->
    -- | Resulting attribute.
    Attr
oninvalid = attr "oninvalid"
{-# INLINE oninvalid #-}

{- | Combinator for the @onkeydown@ attribute.

 Example:

 > div ! onkeydown "bar" $ "Hello."

 Result:

 > <div onkeydown="bar">Hello.</div>
-}
onkeydown ::
    ToLogStr s =>
    -- | Attribute value.
    s ->
    -- | Resulting attribute.
    Attr
onkeydown = attr "onkeydown"
{-# INLINE onkeydown #-}


{- | Combinator for the @onkeyup@ attribute.

 Example:

 > div ! onkeyup "bar" $ "Hello."

 Result:

 > <div onkeyup="bar">Hello.</div>
-}
onkeyup ::
    ToLogStr s =>
    -- | Attribute value.
    s ->
    -- | Resulting attribute.
    Attr
onkeyup = attr "onkeyup"
{-# INLINE onkeyup #-}

{- | Combinator for the @onload@ attribute.

 Example:

 > div ! onload "bar" $ "Hello."

 Result:

 > <div onload="bar">Hello.</div>
-}
onload ::
    ToLogStr s =>
    -- | Attribute value.
    s ->
    -- | Resulting attribute.
    Attr
onload = attr "onload"
{-# INLINE onload #-}

{- | Combinator for the @onloadeddata@ attribute.

 Example:

 > div ! onloadeddata "bar" $ "Hello."

 Result:

 > <div onloadeddata="bar">Hello.</div>
-}
onloadeddata ::
    ToLogStr s =>
    -- | Attribute value.
    s ->
    -- | Resulting attribute.
    Attr
onloadeddata = attr "onloadeddata"
{-# INLINE onloadeddata #-}

{- | Combinator for the @onloadedmetadata@ attribute.

 Example:

 > div ! onloadedmetadata "bar" $ "Hello."

 Result:

 > <div onloadedmetadata="bar">Hello.</div>
-}
onloadedmetadata ::
    ToLogStr s =>
    -- | Attribute value.
    s ->
    -- | Resulting attribute.
    Attr
onloadedmetadata = attr "onloadedmetadata"
{-# INLINE onloadedmetadata #-}

{- | Combinator for the @onloadstart@ attribute.

 Example:

 > div ! onloadstart "bar" $ "Hello."

 Result:

 > <div onloadstart="bar">Hello.</div>
-}
onloadstart ::
    ToLogStr s =>
    -- | Attribute value.
    s ->
    -- | Resulting attribute.
    Attr
onloadstart = attr "onloadstart"
{-# INLINE onloadstart #-}


{- | Combinator for the @onmessage@ attribute.

 Example:

 > div ! onmessage "bar" $ "Hello."

 Result:

 > <div onmessage="bar">Hello.</div>
-}
onmessage ::
    ToLogStr s =>
    -- | Attribute value.
    s ->
    -- | Resulting attribute.
    Attr
onmessage = attr "onmessage"
{-# INLINE onmessage #-}

{- | Combinator for the @onmousedown@ attribute.

 Example:

 > div ! onmousedown "bar" $ "Hello."

 Result:

 > <div onmousedown="bar">Hello.</div>
-}
onmousedown ::
    ToLogStr s =>
    -- | Attribute value.
    s ->
    -- | Resulting attribute.
    Attr
onmousedown = attr "onmousedown"
{-# INLINE onmousedown #-}

{- | Combinator for the @onmousemove@ attribute.

 Example:

 > div ! onmousemove "bar" $ "Hello."

 Result:

 > <div onmousemove="bar">Hello.</div>
-}
onmousemove ::
    ToLogStr s =>
    -- | Attribute value.
    s ->
    -- | Resulting attribute.
    Attr
onmousemove = attr "onmousemove"
{-# INLINE onmousemove #-}

{- | Combinator for the @onmouseout@ attribute.

 Example:

 > div ! onmouseout "bar" $ "Hello."

 Result:

 > <div onmouseout="bar">Hello.</div>
-}
onmouseout ::
    ToLogStr s =>
    -- | Attribute value.
    s ->
    -- | Resulting attribute.
    Attr
onmouseout = attr "onmouseout"
{-# INLINE onmouseout #-}

{- | Combinator for the @onmouseover@ attribute.

 Example:

 > div ! onmouseover "bar" $ "Hello."

 Result:

 > <div onmouseover="bar">Hello.</div>
-}
onmouseover ::
    ToLogStr s =>
    -- | Attribute value.
    s ->
    -- | Resulting attribute.
    Attr
onmouseover = attr "onmouseover"
{-# INLINE onmouseover #-}

{- | Combinator for the @onmouseup@ attribute.

 Example:

 > div ! onmouseup "bar" $ "Hello."

 Result:

 > <div onmouseup="bar">Hello.</div>
-}
onmouseup ::
    ToLogStr s =>
    -- | Attribute value.
    s ->
    -- | Resulting attribute.
    Attr
onmouseup = attr "onmouseup"
{-# INLINE onmouseup #-}

{- | Combinator for the @onmousewheel@ attribute.

 Example:

 > div ! onmousewheel "bar" $ "Hello."

 Result:

 > <div onmousewheel="bar">Hello.</div>
-}
onmousewheel ::
    ToLogStr s =>
    -- | Attribute value.
    s ->
    -- | Resulting attribute.
    Attr
onmousewheel = attr "onmousewheel"
{-# INLINE onmousewheel #-}

{- | Combinator for the @ononline@ attribute.

 Example:

 > div ! ononline "bar" $ "Hello."

 Result:

 > <div ononline="bar">Hello.</div>
-}
ononline ::
    ToLogStr s =>
    -- | Attribute value.
    s ->
    -- | Resulting attribute.
    Attr
ononline = attr "ononline"
{-# INLINE ononline #-}

{- | Combinator for the @onpagehide@ attribute.

 Example:

 > div ! onpagehide "bar" $ "Hello."

 Result:

 > <div onpagehide="bar">Hello.</div>
-}
onpagehide ::
    ToLogStr s =>
    -- | Attribute value.
    s ->
    -- | Resulting attribute.
    Attr
onpagehide = attr "onpagehide"
{-# INLINE onpagehide #-}

{- | Combinator for the @onpageshow@ attribute.

 Example:

 > div ! onpageshow "bar" $ "Hello."

 Result:

 > <div onpageshow="bar">Hello.</div>
-}
onpageshow ::
    ToLogStr s =>
    -- | Attribute value.
    s ->
    -- | Resulting attribute.
    Attr
onpageshow = attr "onpageshow"
{-# INLINE onpageshow #-}

{- | Combinator for the @onpause@ attribute.

 Example:

 > div ! onpause "bar" $ "Hello."

 Result:

 > <div onpause="bar">Hello.</div>
-}
onpause ::
    ToLogStr s =>
    -- | Attribute value.
    s ->
    -- | Resulting attribute.
    Attr
onpause = attr "onpause"
{-# INLINE onpause #-}

{- | Combinator for the @onplay@ attribute.

 Example:

 > div ! onplay "bar" $ "Hello."

 Result:

 > <div onplay="bar">Hello.</div>
-}
onplay ::
    ToLogStr s =>
    -- | Attribute value.
    s ->
    -- | Resulting attribute.
    Attr
onplay = attr "onplay"
{-# INLINE onplay #-}

{- | Combinator for the @onplaying@ attribute.

 Example:

 > div ! onplaying "bar" $ "Hello."

 Result:

 > <div onplaying="bar">Hello.</div>
-}
onplaying ::
    ToLogStr s =>
    -- | Attribute value.
    s ->
    -- | Resulting attribute.
    Attr
onplaying = attr "onplaying"
{-# INLINE onplaying #-}

{- | Combinator for the @onprogress@ attribute.

 Example:

 > div ! onprogress "bar" $ "Hello."

 Result:

 > <div onprogress="bar">Hello.</div>
-}
onprogress ::
    ToLogStr s =>
    -- | Attribute value.
    s ->
    -- | Resulting attribute.
    Attr
onprogress = attr "onprogress"
{-# INLINE onprogress #-}

{- | Combinator for the @onpropstate@ attribute.

 Example:

 > div ! onpropstate "bar" $ "Hello."

 Result:

 > <div onpropstate="bar">Hello.</div>
-}
onpropstate ::
    ToLogStr s =>
    -- | Attribute value.
    s ->
    -- | Resulting attribute.
    Attr
onpropstate = attr "onpropstate"
{-# INLINE onpropstate #-}

{- | Combinator for the @onratechange@ attribute.

 Example:

 > div ! onratechange "bar" $ "Hello."

 Result:

 > <div onratechange="bar">Hello.</div>
-}
onratechange ::
    ToLogStr s =>
    -- | Attribute value.
    s ->
    -- | Resulting attribute.
    Attr
onratechange = attr "onratechange"
{-# INLINE onratechange #-}

{- | Combinator for the @onreadystatechange@ attribute.

 Example:

 > div ! onreadystatechange "bar" $ "Hello."

 Result:

 > <div onreadystatechange="bar">Hello.</div>
-}
onreadystatechange ::
    ToLogStr s =>
    -- | Attribute value.
    s ->
    -- | Resulting attribute.
    Attr
onreadystatechange = attr "onreadystatechange"
{-# INLINE onreadystatechange #-}

{- | Combinator for the @onredo@ attribute.

 Example:

 > div ! onredo "bar" $ "Hello."

 Result:

 > <div onredo="bar">Hello.</div>
-}
onredo ::
    ToLogStr s =>
    -- | Attribute value.
    s ->
    -- | Resulting attribute.
    Attr
onredo = attr "onredo"
{-# INLINE onredo #-}

{- | Combinator for the @onresize@ attribute.

 Example:

 > div ! onresize "bar" $ "Hello."

 Result:

 > <div onresize="bar">Hello.</div>
-}
onresize ::
    ToLogStr s =>
    -- | Attribute value.
    s ->
    -- | Resulting attribute.
    Attr
onresize = attr "onresize"
{-# INLINE onresize #-}

{- | Combinator for the @onscroll@ attribute.

 Example:

 > div ! onscroll "bar" $ "Hello."

 Result:

 > <div onscroll="bar">Hello.</div>
-}
onscroll ::
    ToLogStr s =>
    -- | Attribute value.
    s ->
    -- | Resulting attribute.
    Attr
onscroll = attr "onscroll"
{-# INLINE onscroll #-}

{- | Combinator for the @onseeked@ attribute.

 Example:

 > div ! onseeked "bar" $ "Hello."

 Result:

 > <div onseeked="bar">Hello.</div>
-}
onseeked ::
    ToLogStr s =>
    -- | Attribute value.
    s ->
    -- | Resulting attribute.
    Attr
onseeked = attr "onseeked"
{-# INLINE onseeked #-}

{- | Combinator for the @onseeking@ attribute.

 Example:

 > div ! onseeking "bar" $ "Hello."

 Result:

 > <div onseeking="bar">Hello.</div>
-}
onseeking ::
    ToLogStr s =>
    -- | Attribute value.
    s ->
    -- | Resulting attribute.
    Attr
onseeking = attr "onseeking"
{-# INLINE onseeking #-}

{- | Combinator for the @onselect@ attribute.

 Example:

 > div ! onselect "bar" $ "Hello."

 Result:

 > <div onselect="bar">Hello.</div>
-}
onselect ::
    ToLogStr s =>
    -- | Attribute value.
    s ->
    -- | Resulting attribute.
    Attr
onselect = attr "onselect"
{-# INLINE onselect #-}

{- | Combinator for the @onstalled@ attribute.

 Example:

 > div ! onstalled "bar" $ "Hello."

 Result:

 > <div onstalled="bar">Hello.</div>
-}
onstalled ::
    ToLogStr s =>
    -- | Attribute value.
    s ->
    -- | Resulting attribute.
    Attr
onstalled = attr "onstalled"
{-# INLINE onstalled #-}

{- | Combinator for the @onstorage@ attribute.

 Example:

 > div ! onstorage "bar" $ "Hello."

 Result:

 > <div onstorage="bar">Hello.</div>
-}
onstorage ::
    ToLogStr s =>
    -- | Attribute value.
    s ->
    -- | Resulting attribute.
    Attr
onstorage = attr "onstorage"
{-# INLINE onstorage #-}

{- | Combinator for the @onsubmit@ attribute.

 Example:

 > div ! onsubmit "bar" $ "Hello."

 Result:

 > <div onsubmit="bar">Hello.</div>
-}
onsubmit ::
    ToLogStr s =>
    -- | Attribute value.
    s ->
    -- | Resulting attribute.
    Attr
onsubmit = attr "onsubmit"
{-# INLINE onsubmit #-}

{- | Combinator for the @onsuspend@ attribute.

 Example:

 > div ! onsuspend "bar" $ "Hello."

 Result:

 > <div onsuspend="bar">Hello.</div>
-}
onsuspend ::
    ToLogStr s =>
    -- | Attribute value.
    s ->
    -- | Resulting attribute.
    Attr
onsuspend = attr "onsuspend"
{-# INLINE onsuspend #-}

{- | Combinator for the @ontimeupdate@ attribute.

 Example:

 > div ! ontimeupdate "bar" $ "Hello."

 Result:

 > <div ontimeupdate="bar">Hello.</div>
-}
ontimeupdate ::
    ToLogStr s =>
    -- | Attribute value.
    s ->
    -- | Resulting attribute.
    Attr
ontimeupdate = attr "ontimeupdate"
{-# INLINE ontimeupdate #-}

{- | Combinator for the @onundo@ attribute.

 Example:

 > div ! onundo "bar" $ "Hello."

 Result:

 > <div onundo="bar">Hello.</div>
-}
onundo ::
    ToLogStr s =>
    -- | Attribute value.
    s ->
    -- | Resulting attribute.
    Attr
onundo = attr "onundo"
{-# INLINE onundo #-}

{- | Combinator for the @onunload@ attribute.

 Example:

 > div ! onunload "bar" $ "Hello."

 Result:

 > <div onunload="bar">Hello.</div>
-}
onunload ::
    ToLogStr s =>
    -- | Attribute value.
    s ->
    -- | Resulting attribute.
    Attr
onunload = attr "onunload"
{-# INLINE onunload #-}

{- | Combinator for the @onvolumechange@ attribute.

 Example:

 > div ! onvolumechange "bar" $ "Hello."

 Result:

 > <div onvolumechange="bar">Hello.</div>
-}
onvolumechange ::
    ToLogStr s =>
    -- | Attribute value.
    s ->
    -- | Resulting attribute.
    Attr
onvolumechange = attr "onvolumechange"
{-# INLINE onvolumechange #-}

{- | Combinator for the @onwaiting@ attribute.

 Example:

 > div ! onwaiting "bar" $ "Hello."

 Result:

 > <div onwaiting="bar">Hello.</div>
-}
onwaiting ::
    ToLogStr s =>
    -- | Attribute value.
    s ->
    -- | Resulting attribute.
    Attr
onwaiting = attr "onwaiting"
{-# INLINE onwaiting #-}

{- | Combinator for the @open@ attribute.

 Example:

 > div ! open "bar" $ "Hello."

 Result:

 > <div open="bar">Hello.</div>
-}
open ::
    ToLogStr s =>
    -- | Attribute value.
    s ->
    -- | Resulting attribute.
    Attr
open = attr "open"
{-# INLINE open #-}

{- | Combinator for the @optimum@ attribute.

 Example:

 > div ! optimum "bar" $ "Hello."

 Result:

 > <div optimum="bar">Hello.</div>
-}
optimum ::
    ToLogStr s =>
    -- | Attribute value.
    s ->
    -- | Resulting attribute.
    Attr
optimum = attr "optimum"
{-# INLINE optimum #-}

{- | Combinator for the @pattern@ attribute.

 Example:

 > div ! pattern "bar" $ "Hello."

 Result:

 > <div pattern="bar">Hello.</div>
-}
pattern ::
    ToLogStr s =>
    -- | Attribute value.
    s ->
    -- | Resulting attribute.
    Attr
pattern = attr "pattern"
{-# INLINE pattern #-}

{- | Combinator for the @ping@ attribute.

 Example:

 > div ! ping "bar" $ "Hello."

 Result:

 > <div ping="bar">Hello.</div>
-}
ping ::
    ToLogStr s =>
    -- | Attribute value.
    s ->
    -- | Resulting attribute.
    Attr
ping = attr "ping"
{-# INLINE ping #-}

{- | Combinator for the @placeholder@ attribute.

 Example:

 > div ! placeholder "bar" $ "Hello."

 Result:

 > <div placeholder="bar">Hello.</div>
-}
placeholder ::
    ToLogStr s =>
    -- | Attribute value.
    s ->
    -- | Resulting attribute.
    Attr
placeholder = attr "placeholder"
{-# INLINE placeholder #-}

{- | Combinator for the @preload@ attribute.

 Example:

 > div ! preload "bar" $ "Hello."

 Result:

 > <div preload="bar">Hello.</div>
-}
preload ::
    ToLogStr s =>
    -- | Attribute value.
    s ->
    -- | Resulting attribute.
    Attr
preload = attr "preload"
{-# INLINE preload #-}

{- | Combinator for the @pubdate@ attribute.

 Example:

 > div ! pubdate "bar" $ "Hello."

 Result:

 > <div pubdate="bar">Hello.</div>
-}
pubdate ::
    ToLogStr s =>
    -- | Attribute value.
    s ->
    -- | Resulting attribute.
    Attr
pubdate = attr "pubdate"
{-# INLINE pubdate #-}

{- | Combinator for the @radiogroup@ attribute.

 Example:

 > div ! radiogroup "bar" $ "Hello."

 Result:

 > <div radiogroup="bar">Hello.</div>
-}
radiogroup ::
    ToLogStr s =>
    -- | Attribute value.
    s ->
    -- | Resulting attribute.
    Attr
radiogroup = attr "radiogroup"
{-# INLINE radiogroup #-}

{- | Combinator for the @readonly@ attribute.

 Example:

 > div ! readonly "bar" $ "Hello."

 Result:

 > <div readonly="bar">Hello.</div>
-}
readonly ::
    ToLogStr s =>
    -- | Attribute value.
    s ->
    -- | Resulting attribute.
    Attr
readonly = attr "readonly"
{-# INLINE readonly #-}

{- | Combinator for the @rel@ attribute.

 Example:

 > div ! rel "bar" $ "Hello."

 Result:

 > <div rel="bar">Hello.</div>
-}
rel ::
    ToLogStr s =>
    -- | Attribute value.
    s ->
    -- | Resulting attribute.
    Attr
rel = attr "rel"
{-# INLINE rel #-}

{- | Combinator for the @required@ attribute.

 Example:

 > div ! required "bar" $ "Hello."

 Result:

 > <div required="bar">Hello.</div>
-}
required ::
    ToLogStr s =>
    -- | Attribute value.
    s ->
    -- | Resulting attribute.
    Attr
required = attr "required"
{-# INLINE required #-}

{- | Combinator for the @reversed@ attribute.

 Example:

 > div ! reversed "bar" $ "Hello."

 Result:

 > <div reversed="bar">Hello.</div>
-}
reversed ::
    ToLogStr s =>
    -- | Attribute value.
    s ->
    -- | Resulting attribute.
    Attr
reversed = attr "reversed"
{-# INLINE reversed #-}

{- | Combinator for the @role@ attribute.

 Example:

 > div ! role "bar" $ "Hello."

 Result:

 > <div role="bar">Hello.</div>
-}
role ::
    ToLogStr s =>
    -- | Attribute value.
    s ->
    -- | Resulting attribute.
    Attr
role = attr "role"
{-# INLINE role #-}

{- | Combinator for the @rows@ attribute.

 Example:

 > div ! rows "bar" $ "Hello."

 Result:

 > <div rows="bar">Hello.</div>
-}
rows ::
    ToLogStr s =>
    -- | Attribute value.
    s ->
    -- | Resulting attribute.
    Attr
rows = attr "rows"
{-# INLINE rows #-}

{- | Combinator for the @rowspan@ attribute.

 Example:

 > div ! rowspan "bar" $ "Hello."

 Result:

 > <div rowspan="bar">Hello.</div>
-}
rowspan ::
    ToLogStr s =>
    -- | Attribute value.
    s ->
    -- | Resulting attribute.
    Attr
rowspan = attr "rowspan"
{-# INLINE rowspan #-}

{- | Combinator for the @sandbox@ attribute.

 Example:

 > div ! sandbox "bar" $ "Hello."

 Result:

 > <div sandbox="bar">Hello.</div>
-}
sandbox ::
    ToLogStr s =>
    -- | Attribute value.
    s ->
    -- | Resulting attribute.
    Attr
sandbox = attr "sandbox"
{-# INLINE sandbox #-}

{- | Combinator for the @scope@ attribute.

 Example:

 > div ! scope "bar" $ "Hello."

 Result:

 > <div scope="bar">Hello.</div>
-}
scope ::
    ToLogStr s =>
    -- | Attribute value.
    s ->
    -- | Resulting attribute.
    Attr
scope = attr "scope"
{-# INLINE scope #-}

{- | Combinator for the @scoped@ attribute.

 Example:

 > div ! scoped "bar" $ "Hello."

 Result:

 > <div scoped="bar">Hello.</div>
-}
scoped ::
    ToLogStr s =>
    -- | Attribute value.
    s ->
    -- | Resulting attribute.
    Attr
scoped = attr "scoped"
{-# INLINE scoped #-}

{- | Combinator for the @seamless@ attribute.

 Example:

 > div ! seamless "bar" $ "Hello."

 Result:

 > <div seamless="bar">Hello.</div>
-}
seamless ::
    ToLogStr s =>
    -- | Attribute value.
    s ->
    -- | Resulting attribute.
    Attr
seamless = attr "seamless"
{-# INLINE seamless #-}

{- | Combinator for the @selected@ attribute.

 Example:

 > div ! selected "bar" $ "Hello."

 Result:

 > <div selected="bar">Hello.</div>
-}
selected ::
    ToLogStr s =>
    -- | Attribute value.
    s ->
    -- | Resulting attribute.
    Attr
selected = attr "selected"
{-# INLINE selected #-}

{- | Combinator for the @shape@ attribute.

 Example:

 > div ! shape "bar" $ "Hello."

 Result:

 > <div shape="bar">Hello.</div>
-}
shape ::
    ToLogStr s =>
    -- | Attribute value.
    s ->
    -- | Resulting attribute.
    Attr
shape = attr "shape"
{-# INLINE shape #-}

{- | Combinator for the @size@ attribute.

 Example:

 > div ! size "bar" $ "Hello."

 Result:

 > <div size="bar">Hello.</div>
-}
size ::
    ToLogStr s =>
    -- | Attribute value.
    s ->
    -- | Resulting attribute.
    Attr
size = attr "size"
{-# INLINE size #-}

{- | Combinator for the @sizes@ attribute.

 Example:

 > div ! sizes "bar" $ "Hello."

 Result:

 > <div sizes="bar">Hello.</div>
-}
sizes ::
    ToLogStr s =>
    -- | Attribute value.
    s ->
    -- | Resulting attribute.
    Attr
sizes = attr "sizes"
{-# INLINE sizes #-}

{- | Combinator for the @span@ attribute.

 Example:

 > div ! span "bar" $ "Hello."

 Result:

 > <div span="bar">Hello.</div>
-}
span ::
    ToLogStr s =>
    -- | Attribute value.
    s ->
    -- | Resulting attribute.
    Attr
span = attr "span"
{-# INLINE span #-}

{- | Combinator for the @spellcheck@ attribute.

 Example:

 > div ! spellcheck "bar" $ "Hello."

 Result:

 > <div spellcheck="bar">Hello.</div>
-}
spellcheck ::
    ToLogStr s =>
    -- | Attribute value.
    s ->
    -- | Resulting attribute.
    Attr
spellcheck = attr "spellcheck"
{-# INLINE spellcheck #-}

{- | Combinator for the @src@ attribute.

 Example:

 > div ! src "bar" $ "Hello."

 Result:

 > <div src="bar">Hello.</div>
-}
src ::
    ToLogStr s =>
    -- | Attribute value.
    s ->
    -- | Resulting attribute.
    Attr
src = attr "src"
{-# INLINE src #-}

{- | Combinator for the @srcdoc@ attribute.

 Example:

 > div ! srcdoc "bar" $ "Hello."

 Result:

 > <div srcdoc="bar">Hello.</div>
-}
srcdoc ::
    ToLogStr s =>
    -- | Attribute value.
    s ->
    -- | Resulting attribute.
    Attr
srcdoc = attr "srcdoc"
{-# INLINE srcdoc #-}

{- | Combinator for the @start@ attribute.

 Example:

 > div ! start "bar" $ "Hello."

 Result:

 > <div start="bar">Hello.</div>
-}
start ::
    ToLogStr s =>
    -- | Attribute value.
    s ->
    -- | Resulting attribute.
    Attr
start = attr "start"
{-# INLINE start #-}

{- | Combinator for the @step@ attribute.

 Example:

 > div ! step "bar" $ "Hello."

 Result:

 > <div step="bar">Hello.</div>
-}
step ::
    ToLogStr s =>
    -- | Attribute value.
    s ->
    -- | Resulting attribute.
    Attr
step = attr "step"
{-# INLINE step #-}

{- | Combinator for the @style@ attribute.

 Example:

 > div ! style "bar" $ "Hello."

 Result:

 > <div style="bar">Hello.</div>
-}
style ::
    ToLogStr s =>
    -- | Attribute value.
    s ->
    -- | Resulting attribute.
    Attr
style = attr "style"
{-# INLINE style #-}

{- | Combinator for the @subject@ attribute.

 Example:

 > div ! subject "bar" $ "Hello."

 Result:

 > <div subject="bar">Hello.</div>
-}
subject ::
    ToLogStr s =>
    -- | Attribute value.
    s ->
    -- | Resulting attribute.
    Attr
subject = attr "subject"
{-# INLINE subject #-}

{- | Combinator for the @summary@ attribute.

 Example:

 > div ! summary "bar" $ "Hello."

 Result:

 > <div summary="bar">Hello.</div>
-}
summary ::
    ToLogStr s =>
    -- | Attribute value.
    s ->
    -- | Resulting attribute.
    Attr
summary = attr "summary"
{-# INLINE summary #-}

{- | Combinator for the @tabindex@ attribute.

 Example:

 > div ! tabindex "bar" $ "Hello."

 Result:

 > <div tabindex="bar">Hello.</div>
-}
tabindex ::
    ToLogStr s =>
    -- | Attribute value.
    s ->
    -- | Resulting attribute.
    Attr
tabindex = attr "tabindex"
{-# INLINE tabindex #-}

{- | Combinator for the @target@ attribute.

 Example:

 > div ! target "bar" $ "Hello."

 Result:

 > <div target="bar">Hello.</div>
-}
target ::
    ToLogStr s =>
    -- | Attribute value.
    s ->
    -- | Resulting attribute.
    Attr
target = attr "target"
{-# INLINE target #-}

{- | Combinator for the @title@ attribute.

 Example:

 > div ! title "bar" $ "Hello."

 Result:

 > <div title="bar">Hello.</div>
-}
title ::
    ToLogStr s =>
    -- | Attribute value.
    s ->
    -- | Resulting attribute.
    Attr
title = attr "title"
{-# INLINE title #-}

{- | Combinator for the @type@ attribute.

 Example:

 > div ! type_ "bar" $ "Hello."

 Result:

 > <div type="bar">Hello.</div>
-}
type_ ::
    ToLogStr s =>
    -- | Attribute value.
    s ->
    -- | Resulting attribute.
    Attr
type_ = attr "type"
{-# INLINE type_ #-}

{- | Combinator for the @usemap@ attribute.

 Example:

 > div ! usemap "bar" $ "Hello."

 Result:

 > <div usemap="bar">Hello.</div>
-}
usemap ::
    ToLogStr s =>
    -- | Attribute value.
    s ->
    -- | Resulting attribute.
    Attr
usemap = attr "usemap"
{-# INLINE usemap #-}

{- | Combinator for the @value@ attribute.

 Example:

 > div ! value "bar" $ "Hello."

 Result:

 > <div value="bar">Hello.</div>
-}
value ::
    ToLogStr s =>
    -- | Attribute value.
    s ->
    -- | Resulting attribute.
    Attr
value = attr "value"
{-# INLINE value #-}

{- | Combinator for the @width@ attribute.

 Example:

 > div ! width "bar" $ "Hello."

 Result:

 > <div width="bar">Hello.</div>
-}
width ::
    ToLogStr s =>
    -- | Attribute value.
    s ->
    -- | Resulting attribute.
    Attr
width = attr "width"
{-# INLINE width #-}

{- | Combinator for the @wrap@ attribute.

 Example:

 > div ! wrap "bar" $ "Hello."

 Result:

 > <div wrap="bar">Hello.</div>
-}
wrap ::
    ToLogStr s =>
    -- | Attribute value.
    s ->
    -- | Resulting attribute.
    Attr
wrap = attr "wrap"
{-# INLINE wrap #-}

{- | Combinator for the @xmlns@ attribute.

 Example:

 > div ! xmlns "bar" $ "Hello."

 Result:

 > <div xmlns="bar">Hello.</div>
-}
xmlns ::
    ToLogStr s =>
    -- | Attribute value.
    s ->
    -- | Resulting attribute.
    Attr
xmlns = attr "xmlns"
{-# INLINE xmlns #-}
