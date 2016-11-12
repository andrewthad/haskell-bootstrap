{-# OPTIONS_GHC -Wall #-}

module Yesod.Elements
  (
  -- * Parent Elements
    a_
  , abbr_
  , address_
  , article_
  , aside_
  , audio_
  , b_
  , bdo_
  , blockquote_
  , body_
  , button_
  , canvas_
  , caption_
  , cite_
  , code_
  , colgroup_
  , command_
  , datalist_
  , dd_
  , del_
  , details_
  , dfn_
  , div_
  , dl_
  , dt_
  , em_
  , fieldset_
  , figcaption_
  , figure_
  , footer_
  , form_
  , h1_
  , h2_
  , h3_
  , h4_
  , h5_
  , h6_
  , head_
  , header_
  , hgroup_
  , html_
  , i_
  , iframe_
  , ins_
  , kbd_
  , label_
  , legend_
  , li_
  , main_
  , map_
  , mark_
  , menu_
  , meter_
  , nav_
  , noscript_
  , object_
  , ol_
  , optgroup_
  , option_
  , output_
  , p_
  , pre_
  , progress_
  , q_
  , rp_
  , rt_
  , ruby_
  , samp_
  , script_
  , section_
  , select_
  , small_
  , span_'
  , strong_
  , style_
  , sub_
  , summary_
  , sup_
  , table_
  , tbody_
  , td_
  , textarea_
  , tfoot_
  , th_
  , thead_
  , time_
  , title_
  , tr_
  , ul_
  , var_
  , video_
  -- * Leaf Elements
  , area_
  , base_
  , br_
  , col_
  , embed_
  , hr_
  , img_
  , input_
  , keygen_
  , link_
  , menuitem_
  , meta_
  , param_
  , source_
  , track_
  , wbr_
  -- * Attributes
  , accept_
  , acceptCharset_
  , accesskey_
  , action_
  , alt_
  , async_
  , autocomplete_
  , autofocus_
  , autoplay_
  , challenge_
  , charset_
  , checked_
  , cite_'
  , class_
  , cols_
  , colspan_
  , content_
  , contenteditable_
  , contextmenu_
  , controls_
  , coords_
  , data_
  , datetime_
  , defer_
  , dir_
  , disabled_
  , draggable_
  , enctype_
  , for_'
  , form_'
  , formaction_
  , formenctype_
  , formmethod_
  , formnovalidate_
  , formtarget_
  , headers_
  , height_
  , hidden_
  , high_
  , href_
  , hreflang_
  , httpEquiv_
  , icon_
  , id_
  , ismap_
  , item_
  , itemprop_
  , itemscope_
  , itemtype_
  , keytype_
  , label_'
  , lang_
  , list_
  , loop_
  , low_
  , manifest_
  , max_
  , maxlength_
  , media_
  , method_
  , min_
  , multiple_
  , name_
  , novalidate_
  , onbeforeonload_
  , onbeforeprint_
  , onblur_
  , oncanplay_
  , oncanplaythrough_
  , onchange_
  , oncontextmenu_
  , onclick_
  , ondblclick_
  , ondrag_
  , ondragend_
  , ondragenter_
  , ondragleave_
  , ondragover_
  , ondragstart_
  , ondrop_
  , ondurationchange_
  , onemptied_
  , onended_
  , onerror_
  , onfocus_
  , onformchange_
  , onforminput_
  , onhaschange_
  , oninput_
  , oninvalid_
  , onkeydown_
  , onkeyup_
  , onload_
  , onloadeddata_
  , onloadedmetadata_
  , onloadstart_
  , onmessage_
  , onmousedown_
  , onmousemove_
  , onmouseout_
  , onmouseover_
  , onmouseup_
  , onmousewheel_
  , ononline_
  , onpagehide_
  , onpageshow_
  , onpause_
  , onplay_
  , onplaying_
  , onprogress_
  , onpropstate_
  , onratechange_
  , onreadystatechange_
  , onredo_
  , onresize_
  , onscroll_
  , onseeked_
  , onseeking_
  , onselect_
  , onstalled_
  , onstorage_
  , onsubmit_
  , onsuspend_
  , ontimeupdate_
  , onundo_
  , onunload_
  , onvolumechange_
  , onwaiting_
  , open_
  , optimum_
  , pattern_
  , ping_
  , placeholder_
  , preload_
  , pubdate_
  , radiogroup_
  , readonly_
  , rel_
  , required_
  , reversed_
  , rows_
  , rowspan_
  , sandbox_
  , scope_
  , scoped_
  , seamless_
  , selected_
  , shape_
  , size_
  , sizes_
  , span_
  , spellcheck_
  , src_
  , srcdoc_
  , start_
  , step_
  , style_'
  , subject_
  , summary_'
  , tabindex_
  , target_
  , title_'
  , type_
  , usemap_
  , value_
  , width_
  , wrap_
  , xmlns_
    -- * Lift HTML to Widget
  , liftParent
  , liftLeaf
  ) where

import Yesod.Core.Types (Body(..),GWData(..),WidgetT(..))
import Text.Blaze.Html (Html,Attribute,AttributeValue)
import Data.Foldable
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as HA

a_ :: (Monad m, Foldable t) => t Attribute -> WidgetT site m a -> WidgetT site m a
a_ = liftParent H.a

abbr_ :: (Monad m, Foldable t) => t Attribute -> WidgetT site m a -> WidgetT site m a
abbr_ = liftParent H.abbr

address_ :: (Monad m, Foldable t) => t Attribute -> WidgetT site m a -> WidgetT site m a
address_ = liftParent H.address

article_ :: (Monad m, Foldable t) => t Attribute -> WidgetT site m a -> WidgetT site m a
article_ = liftParent H.article

aside_ :: (Monad m, Foldable t) => t Attribute -> WidgetT site m a -> WidgetT site m a
aside_ = liftParent H.aside

audio_ :: (Monad m, Foldable t) => t Attribute -> WidgetT site m a -> WidgetT site m a
audio_ = liftParent H.audio

b_ :: (Monad m, Foldable t) => t Attribute -> WidgetT site m a -> WidgetT site m a
b_ = liftParent H.b

bdo_ :: (Monad m, Foldable t) => t Attribute -> WidgetT site m a -> WidgetT site m a
bdo_ = liftParent H.bdo

blockquote_ :: (Monad m, Foldable t) => t Attribute -> WidgetT site m a -> WidgetT site m a
blockquote_ = liftParent H.blockquote

body_ :: (Monad m, Foldable t) => t Attribute -> WidgetT site m a -> WidgetT site m a
body_ = liftParent H.body

button_ :: (Monad m, Foldable t) => t Attribute -> WidgetT site m a -> WidgetT site m a
button_ = liftParent H.button

canvas_ :: (Monad m, Foldable t) => t Attribute -> WidgetT site m a -> WidgetT site m a
canvas_ = liftParent H.canvas

caption_ :: (Monad m, Foldable t) => t Attribute -> WidgetT site m a -> WidgetT site m a
caption_ = liftParent H.caption

cite_ :: (Monad m, Foldable t) => t Attribute -> WidgetT site m a -> WidgetT site m a
cite_ = liftParent H.cite

code_ :: (Monad m, Foldable t) => t Attribute -> WidgetT site m a -> WidgetT site m a
code_ = liftParent H.code

colgroup_ :: (Monad m, Foldable t) => t Attribute -> WidgetT site m a -> WidgetT site m a
colgroup_ = liftParent H.colgroup

command_ :: (Monad m, Foldable t) => t Attribute -> WidgetT site m a -> WidgetT site m a
command_ = liftParent H.command

datalist_ :: (Monad m, Foldable t) => t Attribute -> WidgetT site m a -> WidgetT site m a
datalist_ = liftParent H.datalist

dd_ :: (Monad m, Foldable t) => t Attribute -> WidgetT site m a -> WidgetT site m a
dd_ = liftParent H.dd

del_ :: (Monad m, Foldable t) => t Attribute -> WidgetT site m a -> WidgetT site m a
del_ = liftParent H.del

details_ :: (Monad m, Foldable t) => t Attribute -> WidgetT site m a -> WidgetT site m a
details_ = liftParent H.details

dfn_ :: (Monad m, Foldable t) => t Attribute -> WidgetT site m a -> WidgetT site m a
dfn_ = liftParent H.dfn

div_ :: (Monad m, Foldable t) => t Attribute -> WidgetT site m a -> WidgetT site m a
div_ = liftParent H.div

dl_ :: (Monad m, Foldable t) => t Attribute -> WidgetT site m a -> WidgetT site m a
dl_ = liftParent H.dl

dt_ :: (Monad m, Foldable t) => t Attribute -> WidgetT site m a -> WidgetT site m a
dt_ = liftParent H.dt

em_ :: (Monad m, Foldable t) => t Attribute -> WidgetT site m a -> WidgetT site m a
em_ = liftParent H.em

fieldset_ :: (Monad m, Foldable t) => t Attribute -> WidgetT site m a -> WidgetT site m a
fieldset_ = liftParent H.fieldset

figcaption_ :: (Monad m, Foldable t) => t Attribute -> WidgetT site m a -> WidgetT site m a
figcaption_ = liftParent H.figcaption

figure_ :: (Monad m, Foldable t) => t Attribute -> WidgetT site m a -> WidgetT site m a
figure_ = liftParent H.figure

footer_ :: (Monad m, Foldable t) => t Attribute -> WidgetT site m a -> WidgetT site m a
footer_ = liftParent H.footer

form_ :: (Monad m, Foldable t) => t Attribute -> WidgetT site m a -> WidgetT site m a
form_ = liftParent H.form

h1_ :: (Monad m, Foldable t) => t Attribute -> WidgetT site m a -> WidgetT site m a
h1_ = liftParent H.h1

h2_ :: (Monad m, Foldable t) => t Attribute -> WidgetT site m a -> WidgetT site m a
h2_ = liftParent H.h2

h3_ :: (Monad m, Foldable t) => t Attribute -> WidgetT site m a -> WidgetT site m a
h3_ = liftParent H.h3

h4_ :: (Monad m, Foldable t) => t Attribute -> WidgetT site m a -> WidgetT site m a
h4_ = liftParent H.h4

h5_ :: (Monad m, Foldable t) => t Attribute -> WidgetT site m a -> WidgetT site m a
h5_ = liftParent H.h5

h6_ :: (Monad m, Foldable t) => t Attribute -> WidgetT site m a -> WidgetT site m a
h6_ = liftParent H.h6

head_ :: (Monad m, Foldable t) => t Attribute -> WidgetT site m a -> WidgetT site m a
head_ = liftParent H.head

header_ :: (Monad m, Foldable t) => t Attribute -> WidgetT site m a -> WidgetT site m a
header_ = liftParent H.header

hgroup_ :: (Monad m, Foldable t) => t Attribute -> WidgetT site m a -> WidgetT site m a
hgroup_ = liftParent H.hgroup

html_ :: (Monad m, Foldable t) => t Attribute -> WidgetT site m a -> WidgetT site m a
html_ = liftParent H.html

i_ :: (Monad m, Foldable t) => t Attribute -> WidgetT site m a -> WidgetT site m a
i_ = liftParent H.i

iframe_ :: (Monad m, Foldable t) => t Attribute -> WidgetT site m a -> WidgetT site m a
iframe_ = liftParent H.iframe

ins_ :: (Monad m, Foldable t) => t Attribute -> WidgetT site m a -> WidgetT site m a
ins_ = liftParent H.ins

kbd_ :: (Monad m, Foldable t) => t Attribute -> WidgetT site m a -> WidgetT site m a
kbd_ = liftParent H.kbd

label_ :: (Monad m, Foldable t) => t Attribute -> WidgetT site m a -> WidgetT site m a
label_ = liftParent H.label

legend_ :: (Monad m, Foldable t) => t Attribute -> WidgetT site m a -> WidgetT site m a
legend_ = liftParent H.legend

li_ :: (Monad m, Foldable t) => t Attribute -> WidgetT site m a -> WidgetT site m a
li_ = liftParent H.li

main_ :: (Monad m, Foldable t) => t Attribute -> WidgetT site m a -> WidgetT site m a
main_ = liftParent H.main

map_ :: (Monad m, Foldable t) => t Attribute -> WidgetT site m a -> WidgetT site m a
map_ = liftParent H.map

mark_ :: (Monad m, Foldable t) => t Attribute -> WidgetT site m a -> WidgetT site m a
mark_ = liftParent H.mark

menu_ :: (Monad m, Foldable t) => t Attribute -> WidgetT site m a -> WidgetT site m a
menu_ = liftParent H.menu

meter_ :: (Monad m, Foldable t) => t Attribute -> WidgetT site m a -> WidgetT site m a
meter_ = liftParent H.meter

nav_ :: (Monad m, Foldable t) => t Attribute -> WidgetT site m a -> WidgetT site m a
nav_ = liftParent H.nav

noscript_ :: (Monad m, Foldable t) => t Attribute -> WidgetT site m a -> WidgetT site m a
noscript_ = liftParent H.noscript

object_ :: (Monad m, Foldable t) => t Attribute -> WidgetT site m a -> WidgetT site m a
object_ = liftParent H.object

ol_ :: (Monad m, Foldable t) => t Attribute -> WidgetT site m a -> WidgetT site m a
ol_ = liftParent H.ol

optgroup_ :: (Monad m, Foldable t) => t Attribute -> WidgetT site m a -> WidgetT site m a
optgroup_ = liftParent H.optgroup

option_ :: (Monad m, Foldable t) => t Attribute -> WidgetT site m a -> WidgetT site m a
option_ = liftParent H.option

output_ :: (Monad m, Foldable t) => t Attribute -> WidgetT site m a -> WidgetT site m a
output_ = liftParent H.output

p_ :: (Monad m, Foldable t) => t Attribute -> WidgetT site m a -> WidgetT site m a
p_ = liftParent H.p

pre_ :: (Monad m, Foldable t) => t Attribute -> WidgetT site m a -> WidgetT site m a
pre_ = liftParent H.pre

progress_ :: (Monad m, Foldable t) => t Attribute -> WidgetT site m a -> WidgetT site m a
progress_ = liftParent H.progress

q_ :: (Monad m, Foldable t) => t Attribute -> WidgetT site m a -> WidgetT site m a
q_ = liftParent H.q

rp_ :: (Monad m, Foldable t) => t Attribute -> WidgetT site m a -> WidgetT site m a
rp_ = liftParent H.rp

rt_ :: (Monad m, Foldable t) => t Attribute -> WidgetT site m a -> WidgetT site m a
rt_ = liftParent H.rt

ruby_ :: (Monad m, Foldable t) => t Attribute -> WidgetT site m a -> WidgetT site m a
ruby_ = liftParent H.ruby

samp_ :: (Monad m, Foldable t) => t Attribute -> WidgetT site m a -> WidgetT site m a
samp_ = liftParent H.samp

script_ :: (Monad m, Foldable t) => t Attribute -> WidgetT site m a -> WidgetT site m a
script_ = liftParent H.script

section_ :: (Monad m, Foldable t) => t Attribute -> WidgetT site m a -> WidgetT site m a
section_ = liftParent H.section

select_ :: (Monad m, Foldable t) => t Attribute -> WidgetT site m a -> WidgetT site m a
select_ = liftParent H.select

small_ :: (Monad m, Foldable t) => t Attribute -> WidgetT site m a -> WidgetT site m a
small_ = liftParent H.small

span_ :: (Monad m, Foldable t) => t Attribute -> WidgetT site m a -> WidgetT site m a
span_ = liftParent H.span

strong_ :: (Monad m, Foldable t) => t Attribute -> WidgetT site m a -> WidgetT site m a
strong_ = liftParent H.strong

style_ :: (Monad m, Foldable t) => t Attribute -> WidgetT site m a -> WidgetT site m a
style_ = liftParent H.style

sub_ :: (Monad m, Foldable t) => t Attribute -> WidgetT site m a -> WidgetT site m a
sub_ = liftParent H.sub

summary_ :: (Monad m, Foldable t) => t Attribute -> WidgetT site m a -> WidgetT site m a
summary_ = liftParent H.summary

sup_ :: (Monad m, Foldable t) => t Attribute -> WidgetT site m a -> WidgetT site m a
sup_ = liftParent H.sup

table_ :: (Monad m, Foldable t) => t Attribute -> WidgetT site m a -> WidgetT site m a
table_ = liftParent H.table

tbody_ :: (Monad m, Foldable t) => t Attribute -> WidgetT site m a -> WidgetT site m a
tbody_ = liftParent H.tbody

td_ :: (Monad m, Foldable t) => t Attribute -> WidgetT site m a -> WidgetT site m a
td_ = liftParent H.td

textarea_ :: (Monad m, Foldable t) => t Attribute -> WidgetT site m a -> WidgetT site m a
textarea_ = liftParent H.textarea

tfoot_ :: (Monad m, Foldable t) => t Attribute -> WidgetT site m a -> WidgetT site m a
tfoot_ = liftParent H.tfoot

th_ :: (Monad m, Foldable t) => t Attribute -> WidgetT site m a -> WidgetT site m a
th_ = liftParent H.th

thead_ :: (Monad m, Foldable t) => t Attribute -> WidgetT site m a -> WidgetT site m a
thead_ = liftParent H.thead

time_ :: (Monad m, Foldable t) => t Attribute -> WidgetT site m a -> WidgetT site m a
time_ = liftParent H.time

title_ :: (Monad m, Foldable t) => t Attribute -> WidgetT site m a -> WidgetT site m a
title_ = liftParent H.title

tr_ :: (Monad m, Foldable t) => t Attribute -> WidgetT site m a -> WidgetT site m a
tr_ = liftParent H.tr

ul_ :: (Monad m, Foldable t) => t Attribute -> WidgetT site m a -> WidgetT site m a
ul_ = liftParent H.ul

var_ :: (Monad m, Foldable t) => t Attribute -> WidgetT site m a -> WidgetT site m a
var_ = liftParent H.var

video_ :: (Monad m, Foldable t) => t Attribute -> WidgetT site m a -> WidgetT site m a
video_ = liftParent H.video



area_ :: (Monad m, Foldable t) => t Attribute -> WidgetT site m ()
area_ = liftLeaf H.area

base_ :: (Monad m, Foldable t) => t Attribute -> WidgetT site m ()
base_ = liftLeaf H.base

br_ :: (Monad m, Foldable t) => t Attribute -> WidgetT site m ()
br_ = liftLeaf H.br

col_ :: (Monad m, Foldable t) => t Attribute -> WidgetT site m ()
col_ = liftLeaf H.col

embed_ :: (Monad m, Foldable t) => t Attribute -> WidgetT site m ()
embed_ = liftLeaf H.embed

hr_ :: (Monad m, Foldable t) => t Attribute -> WidgetT site m ()
hr_ = liftLeaf H.hr

img_ :: (Monad m, Foldable t) => t Attribute -> WidgetT site m ()
img_ = liftLeaf H.img

input_ :: (Monad m, Foldable t) => t Attribute -> WidgetT site m ()
input_ = liftLeaf H.input

keygen_ :: (Monad m, Foldable t) => t Attribute -> WidgetT site m ()
keygen_ = liftLeaf H.keygen

link_ :: (Monad m, Foldable t) => t Attribute -> WidgetT site m ()
link_ = liftLeaf H.link

menuitem_ :: (Monad m, Foldable t) => t Attribute -> WidgetT site m ()
menuitem_ = liftLeaf H.menuitem

meta_ :: (Monad m, Foldable t) => t Attribute -> WidgetT site m ()
meta_ = liftLeaf H.meta

param_ :: (Monad m, Foldable t) => t Attribute -> WidgetT site m ()
param_ = liftLeaf H.param

source_ :: (Monad m, Foldable t) => t Attribute -> WidgetT site m ()
source_ = liftLeaf H.source

track_ :: (Monad m, Foldable t) => t Attribute -> WidgetT site m ()
track_ = liftLeaf H.track

wbr_ :: (Monad m, Foldable t) => t Attribute -> WidgetT site m ()
wbr_ = liftLeaf H.wbr

accept_ :: AttributeValue -> Attribute
accept_ = HA.accept

acceptCharset_ :: AttributeValue -> Attribute
acceptCharset_ = HA.acceptCharset

accesskey_ :: AttributeValue -> Attribute
accesskey_ = HA.accesskey

action_ :: AttributeValue -> Attribute
action_ = HA.action

alt_ :: AttributeValue -> Attribute
alt_ = HA.alt

async_ :: AttributeValue -> Attribute
async_ = HA.async

autocomplete_ :: AttributeValue -> Attribute
autocomplete_ = HA.autocomplete

autofocus_ :: AttributeValue -> Attribute
autofocus_ = HA.autofocus

autoplay_ :: AttributeValue -> Attribute
autoplay_ = HA.autoplay

challenge_ :: AttributeValue -> Attribute
challenge_ = HA.challenge

charset_ :: AttributeValue -> Attribute
charset_ = HA.charset

checked_ :: AttributeValue -> Attribute
checked_ = HA.checked

cite_' :: AttributeValue -> Attribute
cite_' = HA.cite

class_ :: AttributeValue -> Attribute
class_ = HA.class_

cols_ :: AttributeValue -> Attribute
cols_ = HA.cols

colspan_ :: AttributeValue -> Attribute
colspan_ = HA.colspan

content_ :: AttributeValue -> Attribute
content_ = HA.content

contenteditable_ :: AttributeValue -> Attribute
contenteditable_ = HA.contenteditable

contextmenu_ :: AttributeValue -> Attribute
contextmenu_ = HA.contextmenu

controls_ :: AttributeValue -> Attribute
controls_ = HA.controls

coords_ :: AttributeValue -> Attribute
coords_ = HA.coords

data_ :: AttributeValue -> Attribute
data_ = HA.data_

datetime_ :: AttributeValue -> Attribute
datetime_ = HA.datetime

defer_ :: AttributeValue -> Attribute
defer_ = HA.defer

dir_ :: AttributeValue -> Attribute
dir_ = HA.dir

disabled_ :: AttributeValue -> Attribute
disabled_ = HA.disabled

draggable_ :: AttributeValue -> Attribute
draggable_ = HA.draggable

enctype_ :: AttributeValue -> Attribute
enctype_ = HA.enctype

for_' :: AttributeValue -> Attribute
for_' = HA.for

form_' :: AttributeValue -> Attribute
form_' = HA.form

formaction_ :: AttributeValue -> Attribute
formaction_ = HA.formaction

formenctype_ :: AttributeValue -> Attribute
formenctype_ = HA.formenctype

formmethod_ :: AttributeValue -> Attribute
formmethod_ = HA.formmethod

formnovalidate_ :: AttributeValue -> Attribute
formnovalidate_ = HA.formnovalidate

formtarget_ :: AttributeValue -> Attribute
formtarget_ = HA.formtarget

headers_ :: AttributeValue -> Attribute
headers_ = HA.headers

height_ :: AttributeValue -> Attribute
height_ = HA.height

hidden_ :: AttributeValue -> Attribute
hidden_ = HA.hidden

high_ :: AttributeValue -> Attribute
high_ = HA.high

href_ :: AttributeValue -> Attribute
href_ = HA.href

hreflang_ :: AttributeValue -> Attribute
hreflang_ = HA.hreflang

httpEquiv_ :: AttributeValue -> Attribute
httpEquiv_ = HA.httpEquiv

icon_ :: AttributeValue -> Attribute
icon_ = HA.icon

id_ :: AttributeValue -> Attribute
id_ = HA.id

ismap_ :: AttributeValue -> Attribute
ismap_ = HA.ismap

item_ :: AttributeValue -> Attribute
item_ = HA.item

itemprop_ :: AttributeValue -> Attribute
itemprop_ = HA.itemprop

itemscope_ :: AttributeValue -> Attribute
itemscope_ = HA.itemscope

itemtype_ :: AttributeValue -> Attribute
itemtype_ = HA.itemtype

keytype_ :: AttributeValue -> Attribute
keytype_ = HA.keytype

label_' :: AttributeValue -> Attribute
label_' = HA.label

lang_ :: AttributeValue -> Attribute
lang_ = HA.lang

list_ :: AttributeValue -> Attribute
list_ = HA.list

loop_ :: AttributeValue -> Attribute
loop_ = HA.loop

low_ :: AttributeValue -> Attribute
low_ = HA.low

manifest_ :: AttributeValue -> Attribute
manifest_ = HA.manifest

max_ :: AttributeValue -> Attribute
max_ = HA.max

maxlength_ :: AttributeValue -> Attribute
maxlength_ = HA.maxlength

media_ :: AttributeValue -> Attribute
media_ = HA.media

method_ :: AttributeValue -> Attribute
method_ = HA.method

min_ :: AttributeValue -> Attribute
min_ = HA.min

multiple_ :: AttributeValue -> Attribute
multiple_ = HA.multiple

name_ :: AttributeValue -> Attribute
name_ = HA.name

novalidate_ :: AttributeValue -> Attribute
novalidate_ = HA.novalidate

onbeforeonload_ :: AttributeValue -> Attribute
onbeforeonload_ = HA.onbeforeonload

onbeforeprint_ :: AttributeValue -> Attribute
onbeforeprint_ = HA.onbeforeprint

onblur_ :: AttributeValue -> Attribute
onblur_ = HA.onblur

oncanplay_ :: AttributeValue -> Attribute
oncanplay_ = HA.oncanplay

oncanplaythrough_ :: AttributeValue -> Attribute
oncanplaythrough_ = HA.oncanplaythrough

onchange_ :: AttributeValue -> Attribute
onchange_ = HA.onchange

oncontextmenu_ :: AttributeValue -> Attribute
oncontextmenu_ = HA.oncontextmenu

onclick_ :: AttributeValue -> Attribute
onclick_ = HA.onclick

ondblclick_ :: AttributeValue -> Attribute
ondblclick_ = HA.ondblclick

ondrag_ :: AttributeValue -> Attribute
ondrag_ = HA.ondrag

ondragend_ :: AttributeValue -> Attribute
ondragend_ = HA.ondragend

ondragenter_ :: AttributeValue -> Attribute
ondragenter_ = HA.ondragenter

ondragleave_ :: AttributeValue -> Attribute
ondragleave_ = HA.ondragleave

ondragover_ :: AttributeValue -> Attribute
ondragover_ = HA.ondragover

ondragstart_ :: AttributeValue -> Attribute
ondragstart_ = HA.ondragstart

ondrop_ :: AttributeValue -> Attribute
ondrop_ = HA.ondrop

ondurationchange_ :: AttributeValue -> Attribute
ondurationchange_ = HA.ondurationchange

onemptied_ :: AttributeValue -> Attribute
onemptied_ = HA.onemptied

onended_ :: AttributeValue -> Attribute
onended_ = HA.onended

onerror_ :: AttributeValue -> Attribute
onerror_ = HA.onerror

onfocus_ :: AttributeValue -> Attribute
onfocus_ = HA.onfocus

onformchange_ :: AttributeValue -> Attribute
onformchange_ = HA.onformchange

onforminput_ :: AttributeValue -> Attribute
onforminput_ = HA.onforminput

onhaschange_ :: AttributeValue -> Attribute
onhaschange_ = HA.onhaschange

oninput_ :: AttributeValue -> Attribute
oninput_ = HA.oninput

oninvalid_ :: AttributeValue -> Attribute
oninvalid_ = HA.oninvalid

onkeydown_ :: AttributeValue -> Attribute
onkeydown_ = HA.onkeydown

onkeyup_ :: AttributeValue -> Attribute
onkeyup_ = HA.onkeyup

onload_ :: AttributeValue -> Attribute
onload_ = HA.onload

onloadeddata_ :: AttributeValue -> Attribute
onloadeddata_ = HA.onloadeddata

onloadedmetadata_ :: AttributeValue -> Attribute
onloadedmetadata_ = HA.onloadedmetadata

onloadstart_ :: AttributeValue -> Attribute
onloadstart_ = HA.onloadstart

onmessage_ :: AttributeValue -> Attribute
onmessage_ = HA.onmessage

onmousedown_ :: AttributeValue -> Attribute
onmousedown_ = HA.onmousedown

onmousemove_ :: AttributeValue -> Attribute
onmousemove_ = HA.onmousemove

onmouseout_ :: AttributeValue -> Attribute
onmouseout_ = HA.onmouseout

onmouseover_ :: AttributeValue -> Attribute
onmouseover_ = HA.onmouseover

onmouseup_ :: AttributeValue -> Attribute
onmouseup_ = HA.onmouseup

onmousewheel_ :: AttributeValue -> Attribute
onmousewheel_ = HA.onmousewheel

ononline_ :: AttributeValue -> Attribute
ononline_ = HA.ononline

onpagehide_ :: AttributeValue -> Attribute
onpagehide_ = HA.onpagehide

onpageshow_ :: AttributeValue -> Attribute
onpageshow_ = HA.onpageshow

onpause_ :: AttributeValue -> Attribute
onpause_ = HA.onpause

onplay_ :: AttributeValue -> Attribute
onplay_ = HA.onplay

onplaying_ :: AttributeValue -> Attribute
onplaying_ = HA.onplaying

onprogress_ :: AttributeValue -> Attribute
onprogress_ = HA.onprogress

onpropstate_ :: AttributeValue -> Attribute
onpropstate_ = HA.onpropstate

onratechange_ :: AttributeValue -> Attribute
onratechange_ = HA.onratechange

onreadystatechange_ :: AttributeValue -> Attribute
onreadystatechange_ = HA.onreadystatechange

onredo_ :: AttributeValue -> Attribute
onredo_ = HA.onredo

onresize_ :: AttributeValue -> Attribute
onresize_ = HA.onresize

onscroll_ :: AttributeValue -> Attribute
onscroll_ = HA.onscroll

onseeked_ :: AttributeValue -> Attribute
onseeked_ = HA.onseeked

onseeking_ :: AttributeValue -> Attribute
onseeking_ = HA.onseeking

onselect_ :: AttributeValue -> Attribute
onselect_ = HA.onselect

onstalled_ :: AttributeValue -> Attribute
onstalled_ = HA.onstalled

onstorage_ :: AttributeValue -> Attribute
onstorage_ = HA.onstorage

onsubmit_ :: AttributeValue -> Attribute
onsubmit_ = HA.onsubmit

onsuspend_ :: AttributeValue -> Attribute
onsuspend_ = HA.onsuspend

ontimeupdate_ :: AttributeValue -> Attribute
ontimeupdate_ = HA.ontimeupdate

onundo_ :: AttributeValue -> Attribute
onundo_ = HA.onundo

onunload_ :: AttributeValue -> Attribute
onunload_ = HA.onunload

onvolumechange_ :: AttributeValue -> Attribute
onvolumechange_ = HA.onvolumechange

onwaiting_ :: AttributeValue -> Attribute
onwaiting_ = HA.onwaiting

open_ :: AttributeValue -> Attribute
open_ = HA.open

optimum_ :: AttributeValue -> Attribute
optimum_ = HA.optimum

pattern_ :: AttributeValue -> Attribute
pattern_ = HA.pattern

ping_ :: AttributeValue -> Attribute
ping_ = HA.ping

placeholder_ :: AttributeValue -> Attribute
placeholder_ = HA.placeholder

preload_ :: AttributeValue -> Attribute
preload_ = HA.preload

pubdate_ :: AttributeValue -> Attribute
pubdate_ = HA.pubdate

radiogroup_ :: AttributeValue -> Attribute
radiogroup_ = HA.radiogroup

readonly_ :: AttributeValue -> Attribute
readonly_ = HA.readonly

rel_ :: AttributeValue -> Attribute
rel_ = HA.rel

required_ :: AttributeValue -> Attribute
required_ = HA.required

reversed_ :: AttributeValue -> Attribute
reversed_ = HA.reversed

rows_ :: AttributeValue -> Attribute
rows_ = HA.rows

rowspan_ :: AttributeValue -> Attribute
rowspan_ = HA.rowspan

sandbox_ :: AttributeValue -> Attribute
sandbox_ = HA.sandbox

scope_ :: AttributeValue -> Attribute
scope_ = HA.scope

scoped_ :: AttributeValue -> Attribute
scoped_ = HA.scoped

seamless_ :: AttributeValue -> Attribute
seamless_ = HA.seamless

selected_ :: AttributeValue -> Attribute
selected_ = HA.selected

shape_ :: AttributeValue -> Attribute
shape_ = HA.shape

size_ :: AttributeValue -> Attribute
size_ = HA.size

sizes_ :: AttributeValue -> Attribute
sizes_ = HA.sizes

span_' :: AttributeValue -> Attribute
span_' = HA.span

spellcheck_ :: AttributeValue -> Attribute
spellcheck_ = HA.spellcheck

src_ :: AttributeValue -> Attribute
src_ = HA.src

srcdoc_ :: AttributeValue -> Attribute
srcdoc_ = HA.srcdoc

start_ :: AttributeValue -> Attribute
start_ = HA.start

step_ :: AttributeValue -> Attribute
step_ = HA.step

style_' :: AttributeValue -> Attribute
style_' = HA.style

subject_ :: AttributeValue -> Attribute
subject_ = HA.subject

summary_' :: AttributeValue -> Attribute
summary_' = HA.summary

tabindex_ :: AttributeValue -> Attribute
tabindex_ = HA.tabindex

target_ :: AttributeValue -> Attribute
target_ = HA.target

title_' :: AttributeValue -> Attribute
title_' = HA.title

type_ :: AttributeValue -> Attribute
type_ = HA.type_

usemap_ :: AttributeValue -> Attribute
usemap_ = HA.usemap

value_ :: AttributeValue -> Attribute
value_ = HA.value

width_ :: AttributeValue -> Attribute
width_ = HA.width

wrap_ :: AttributeValue -> Attribute
wrap_ = HA.wrap

xmlns_ :: AttributeValue -> Attribute
xmlns_ = HA.xmlns

liftParent :: (Monad m, Foldable t) => (Html -> Html) -> t Attribute -> WidgetT site m a -> WidgetT site m a
liftParent el attrs (WidgetT f) = WidgetT $ \hdata -> do
  (a,gwd) <- f hdata
  let Body bodyFunc = gwdBody gwd
      combinedAttrs = fold attrs
      newBodyFunc render =
        el H.! combinedAttrs $ (bodyFunc render)
  return (a,gwd { gwdBody = Body newBodyFunc })

liftLeaf :: (Monad m, Foldable t) => Html -> t Attribute -> WidgetT site m ()
liftLeaf el attrs = WidgetT $ const
  (return ((),mempty { gwdBody = Body (const (el H.! (fold attrs)))}))

