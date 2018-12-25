module NicLib.HTML

-- !! do something special for <del>, <s>, and <strike>; these are "anti-text"
-- do something special for tables too
-- nonRenderTags and breakingTags consider only descendents of <html> (<head>'s descendentsaren't considered)

-- neither these elements nor descendents thereof are renderable
nonRenderTags :: S.Set T'.Text
nonRenderTags = S.fromList ["area", "audio", "button", "canvas", "colgroup", "embed", "img", "map", "meta", "meter", "noscript", "object", "param", "picture", "progress", "script", "source", "svg", "textarea", "track", "video"]

-- essentially text elements that display block. mutually exclusive with nonRenderTags
breakingTags :: S.Set T'.Text
breakingTags = S.fromList ["address", "blockquote", "br", "caption", "cite", "code", "dd", "details", "dialog", "div", "dl", "footer", "form", "h1", "h2", "h3", "h4", "h5", "h6", "header", "li", "menu", "menuitem", "ol", "option", "p", "pre", "q", "samp", "section", "select", "summary", "table", "thead", "tfoot", "tr", "ul"]
