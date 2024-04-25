
--[[
neps-paper - styles for NEPS survey and working papers

Copyright (c) 2023 Timo Gnambs

Permission to use, copy, modify, and/or distribute this software for any purpose
with or without fee is hereby granted, provided that the above copyright notice
and this permission notice appear in all copies.

THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES WITH
REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND
FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT,
INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS
OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER
TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF
THIS SOFTWARE.
]]

local stringify = pandoc.utils.stringify
local List = require 'pandoc.List'

local docxPagebreak = function()
    return pandoc.RawBlock('openxml', '<w:p><w:r><w:br w:type="page"/></w:r></w:p>')
end

local docxBlock = function(args)
    if args.txt == nil then return "" end
    local bold = args.bold or false
    local size = args.size or 0
    local align = args.align or "both"
    local style = args.style or nil
    local formats = "<w:rPr>"
    if bold then formats = formats .. "<w:b/>" end
    if size > 0 then formats = formats .. "<w:sz w:val='" .. size .. "'/>" end
    formats = formats .. "<w:jc w:val = '" .. align .. "'/>"
    formats = formats .. "</w:rPr>"
    local txt = '<w:r>' .. formats .. "<w:t>" .. args.txt .. "</w:t></w:r>"
    if style ~= nil then
        txt = '<w:pPr><w:pStyle w:val="' .. style .. '"/></w:pPr>' .. txt
    end
    txt = '<w:p>' .. txt .. '</w:p>'
    return pandoc.RawBlock('openxml', txt)
end


Pandoc = function(doc)

    local meta = doc.meta
    local body = doc.blocks
    local tp = List:new{}

    -- For unnumbered headings, we set a new level that has a style without numbers
    -- Thus, the document only considers 4 levels of headings.
    body = body:walk{
        Header = function(h)
            if h.level >= 1 and h.level <= 4 and (h.classes):includes("unnumbered") then
                h.level = h.level + 4
            end
            return h
        end
    }

    -- Set meta information to be used in fields for header and footer
    meta.ReportYear = stringify(meta.bibdata.year)
    meta.ReportNumber = stringify(meta.bibdata.number)
    meta.ReportSeries = stringify(meta.bibdata.series)
    meta.ReportAuthors = stringify(meta.authorsforhead)
    meta.authorsforhead = nil

    -- Page 1: authors
    if (meta.authors ~= nil and #meta.authors > 0) then
        local ca, orcid = List:new(), ""
        ca:extend(List:new{pandoc.RawInline('openxml', '<w:pPr><w:pStyle w:val="Author"/></w:pPr><w:r><w:t>')})
        if meta.authors ~= nil and #meta.authors > 0 then
            ca:extend(List:new{pandoc.LineBreak()})
            for _, author in ipairs(meta.authors) do
                ca:extend(List:new{pandoc.Emph(author.name.literal)})
                if author.affiliationid ~= nil then
                    for _, affil in ipairs(author.affiliationid) do
                        ca:extend(List:new{affil})
                    end
                end
                if author.orcid ~= nil then
                    ca:extend(List:new{pandoc.Space()})
                    orcid = "https://orcid.org/" .. stringify(author.orcid)
                    ca:extend(List:new{pandoc.Link(pandoc.RawInline('openxml', '<w:r><w:rPr><w:i w:val="0"/><w:color w:val="92D050"/></w:rPr><w:sym w:font="Academicons" w:char="E9D9"/></w:r>'), orcid)})
                end
                ca:extend(List:new{pandoc.Str(author.authorsep), pandoc.Space()})
            end
            ca:extend(List:new{pandoc.RawInline('openxml', '</w:t></w:r>')})
            tp:extend(List:new{pandoc.Para(ca)})
            meta.author = nil
        end
        if meta.affiliations ~= nil and #meta.affiliations > 0 then
            ca = List:new{pandoc.RawInline('openxml', '<w:pPr><w:pStyle w:val="Author"/></w:pPr><w:r><w:t>')}
            for _, affil in ipairs(meta.affiliations) do
                ca:extend(List:new{pandoc.Superscript(affil.affiliationid), pandoc.Space(), pandoc.Str(stringify(affil.name))})
                ca:extend(List:new{pandoc.LineBreak()})
            end
            ca:extend(List:new{pandoc.RawInline('openxml', '</w:t></w:r>')})
            tp:extend(List:new{pandoc.Para(ca)})
            meta.affiliations = nil
        end
    end

    -- Page 1: email
    for i = 1, 6 do tp:extend(List:new{pandoc.Para(pandoc.Str "")}) end
    if (meta.email ~= nil) then
        tp:extend(List:new{pandoc.Para(pandoc.Strong("E‐mail address of the lead author:"))})
        tp:extend(List:new{pandoc.Para(stringify(meta.email))})
        meta.email = nil
    end

    -- Page 1: bibliography
    tp:extend(List:new{pandoc.Para(pandoc.Strong("Bibliographic data:"))})
    ca = List:new{pandoc.Str(stringify(meta.bibdata.authors)), pandoc.Space(),
                  pandoc.Str("(" .. stringify(meta.bibdata.year) .. ")."), pandoc.Space(),
                  pandoc.Emph(stringify(meta.bibdata.title)), pandoc.Space(),
                  pandoc.Str("(" .. stringify(meta.bibdata.series)), pandoc.Space(),
                  pandoc.Str("No."), pandoc.Space(),
                  pandoc.Str(stringify(meta.bibdata.number) .. ")."),
                  pandoc.Space(),
                  pandoc.Str(stringify(meta.bibdata.institution) .. ".")}
    if meta.bibdata.doi ~= nil then
        --ca:extend(List:new{pandoc.Space(), pandoc.Str("doi:" .. stringify(meta.bibdata.doi))})
        ca:extend(List:new{pandoc.Space(), pandoc.Link(stringify(meta.bibdata.doi), stringify(meta.bibdata.doi))})
    end
    if meta.bibdata.url ~= nil then
        ca:extend(List:new{pandoc.Space(), pandoc.Link(stringify(meta.bibdata.url), stringify(meta.bibdata.url))})
    end
    tp:extend(List:new{pandoc.Para(ca)})
    meta.bibdata = nil

    -- Page 1: acknowledgments
    if meta.acknowledgments ~= nil then
        for i = 1, 6 do tp:extend(List:new{pandoc.Para(pandoc.Str "")}) end
        local ack = List:new{pandoc.Strong("Acknowledgements"), pandoc.Str ".", pandoc.Space()}
        for _, v in ipairs(meta.acknowledgments[1].content) do ack:extend(List:new{v}) end
        tp:extend(List:new{pandoc.Para(ack)})
        if #meta.acknowledgments > 1 then
            for i = 2, #meta.acknowledgments do tp:extend(List:new{meta.acknowledgments[i]}) end
        end
        meta.acknowledgments = nil
    end

    -- Page 1: end of page
    tp:extend(List:new{docxPagebreak()})

    -- Page 2: title
    tp:extend(List:new{docxBlock{txt = stringify(meta.title), style = "Untertitel"}})

    -- Page 2: abstract
    tp:extend(List:new{pandoc.Para(pandoc.Strong("Abstract"))})
    tp:extend(meta.abstract)
    meta.abstract = nil

    -- Page 2: keywords
    if (meta.keywords ~= nil) then
        tp:extend(List:new{pandoc.Para(pandoc.Str "")})
        tp:extend(List:new{pandoc.Para(pandoc.Strong("Keywords"))})
        tp:extend(List:new{pandoc.Para(meta.keywords)})
        meta.keywords = nil
    end

    -- Page 2: end of page
    tp:extend(List:new{docxPagebreak()})

    -- Page 3: toc
    tp:extend(List:new{pandoc.Para(pandoc.Strong(stringify(meta["toc-title"])))})
    tp:extend(List:new{pandoc.Para(pandoc.Str "")})
    local thetoc = ''
    thetoc = thetoc .. '<w:sdt><w:sdtPr><w:docPartObj><w:docPartGallery w:val="Table of Contents"/><w:docPartUnique/></w:docPartObj></w:sdtPr><w:sdtContent>'
    thetoc = thetoc .. '<w:p><w:r><w:fldChar w:fldCharType="begin" w:dirty="true" />'
    thetoc = thetoc .. "<w:instrText xml:space='preserve'>TOC \\o '1-8' \\h \\z \\u</w:instrText>"
    thetoc = thetoc .. '<w:fldChar w:fldCharType="separate"/>'
    thetoc = thetoc .. '<w:fldChar w:fldCharType="end"/></w:r></w:p></w:sdtContent></w:sdt>'
    tp:extend(List:new{pandoc.Para(pandoc.RawInline('openxml', thetoc))})
    tp:extend(List:new{pandoc.Para(pandoc.Str "")})

    -- Page 3: end of page
    tp:extend(List:new{docxPagebreak()})

    return pandoc.Pandoc(tp .. body, meta)

  end

