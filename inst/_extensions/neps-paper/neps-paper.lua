
--[[
neps-paper - set meta information for NEPS survey and working papers

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

local List = require 'pandoc.List'
local stringify = pandoc.utils.stringify

-- taken from https://github.com/pandoc/lua-filters/blob/1660794b991c3553968beb993f5aabb99b317584/author-info-blocks/author-info-blocks.lua
local intercalate = function (lists, elem)
    local result = List:new{}
    for i = 1, (#lists - 1) do
      result:extend(lists[i])
      result:extend(elem)
    end
    if #lists > 0 then
      result:extend(lists[#lists])
    end
    return result
  end

-- Formats author following APA
-- @param table   entry in meta.authors
-- @returns       string with formatted author name
local getBibauthor = function(author)
    if author.name == nil then return "" end
    if author.name.family == nil then return "" end
    local bibauthor = stringify(author.name.family)
    if author.name.given == nil then return bibauthor end
    bibauthor = bibauthor .. ","
    for str in string.gmatch(stringify(author.name.given), "%S+") do
        bibauthor = bibauthor .. " " .. string.sub(str, 1, 1) .. "."
    end
    return bibauthor
end

-- Converts the first character of a title string to uppercase
-- @param str   a string
-- @param len   minimum length of strings to consider
-- @returns
local firstToUpper = function(str, len)
    len = len or 4
    if str == nil or #str < len then return str end
    return str:gsub("^%l", string.upper)
end

-- Convert the first characters of a title string to lowercase
-- @param str   a string
-- @returns
local firstToLower = function(str)
    local excl = {"NEPS", "LIfBi", "SUF"}
    if str == nil or #str <= 1 then return str end
    for _, v in ipairs(excl) do
       if str == v then return str end
    end
    return str:gsub("^%u", string.lower)
end

Pandoc = function(doc)

    local meta = doc.meta
    local body = doc.blocks

    ----------------
    -- Title page --
    ----------------

    -- Main title
    if meta.title == nil then meta.title = pandoc.MetaInlines({}) end
    meta.title = meta.title:walk {
        Str = function (s) return firstToUpper(s.text) end
    }

    -- Authors and affiliation for title page
    local affiliationid = {}
    if meta.authors == nil then meta.authors = pandoc.MetaMap({}) end
    if meta.affiliations == nil then meta.affiliations = pandoc.MetaMap({}) end
    for i, author in ipairs(meta.authors) do
        author.affiliationid, author.authorsep = "", ""
        if #(meta.affiliations) > 1 and author.affiliations ~= nil then
            affiliationid = {}
            for _, v in ipairs(author.affiliations) do
                table.insert(affiliationid, stringify(v.ref):match('%d$'))
            end
            affiliationid = table.concat(affiliationid, ",")
            author.affiliationid = pandoc.Superscript(affiliationid)
        end
        if #(meta.authors) > 2 and i < #(meta.authors) then
            author.authorsep = ","
        end
        if #(meta.authors) > 1 and i + 1 == #(meta.authors) then
            author.authorsep = author.authorsep .. " and"
        end
    end
    for _, v in ipairs(meta.affiliations) do
        v.affiliationid = ""
        if #(meta.affiliations) > 1 then
            v.affiliationid = pandoc.Superscript(stringify(v.id):match('%d$'))
        end
    end

    -- Author for head
    if meta.authorsforhead == nil then
        local authorsforhead, nauthors = "", #(meta.authors)
        if nauthors == 0 then
            authorsforhead = "LIfBi"
        elseif nauthors == 1 then
            authorsforhead = stringify(meta.authors[1].name.family)
        else
            for i, author in ipairs(meta.authors) do
                if i == nauthors then
                    authorsforhead = authorsforhead .. "& "
                end
                authorsforhead = authorsforhead .. stringify(author.name.family)
                if i < nauthors and nauthors > 2 then
                    authorsforhead = authorsforhead .. ", "
                end
            end
        end
        meta.authorsforhead = List:new{pandoc.Str(authorsforhead)}
    end

    -- Email of corresponding author
    if meta.email == nil then
        local email = List:new{}
        for _, v in ipairs(meta.authors) do
            if v.email ~= nil then
                email:extend(List:new{v.email})
            end
        end
        if (#email > 0) then
            meta.email = intercalate(email, {pandoc.Str ", "})
        end
    end

    ------------------------
    -- Bibliographic data --
    ------------------------

    if meta.bibdata == nil then meta.bibdata = pandoc.MetaMap({}) end

    -- Set title
    if (meta.bibdata.title == nil) then
        meta.bibdata.title = meta.title:walk {
            Str = function (s) return firstToLower(s.text) end
          }
    end

    -- Set publication year

    if meta.bibdata.year == nil then
        meta.bibdata.year = List:new{pandoc.Str(os.date("%Y"))}
    end

    -- Publication series

    if meta.bibdata.series == nil then
        meta.bibdata.series = List:new{pandoc.Str "NEPS Survey Paper"}
    end

    -- Series number

    if meta.bibdata.number == nil then
        meta.bibdata.number = List:new{pandoc.Str '0'}
    end

    -- DOI
    if meta.bibdata.doi ~= nil then
        meta.bibdata.doi = List:new{pandoc.Str("https://doi.org/" .. stringify(meta.bibdata.doi))}
    end

    -- Institution
    if meta.bibdata.institution == nil then
        meta.bibdata.institution = List:new{pandoc.Str "Leibniz Institute for Educational Trajectories"}
    end

    -- Set authors for bibliographic data

    if meta.bibdata.authors == nil then
        local bibauthors, nauthors = List:new{}, #(meta.authors)
        if nauthors == 0 then
            bibauthors:extend(List:new{pandoc.Str "LIfBi"})
        elseif nauthors == 1 then
            bibauthors:extend(List:new{pandoc.Str(getBibauthor(meta.authors[1]))})
        else
            for i, author in ipairs(meta.authors) do
                if i == nauthors then
                    bibauthors:extend(List:new{pandoc.Str '& '})
                end
                bibauthors:extend(List:new{pandoc.Str(getBibauthor(author))})
                if i < nauthors then
                    bibauthors:extend(List:new{pandoc.Str ', '})
                end
            end
        end
        meta.bibdata["authors"] = bibauthors
    end

    return pandoc.Pandoc(body, meta)

  end

