--[[
  landscape – set page to landscape
]]

local function begin_landscape(format)
  if format == 'docx' then
    local pagebreak = '<w:p><w:pPr><w:sectPr><w:r><w:br w:type=\"page\"/></w:r><w:pgSz w:orient=\"landscape\" w:w=\"11906\" w:h=\"16838\"/></w:sectPr></w:pPr></w:p>'
    return pandoc.RawBlock('openxml', pagebreak)
  elseif format == 'latex' then
    return pandoc.RawBlock('latex', '\\clearpage\\begin{landscape}')
  else
    return pandoc.Para{pandoc.Str '\f'}
  end
end

local function end_landscape(format)
  if format == 'docx' then
    local pagebreak = '<w:p><w:pPr><w:sectPr><w:r><w:br w:type=\"page\"/></w:r><w:pgSz w:orient=\"portrait\" w:w=\"16838\" w:h=\"11906\"/></w:sectPr></w:pPr></w:p>'
    return pandoc.RawBlock('openxml', pagebreak)
  elseif format == 'latex' then
    return pandoc.RawBlock('latex', '\\end{landscape}\\clearpage')
  else
    return pandoc.Para{pandoc.Str '\f'}
  end
end

function Div (el)
  if el.classes:includes('landscape') then
    local blocks = {}
    table.insert(blocks, begin_landscape(FORMAT))
    for _, v in ipairs(el.content) do
      table.insert(blocks, v)
    end
    table.insert(blocks, end_landscape(FORMAT))
    return blocks
  end
end

