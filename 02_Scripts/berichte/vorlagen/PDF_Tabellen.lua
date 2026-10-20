-- Feste Spaltenbreiten erlauben Umbrüche in breiten Markdown-Tabellen.
function Table(t)
  local widths = {}
  for i = 1, #t.colspecs do widths[i] = 6 end
  local function measure(rows)
    for _, row in ipairs(rows) do
      for i, cell in ipairs(row.cells) do
        widths[i] = math.max(widths[i], math.min(40, #pandoc.utils.stringify(cell)))
      end
    end
  end
  measure(t.head.rows)
  for _, body in ipairs(t.bodies) do measure(body.body) end
  local total = 0
  for _, width in ipairs(widths) do total = total + width end
  for i, spec in ipairs(t.colspecs) do spec[2] = widths[i] / total end
  for _, row in ipairs(t.head.rows) do
    for _, cell in ipairs(row.cells) do
      for _, block in ipairs(cell.contents) do
        if block.t == "Plain" or block.t == "Para" then
          block.content = pandoc.Inlines({pandoc.Strong(block.content)})
          if FORMAT:match("latex") then
            block.content:insert(1, pandoc.RawInline("latex", "\\cellcolor{ReportTint}"))
          end
        end
      end
    end
  end
  return t
end
