local List = require 'pandoc.List'

local function file_exists(name)
  local f = io.open(name, 'r')
  if f ~= nil then
    io.close(f)
    return true
  else
    return false
  end
end

local function unescape_urls(bib)
  return (string.gsub(bib, "(\n%s*url%s*=%s*{[^\n]*},\n)",
    function(line) return string.gsub(line, "\\", "") end))
end

local idmap = {}

function Meta(meta)
  local bibs = meta['bibliography']
  if not bibs then bibs = List:new{} end
  local citekeys = meta['citekeys']
  for k,v in pairs(citekeys) do
    local v = table.unpack(v).text
    local d = string.match(v, "DBLP:(.*)")
    if d then
      idmap[k] = v
      local fname = "dblp/" .. d .. ".bib"
      bibs[#bibs + 1] = fname

      if not file_exists(fname) then
        print("Fetching " .. d)
        local _,bib = pandoc.mediabag.fetch("https://dblp.org/rec/bib2/" .. d)
        os.execute("mkdir -p " .. fname:match(".*/"))
        local f = io.open(fname, 'w')
        f:write(unescape_urls(bib))
        f:close()
      end
    end
  end
  meta['bibliography'] = bibs

  local incl = meta['include-after']
  if not incl then incl = List:new{} end
  local html = io.open("dblp-cite.html", 'r')
  incl[#incl + 1] = pandoc.RawBlock("html", html:read('*a'))
  html:close()
  meta['include-after'] = incl
  return meta
end

function Cite(cite)
  cite.citations = cite.citations:map(function(cit)
    local id = idmap[cit.id]
    if id then cit.id = id end
    return cit
  end)
  return cite
end

-- force Meta to run before Cite
return {{Meta = Meta}, {Cite = Cite}}
