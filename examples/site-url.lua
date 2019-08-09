-- Replaces relative URLs with absolute
-- e.g. "/about" -> "https://www.example.com/about"

-- Configuration
site_url = "https://www.example.com"

-- Plugin code

if not Regex.match(site_url, "(.*)/$") then
  site_url = site_url .. "/"
end

links = HTML.select(page, "a")

-- That's Lua 2.5, hand-cranked iteration...
index, link = next(links)

while index do
  href = HTML.get_attribute(link, "href")
  if href then
    -- Check if URL schema is present
    if not Regex.match(href, "^([a-zA-Z0-9]+):") then
      -- Remove leading slashes
      href = Regex.replace(href, "^/*", "")
      href = site_url .. href
      HTML.set_attribute(link, "href", href)
    end
  end
  index, link = next(links, index)
end

