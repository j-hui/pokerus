local compilers = { "gcc", "g++", "clang" }
local locations = {
  "/nix/store",
  "/usr/bin",
  "/usr/local/bin",
  "/opt/homebrew/bin",
}

local driver_paths = {}
for _, c in ipairs(compilers) do
  for _, l in ipairs(locations) do
    table.insert(driver_paths, l .. "/*" .. c .. "*")
  end
end

-- return false and {
return {
  -- TODO: Get rid of this awful config.
  -- Tracking: https://github.com/clangd/clangd/issues/539
  opts = {
    cmd = { "clangd", "--query-driver=" .. table.concat(driver_paths, ",") }
  } 
}
