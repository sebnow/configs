local snap = require('snap')

-- TODO: The fzy rock doesn't seem to be found, so consumer.fzy doesn't
-- work
local fuzzy = snap.get('consumer.fzf')

local M = {}

M.find_files = snap.create(function ()
    local rg = snap.get('producer.ripgrep.file')
    local select = snap.get('select.file')

    return {
        producer = fuzzy(rg),
        select = select.select,
        multiselect = select.multiselect,
        views = {snap.get('preview.file')},
    }
end)

M.grep = snap.create(function ()
    local select = snap.get('select.vimgrep')

    return {
        producer = snap.get('producer.ripgrep.vimgrep'),
        select = select.select,
        multiselect = select.multiselect,
        views = {snap.get('preview.vimgrep')}
    }
end)

M.buffers = snap.create(function ()
    local select = snap.get('select.vimgrep')

    return {
        producer = fuzzy(snap.get'producer.vim.buffer'),
        select = snap.get('select.file').select,
        multiselect = snap.get('select.file').multiselect,
        views = {snap.get('preview.file')}
    }
end)

return M
