local snap = require('snap')
local wk = require("which-key")

-- TODO: The fzy rock doesn't seem to be found, so consumer.fzy doesn't
-- work
local fuzzy = snap.get('consumer.fzf')

local find_files = snap.create(function ()
    local rg = snap.get('producer.ripgrep.file')
    local select = snap.get('select.file')

    return {
        producer = fuzzy(rg),
        select = select.select,
        multiselect = select.multiselect,
        views = {snap.get('preview.file')},
    }
end)

local grep = snap.create(function ()
    local select = snap.get('select.vimgrep')

    return {
        producer = snap.get('producer.ripgrep.vimgrep'),
        select = select.select,
        multiselect = select.multiselect,
        views = {snap.get('preview.vimgrep')}
    }
end)

local buffers = snap.create(function ()
    local select = snap.get('select.vimgrep')

    return {
        producer = fuzzy(snap.get'producer.vim.buffer'),
        select = snap.get('select.file').select,
        multiselect = snap.get('select.file').multiselect,
        views = {snap.get('preview.file')}
    }
end)

wk.register({
    ['<C-p>'] = {find_files, 'Explore files'},
})

wk.register({
    b = {
        name = 'Buffers',
        e = {buffers, 'Explore buffers' },
    },
    p = {
        name = 'Project',
        ['/'] = {grep, 'Search in project'},
    },
}, {prefix = '<localleader>'})
