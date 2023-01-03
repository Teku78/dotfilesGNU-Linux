-- General settings

local set = vim.opt -- set options

-- tabs & indentation
set.tabstop = 4
set.softtabstop = 4
set.shiftwidth = 4
set.autoindent = true

set.cursorline = true
set.number = true

set.mouse = 'a'

-- search
set.ignorecase = true
set.hlsearch = true
set.smartcase = true

-- appearance
set.syntax = "on"
set.termguicolors = true

-- clipboard
set.clipboard:append("unnamedplus")
