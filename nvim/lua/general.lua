-- General settings

local set = vim.opt -- set options

-- tabs & indentation
set.tabstop = 4
set.softtabstop = 4
set.shiftwidth = 4
set.autoindent = true

set.smartcase = true
set.smartindent = true

set.cursorline = true
set.number = true

set.showmode = false

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

-- 
set.undofile = true
vim.g.smoothie_update_interval=1

vim.api.nvim_create_autocmd("BufReadPost", {
	callback = function()
		local mark = vim.api.nvim_buf_get_mark(0, '"')
		local lcount = vim.api.nvim_buf_line_count(0)
		if mark[1] > 0 and mark[1] <= lcount then
			pcall(vim.api.nvim_win_set_cursor, 0, mark)
		end
	end,
})
